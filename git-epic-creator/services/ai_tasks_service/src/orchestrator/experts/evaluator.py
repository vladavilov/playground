"""Evaluator expert - computes quality scores for generated backlog."""

from typing import Dict
from pydantic import BaseModel, Field
import structlog

from task_models.agent_models import BacklogDraft, AuditFindings, EvaluationReport
from config import get_ai_tasks_settings
from utils.llm_client_factory import create_llm
from orchestrator.prompts import EVALUATOR, build_chat_prompt
from utils.deepeval_utils import evaluate_with_metrics, StrictGEval
from utils.chunk_utils import truncate_chunk_text
from utils.token_utils import count_tokens
from deepeval.test_case import LLMTestCase, LLMTestCaseParams
from deepeval.metrics import FaithfulnessMetric, AnswerRelevancyMetric

logger = structlog.get_logger(__name__)


class Evaluator:
    """Expert that evaluates backlog quality against rubric axes."""

    def __init__(self) -> None:
        settings = get_ai_tasks_settings()
        self.weights = settings.EVAL_WEIGHTS

    async def evaluate(
        self,
        draft: BacklogDraft,
        findings: AuditFindings,
        requirements: str,
    ) -> EvaluationReport:
        """Evaluate backlog quality and compute score using DeepEval.
        
        Args:
            draft: Generated backlog draft
            findings: Audit findings
            requirements: Original requirements text
            
        Returns:
            EvaluationReport with overall score, component scores, and gaps
        """
        class EvalOut(BaseModel):
            rationale: str = Field(..., description="Brief scoring explanation")
            gaps: list[str] = Field(default_factory=list, description="Top 3 identified gaps")

        # Build evaluation prompt for LLM rationale and gaps
        backlog_summary = self._summarize_for_eval(draft, findings)
        
        prompt_tmpl = build_chat_prompt(EVALUATOR)
        
        llm = create_llm(use_fast_model=True)
        chain = prompt_tmpl | llm.with_structured_output(EvalOut)
        out: EvalOut = await chain.ainvoke({
            "requirements": requirements,
            "backlog_summary": backlog_summary,
        })
        
        # Compute component scores using DeepEval
        logger.info(
            "deepeval_backlog_evaluation_starting",
            epics_count=len(draft.epics),
            total_tasks=sum(len(e.tasks) for e in draft.epics),
            requirements_length=len(requirements),
        )
        
        component_scores = await self._evaluate_with_deepeval(draft, requirements, findings)
        
        # Compute weighted overall score
        overall_score = sum(
            component_scores.get(k, 0.0) * self.weights.get(k, 0.0)
            for k in component_scores
        )
        
        logger.info(
            "deepeval_backlog_evaluation_completed",
            overall_score=overall_score,
            component_scores=component_scores,
        )
        
        return EvaluationReport(
            score=overall_score,
            component_scores=component_scores,
            rationale=out.rationale,
            gaps=out.gaps,
        )

    async def _evaluate_with_deepeval(
        self, 
        draft: BacklogDraft, 
        requirements: str,
        findings: AuditFindings,
    ) -> Dict[str, float]:
        """Evaluate backlog using DeepEval metrics.
        
        Args:
            draft: Generated backlog draft
            requirements: Original requirements text
            findings: Audit findings for context
            
        Returns:
            Dict with component scores (coverage, specificity, feasibility, duplication)
        """
        settings = get_ai_tasks_settings()
        
        # Build backlog text representation with trimming (token optimization)
        parts: list[str] = []
        for epic in draft.epics[:settings.MAX_EPICS_FOR_EVAL]:  # Trim epics
            epic_desc = truncate_chunk_text(epic.description, 2000)
            parts.append(f"Epic {epic.id}: {epic.title}. {epic_desc}")
            
            for task in epic.tasks[:settings.MAX_TASKS_PER_EPIC_EVAL]:  # Trim tasks per epic
                task_desc = truncate_chunk_text(task.description, 1500)
                # Trim acceptance criteria
                ac_text = "; ".join(task.acceptance_criteria[:settings.MAX_AC_PER_TASK_EVAL])
                # Trim dependencies to first 3
                deps_text = f" (depends on: {', '.join(task.dependencies[:3])})" if task.dependencies else ""
                parts.append(f"  Task {task.id}: {task.title}. {task_desc}. ACs: {ac_text}{deps_text}")
        
        # Add indicator if content was trimmed
        if len(draft.epics) > settings.MAX_EPICS_FOR_EVAL:
            parts.append(f"... and {len(draft.epics) - settings.MAX_EPICS_FOR_EVAL} more epics (trimmed for evaluation)")
        
        if draft.assumptions:
            parts.append("Assumptions: " + "; ".join(draft.assumptions[:3]))  # Limit to 3
        if draft.risks:
            parts.append("Risks: " + "; ".join(draft.risks[:3]))  # Limit to 3
        
        backlog_text = "\n".join(p for p in parts if p) or "Empty backlog"
        
        # Monitor token usage for backlog serialization
        backlog_tokens = count_tokens(backlog_text, model_name="gpt-4o")
        logger.info(
            "backlog_serialization_metrics",
            char_length=len(backlog_text),
            token_count=backlog_tokens,
            epics_included=min(len(draft.epics), settings.MAX_EPICS_FOR_EVAL),
            total_epics=len(draft.epics),
        )
        
        # Build context from findings
        contexts = []
        if findings.issues:
            contexts.append("Issues identified: " + "; ".join(findings.issues[:5]))
        if findings.suggestions:
            contexts.append("Suggestions: " + "; ".join(findings.suggestions[:5]))
        
        # Guard against empty context
        if not contexts:
            logger.warning(
                "evaluation_context_minimal",
                message="No audit issues or suggestions found. Using minimal context placeholder."
            )
            contexts.append("No audit issues found")
        
        # Build smaller retrieval context (only issues, no suggestions) to avoid duplication
        retrieval_contexts = []
        if findings.issues:
            # Limit to 3 issues for retrieval context (reduce token usage)
            retrieval_contexts.append("Issues: " + "; ".join(findings.issues[:3]))
        
        if not retrieval_contexts:
            retrieval_contexts.append("No issues found")
        
        logger.info(
            "deepeval_test_case_building",
            backlog_text_length=len(backlog_text),
            contexts_count=len(contexts),
            retrieval_contexts_count=len(retrieval_contexts),
            requirements_length=len(requirements),
        )
        
        try:
            # Build test case with optimized context (avoid duplication)
            test_case = LLMTestCase(
                input=requirements[:500],  # Trim requirements for faster processing
                actual_output=backlog_text,
                retrieval_context=retrieval_contexts,  # Smaller, focused context
                context=contexts,  # Full context for evaluation
            )
            logger.debug("deepeval_test_case_created", backlog_text_length=len(backlog_text))
            
            # Configure metrics (configuration loaded automatically from shared LLM config)
            metrics_config = {
                "coverage": {
                    "class": FaithfulnessMetric,
                    "kwargs": {},
                },
                "specificity": {
                    "class": StrictGEval,
                    "kwargs": {
                        "name": "Technical Specificity",
                        "criteria": (
                            "Epics and tasks must be technically specific, implementable, and testable. "
                            "Each work item should include concrete technical details (APIs, endpoints, data models, services, infrastructure), "
                            "clear implementation approach, and well-formed Given/When/Then acceptance criteria with specific values."
                        ),
                        "evaluation_steps": [
                            "Parse all epics and tasks from the backlog output",
                            "For each epic, check if description includes: technical approach, services/components involved, integration points, and architectural details",
                            "For each task, verify presence of: specific API endpoints, data model details, configuration items, or infrastructure components",
                            "Evaluate acceptance criteria format: must follow 'Given [specific precondition] When [action with parameters] Then [measurable outcome with values]'",
                            "Check if acceptance criteria include specific values (status codes, field names, thresholds) rather than vague statements",
                            "Assess if tasks specify HOW to implement (frameworks, libraries, patterns) not just WHAT business need",
                            "Count tasks with vague descriptions (e.g., 'user-friendly', 'improve', 'enhance' without specifics)",
                            "Score based on: (tasks with concrete technical details / total tasks) weighted with (criteria following Given/When/Then / total criteria)"
                        ],
                        "evaluation_params": [LLMTestCaseParams.ACTUAL_OUTPUT, LLMTestCaseParams.CONTEXT],
                        "strict_mode": False,
                        "verbose_mode": False,
                    },
                },
                "feasibility": {
                    "class": StrictGEval,
                    "kwargs": {
                        "name": "Technical Feasibility",
                        "criteria": (
                            "Tasks must be technically feasible within project constraints. "
                            "Check for: realistic scope, clear implementation path, available technology stack."
                        ),
                        "evaluation_steps": [
                            "Review task descriptions for technical feasibility markers",
                            "Check if tasks reference existing services/APIs from context",
                            "Assess if acceptance criteria are achievable with known tech stack",
                            "Count tasks with unclear implementation vs clear technical approach",
                            "Score: (feasible_tasks / total_tasks)"
                        ],
                        "evaluation_params": [LLMTestCaseParams.ACTUAL_OUTPUT, LLMTestCaseParams.CONTEXT],
                        "strict_mode": False,
                        "verbose_mode": False,
                    },
                },
                "duplication": {
                    "class": StrictGEval,
                    "kwargs": {
                        "name": "Uniqueness",
                        "criteria": (
                            "Backlog should minimize duplication and overlap. Each epic and task should address a distinct capability "
                            "without repeating work covered by other items. Tasks should not implement the same API endpoint, modify the "
                            "same data model, or duplicate acceptance criteria found elsewhere in the backlog."
                        ),
                        "evaluation_steps": [
                            "Extract all epic and task titles and descriptions from the backlog",
                            "Compare epic pairs for semantic similarity in scope (e.g., both implementing authentication features)",
                            "Compare task pairs for overlapping technical work (e.g., both creating the same API endpoint or database table)",
                            "Identify acceptance criteria that appear in multiple tasks with minimal variation",
                            "Check audit findings context for mentions of duplicate or overlapping work",
                            "Assess if similar work could be consolidated into single tasks without loss of clarity",
                            "Calculate duplication score: 1.0 - (number of identified overlaps / total work items)",
                            "Apply penalties for: exact duplicate technical components (0.3), similar functionality (0.2), redundant acceptance criteria (0.1)"
                        ],
                        "evaluation_params": [LLMTestCaseParams.ACTUAL_OUTPUT, LLMTestCaseParams.CONTEXT],
                        "strict_mode": False,
                        "verbose_mode": False,
                    },
                },
            }
            
            # Execute metrics (telemetry disabled, model cached, parallel execution, 30s timeout)
            component_scores = await evaluate_with_metrics(test_case, metrics_config)
            
            logger.info(
                "deepeval_evaluation_completed_successfully",
                coverage=component_scores.get("coverage", 0.0),
                specificity=component_scores.get("specificity", 0.0),
                feasibility=component_scores.get("feasibility", 0.0),
                duplication=component_scores.get("duplication", 0.0),
            )
            
        except ImportError as e:
            logger.error(
                "deepeval_import_failed",
                error=str(e),
                error_type=type(e).__name__,
                message="DeepEval modules could not be imported. Check installation and dependencies.",
            )
            raise RuntimeError(f"DeepEval import failed: {e}. Ensure deepeval is installed in the environment.") from e
        except Exception as e:
            logger.error(
                "deepeval_execution_failed",
                error=str(e),
                error_type=type(e).__name__,
                message="DeepEval metric execution failed. Check API keys, network connectivity, and DeepEval configuration.",
            )
            raise RuntimeError(f"DeepEval metric execution failed: {e}. Check logs for details.") from e
        
        # Ensure all expected keys exist with fallback
        for k in ("coverage", "specificity", "feasibility", "duplication"):
            component_scores.setdefault(k, 0.0)
        
        return component_scores

    def _summarize_for_eval(self, draft: BacklogDraft, findings: AuditFindings) -> str:
        """Summarize backlog and findings for evaluation."""
        lines = [
            f"**Epics**: {len(draft.epics)}",
            f"**Total Tasks**: {sum(len(e.tasks) for e in draft.epics)}",
            f"**Assumptions**: {len(draft.assumptions)}",
            f"**Risks**: {len(draft.risks)}",
            "",
            "**Audit Findings:**",
            f"- Issues: {len(findings.issues)}",
            f"- Suggestions: {len(findings.suggestions)}",
            f"- Overlaps: {len(findings.overlaps)}",
            "",
        ]
        
        if findings.issues:
            lines.append("**Top Issues:**")
            lines.extend([f"- {issue}" for issue in findings.issues[:3]])
            lines.append("")
        
        # Sample epic/task details
        lines.append("**Sample Epics:**")
        for epic in draft.epics[:2]:
            lines.append(f"- {epic.id}: {epic.title} ({len(epic.tasks)} tasks)")
            for task in epic.tasks[:2]:
                lines.append(f"  - {task.id}: {task.title} ({len(task.acceptance_criteria)} ACs)")
        
        return "\n".join(lines)


