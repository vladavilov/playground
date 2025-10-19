"""Evaluator expert - computes quality scores for generated backlog."""

from typing import Dict
from pydantic import BaseModel, Field
from langchain_core.prompts import ChatPromptTemplate
import structlog

from task_models.agent_models import BacklogDraft, AuditFindings, EvaluationReport
from config import get_ai_tasks_settings
from orchestrator.experts.clients.llm import get_llm
from utils.deepeval_utils import evaluate_with_metrics, StrictGEval
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
        
        prompt_tmpl = ChatPromptTemplate.from_messages([
            (
                "system",
                "You are a Backlog Evaluator. Analyze the backlog and provide:\n"
                "- **rationale**: Brief explanation of backlog quality\n"
                "- **gaps**: Top 3 gaps (what's missing or unclear)\n\n"
                "Respond ONLY with JSON: {{\"rationale\": string, \"gaps\": string[]}}",
            ),
            ("human", "### Requirements\n{requirements}\n\n### Backlog & Findings\n{backlog_summary}"),
        ])
        
        llm = get_llm()
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
        # Build backlog text representation
        parts: list[str] = []
        for epic in draft.epics:
            parts.append(f"Epic {epic.id}: {epic.title}. {epic.description}")
            for task in epic.tasks:
                ac_text = "; ".join(task.acceptance_criteria)
                deps_text = f" (depends on: {', '.join(task.dependencies)})" if task.dependencies else ""
                parts.append(f"  Task {task.id}: {task.title}. {task.description}. ACs: {ac_text}{deps_text}")
        
        if draft.assumptions:
            parts.append("Assumptions: " + "; ".join(draft.assumptions))
        if draft.risks:
            parts.append("Risks: " + "; ".join(draft.risks))
        
        backlog_text = "\n".join(p for p in parts if p) or "Empty backlog"
        
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
        
        logger.info(
            "deepeval_test_case_building",
            backlog_text_length=len(backlog_text),
            contexts_count=len(contexts),
            requirements_length=len(requirements),
        )
        
        try:
            # Build test case
            test_case = LLMTestCase(
                input=requirements,
                actual_output=backlog_text,
                retrieval_context=contexts,
                context=contexts,
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
                        "name": "Specificity",
                        "criteria": "Are tasks technically clear, testable with Given/When/Then criteria, and have non-ambiguous scopes?",
                        "evaluation_steps": [
                            "Identify all tasks and their acceptance criteria in the backlog",
                            "Check if acceptance criteria follow Given/When/Then format",
                            "Verify that task descriptions are technically specific and clear",
                            "Assess if task scopes are unambiguous and well-defined",
                            "Check for presence of technical details (APIs, data models, endpoints)",
                            "Assign a score based on technical clarity and testability"
                        ],
                        "evaluation_params": [LLMTestCaseParams.ACTUAL_OUTPUT, LLMTestCaseParams.CONTEXT],
                        "strict_mode": False,
                        "verbose_mode": False,
                    },
                },
                "feasibility": {
                    "class": AnswerRelevancyMetric,
                    "kwargs": {},
                },
                "duplication": {
                    "class": StrictGEval,
                    "kwargs": {
                        "name": "Duplication",
                        "criteria": "Are duplicates minimized? Tasks and epics should not overlap significantly or repeat the same functionality.",
                        "evaluation_steps": [
                            "Identify all epics and tasks in the backlog",
                            "Compare epic titles and descriptions for similarity",
                            "Compare task titles and descriptions for overlap",
                            "Check if multiple tasks/epics address the same requirement",
                            "Evaluate if audit findings mention duplicate issues",
                            "Assign a score based on uniqueness (1.0 = no duplicates, 0.0 = many duplicates)"
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


