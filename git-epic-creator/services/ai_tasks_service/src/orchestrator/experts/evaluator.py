"""Evaluator expert - computes quality scores for generated backlog."""

from typing import Dict
from pydantic import BaseModel, Field
from langchain_core.prompts import ChatPromptTemplate
import structlog

from task_models.agent_models import BacklogDraft, AuditFindings, EvaluationReport
from config import get_ai_tasks_settings
from orchestrator.experts.clients.llm import get_llm
from configuration.llm_config import get_llm_config
from deepeval.test_case import LLMTestCase, LLMTestCaseParams
from deepeval.metrics import (
    FaithfulnessMetric,
    AnswerRelevancyMetric,
    GEval,
)
from deepeval.models import LiteLLMModel

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
        if not contexts:
            contexts.append("No audit issues found")
        
        logger.info(
            "deepeval_test_case_building",
            backlog_text_length=len(backlog_text),
            contexts_count=len(contexts),
            requirements_length=len(requirements),
        )
        
        component_scores: Dict[str, float] = {}
        
        try:
            llm_config = get_llm_config()
            
            if not llm_config.OAI_KEY:
                raise RuntimeError(
                    "OpenAI API key not configured. Set OAI_KEY environment variable. "
                    "DeepEval requires OpenAI API access for evaluation metrics."
                )
            
            # Create custom Azure OpenAI model for DeepEval pointing to mock service
            custom_model = LiteLLMModel(
                model=f"azure/{llm_config.OAI_MODEL}",
                api_key=llm_config.OAI_KEY,
                api_base=llm_config.OAI_BASE_URL,
                api_version=llm_config.OAI_API_VERSION
            )
            
            logger.info("deepeval_custom_model_configured", 
                        endpoint=llm_config.OAI_BASE_URL,
                        model=llm_config.OAI_MODEL,
                        api_version=llm_config.OAI_API_VERSION)
            
            test_case = LLMTestCase(
                input=requirements,
                actual_output=backlog_text,
                retrieval_context=contexts,
                context=contexts,
            )
            logger.info("deepeval_test_case_created", backlog_text_length=len(backlog_text))
            
            # Faithfulness Metric - Backlog faithful to requirements
            logger.info("deepeval_executing_metric", metric="FaithfulnessMetric")
            faithfulness_metric = FaithfulnessMetric(model=custom_model)
            faithfulness_metric.measure(test_case)
            faithfulness_score = float(getattr(faithfulness_metric, "score", 0.0) or 0.0)
            component_scores["coverage"] = faithfulness_score
            logger.info("deepeval_metric_completed", metric="FaithfulnessMetric", score=faithfulness_score)
            
            # Groundedness Metric (GEval) - Tasks grounded in context
            logger.info("deepeval_executing_metric", metric="GEval-Specificity")
            specificity_metric = GEval(
                name="Specificity",
                criteria=(
                    "Are tasks technically clear, testable with Given/When/Then criteria, "
                    "and have non-ambiguous scopes?"
                ),
                evaluation_steps=[
                    "Identify all tasks and their acceptance criteria in the backlog",
                    "Check if acceptance criteria follow Given/When/Then format",
                    "Verify that task descriptions are technically specific and clear",
                    "Assess if task scopes are unambiguous and well-defined",
                    "Check for presence of technical details (APIs, data models, endpoints)",
                    "Assign a score based on technical clarity and testability"
                ],
                evaluation_params=[LLMTestCaseParams.ACTUAL_OUTPUT, LLMTestCaseParams.CONTEXT],
                strict_mode=False,
                verbose_mode=True,
                model=custom_model,
            )
            specificity_metric.measure(test_case)
            specificity_score = float(getattr(specificity_metric, "score", 0.0) or 0.0)
            component_scores["specificity"] = specificity_score
            logger.info("deepeval_metric_completed", metric="GEval-Specificity", score=specificity_score)
            
            # Relevancy Metric - Backlog relevant to requirements
            logger.info("deepeval_executing_metric", metric="AnswerRelevancyMetric")
            relevancy_metric = AnswerRelevancyMetric(model=custom_model)
            relevancy_metric.measure(test_case)
            relevancy_score = float(getattr(relevancy_metric, "score", 0.0) or 0.0)
            component_scores["feasibility"] = relevancy_score
            logger.info("deepeval_metric_completed", metric="AnswerRelevancyMetric", score=relevancy_score)
            
            # Completeness Metric (GEval) - Duplication check
            logger.info("deepeval_executing_metric", metric="GEval-Duplication")
            duplication_metric = GEval(
                name="Duplication",
                criteria=(
                    "Are duplicates minimized? Tasks and epics should not overlap significantly "
                    "or repeat the same functionality."
                ),
                evaluation_steps=[
                    "Identify all epics and tasks in the backlog",
                    "Compare epic titles and descriptions for similarity",
                    "Compare task titles and descriptions for overlap",
                    "Check if multiple tasks/epics address the same requirement",
                    "Evaluate if audit findings mention duplicate issues",
                    "Assign a score based on uniqueness (1.0 = no duplicates, 0.0 = many duplicates)"
                ],
                evaluation_params=[LLMTestCaseParams.ACTUAL_OUTPUT, LLMTestCaseParams.CONTEXT],
                strict_mode=False,
                model=custom_model,
            )
            duplication_metric.measure(test_case)
            duplication_score = float(getattr(duplication_metric, "score", 0.0) or 0.0)
            component_scores["duplication"] = duplication_score
            logger.info("deepeval_metric_completed", metric="GEval-Duplication", score=duplication_score)
            
            logger.info(
                "deepeval_evaluation_completed_successfully",
                coverage=faithfulness_score,
                specificity=specificity_score,
                feasibility=relevancy_score,
                duplication=duplication_score,
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
                current_scores=component_scores,
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


