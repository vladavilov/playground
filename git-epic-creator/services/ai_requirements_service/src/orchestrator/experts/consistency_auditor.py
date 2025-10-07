from workflow_models.agent_models import DraftRequirements, RetrievedContext, AuditFindings
from typing import List, Set, Dict
from pydantic import BaseModel, Field
from langchain_core.prompts import ChatPromptTemplate
from orchestrator.experts.clients.llm import get_llm
import structlog
import os
from configuration.llm_config import get_llm_config
from deepeval.test_case import LLMTestCase, LLMTestCaseParams
from deepeval.metrics import (
    FaithfulnessMetric,
    AnswerRelevancyMetric,
    GEval,
)
from deepeval.models import LiteLLMModel

logger = structlog.get_logger(__name__)

class ConsistencyAuditor:
    def __init__(self) -> None:
        pass

    async def audit(self, draft: DraftRequirements, context: RetrievedContext, user_prompt: str) -> AuditFindings:
        issues: List[str] = []
        suggestions: List[str] = []

        # Rule: duplicate titles across requirements
        titles: List[str] = [r.title.strip().lower() for r in list(draft.business_requirements) + list(draft.functional_requirements) if r.title]
        seen: Set[str] = set()
        dups: Set[str] = set()
        for t in titles:
            if t in seen:
                dups.add(t)
            else:
                seen.add(t)
        if dups:
            issues.append(f"Duplicate requirement titles: {', '.join(sorted(dups))}")

        # Rule: acceptance criteria should include Given/When/Then tokens
        for r in list(draft.business_requirements) + list(draft.functional_requirements):
            for ac in r.acceptance_criteria:
                tokens = ac.lower()
                if not ("given" in tokens and "when" in tokens and "then" in tokens):
                    issues.append(f"Non-testable AC for {r.id}: '{ac}' lacks Given/When/Then")

        # Rule: missing descriptions or ACs
        for r in list(draft.business_requirements) + list(draft.functional_requirements):
            if not r.description:
                issues.append(f"Requirement {r.id} missing description")
            if not r.acceptance_criteria:
                issues.append(f"Requirement {r.id} missing acceptance criteria")

        # Rule: hint if no assumptions/risks
        if not draft.assumptions:
            suggestions.append("Add explicit assumptions to clarify constraints")
        if not draft.risks:
            suggestions.append("Document key delivery risks")

        payload = self._build_draft_payload(draft)
        system = (
            "You are a senior requirements QA reviewer for finance. Critique the requirements for contradictions, "
            "duplicates, gaps and relevance. Return ONLY JSON object with fields: {{severity (in [0,1]), suggestions (string[])}}."
        )
        class Critique(BaseModel):
            severity: float = 0.0
            suggestions: List[str] = Field(default_factory=list)

        tmpl = ChatPromptTemplate.from_messages([
            ("system", system),
            ("human", "requirements: {requirements}\nassumptions: {assumptions}\nrisks: {risks}\ncontexts: {contexts}"),
        ])
        llm = get_llm()
        chain = tmpl | llm.with_structured_output(Critique)
        out: Critique = await chain.ainvoke({
            "requirements": payload["requirements"],
            "assumptions": payload["assumptions"],
            "risks": payload["risks"],
            "contexts": self._aggregate_context(context),
        })
        for s in out.suggestions:
            if isinstance(s, str) and s:
                suggestions.append(s)

        # Compute component/axis scores using DeepEval (safe fallback to zeros if unavailable)
        axes: Dict[str, float] = await self._evaluate_axes(draft, user_prompt, context)

        return AuditFindings(
            issues=issues,
            suggestions=suggestions,
            llm_critique_severity=float(out.severity or 0.0),
            component_scores={k: float(v) for k, v in axes.items()},
        )

    def _build_draft_payload(self, draft: DraftRequirements) -> dict:
        reqs = [
            {
                "id": r.id,
                "title": r.title,
                "desc": r.description,
                "acs": r.acceptance_criteria,
            }
            for r in list(draft.business_requirements) + list(draft.functional_requirements)
        ]
        return {
            "requirements": reqs,
            "assumptions": draft.assumptions,
            "risks": draft.risks,
        }

    async def _evaluate_axes(self, draft: DraftRequirements, user_prompt: str, context: RetrievedContext) -> Dict[str, float]:
        parts: list[str] = []
        for r in list(draft.business_requirements) + list(draft.functional_requirements):
            ac_text = "; ".join(r.acceptance_criteria)
            parts.append(f"{r.title}: {r.description}. ACs: {ac_text}")
        if draft.assumptions:
            parts.append("Assumptions: " + "; ".join(draft.assumptions))
        if draft.risks:
            parts.append("Risks: " + "; ".join(draft.risks))
        answer_text = ". ".join(p for p in parts if p) or ""

        contexts = self._aggregate_context(context)

        logger.info(
            "deepeval_evaluation_starting",
            requirements_count=len(draft.business_requirements) + len(draft.functional_requirements),
            contexts_count=len(contexts),
            user_prompt_length=len(user_prompt),
        )

        axes: Dict[str, float] = {}

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
            
            logger.info("deepeval_modules_imported_successfully")

            test_case = LLMTestCase(
                input=user_prompt,
                actual_output=answer_text,
                retrieval_context=contexts,
                context=contexts,
            )
            logger.info("deepeval_test_case_created", answer_text_length=len(answer_text))

            # Faithfulness Metric
            logger.info("deepeval_executing_metric", metric="FaithfulnessMetric")
            faithfulness_metric = FaithfulnessMetric(model=custom_model)
            faithfulness_metric.measure(test_case)
            faithfulness_score = float(getattr(faithfulness_metric, "score", 0.0) or 0.0)
            axes["faithfulness"] = faithfulness_score
            logger.info("deepeval_metric_completed", metric="FaithfulnessMetric", score=faithfulness_score)

            # Groundedness Metric (GEval)
            logger.info("deepeval_executing_metric", metric="GEval-Citations")
            groundedness_metric = GEval(
                name="Citations",
                criteria=(
                    "Does the actual output cite or clearly derive from the provided context?"
                ),
                evaluation_steps=[
                    "Identify all claims, facts, or statements in the actual output",
                    "For each claim, verify if it can be traced back to or derived from the provided context",
                    "Check if the actual output includes explicit citations or references to the context",
                    "Evaluate whether the actual output introduces information not present in the context",
                    "Assign a score based on how well the actual output is grounded in the provided context"
                ],
                evaluation_params=[LLMTestCaseParams.ACTUAL_OUTPUT, LLMTestCaseParams.CONTEXT],
                strict_mode=False,
                verbose_mode=True,
                model=custom_model,
            )
            groundedness_metric.measure(test_case)
            groundedness_score = float(getattr(groundedness_metric, "score", 0.0) or 0.0)
            axes["groundedness"] = groundedness_score
            logger.info("deepeval_metric_completed", metric="GEval-Citations", score=groundedness_score)

            # Response Relevancy Metric
            logger.info("deepeval_executing_metric", metric="AnswerRelevancyMetric")
            relevancy_metric = AnswerRelevancyMetric(model=custom_model)
            relevancy_metric.measure(test_case)
            relevancy_score = float(getattr(relevancy_metric, "score", 0.0) or 0.0)
            axes["response_relevancy"] = relevancy_score
            logger.info("deepeval_metric_completed", metric="AnswerRelevancyMetric", score=relevancy_score)

            # Completeness Metric (GEval)
            logger.info("deepeval_executing_metric", metric="GEval-Completeness")
            completeness_metric = GEval(
                name="Completeness",
                criteria=(
                    "All user intents and constraints are fully addressed with grounded requirements and testable acceptance criteria."
                ),
                evaluation_steps=[
                    "Extract all user intents, requirements, and constraints from the input",
                    "Identify each requirement, acceptance criterion, and assumption in the actual output",
                    "Verify that each user intent from the input is addressed in the actual output",
                    "Check if acceptance criteria are testable and follow Given/When/Then format",
                    "Assess if any user intent or constraint is missing or inadequately addressed",
                    "Assign a score based on completeness of coverage and quality of testable criteria"
                ],
                evaluation_params=[LLMTestCaseParams.INPUT, LLMTestCaseParams.ACTUAL_OUTPUT],
                strict_mode=False,
                model=custom_model,
            )
            completeness_metric.measure(test_case)
            completeness_score = float(getattr(completeness_metric, "score", 0.0) or 0.0)
            axes["completeness"] = completeness_score
            logger.info("deepeval_metric_completed", metric="GEval-Completeness", score=completeness_score)

            logger.info(
                "deepeval_evaluation_completed_successfully",
                faithfulness=faithfulness_score,
                groundedness=groundedness_score,
                response_relevancy=relevancy_score,
                completeness=completeness_score,
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
                current_axes=axes,
                message="DeepEval metric execution failed. Check API keys, network connectivity, and DeepEval configuration.",
            )
            raise RuntimeError(f"DeepEval metric execution failed: {e}. Check logs for details.") from e

        for k in ("faithfulness", "groundedness", "response_relevancy", "completeness"):
            axes.setdefault(k, 0.0)
        
        return axes

    def _aggregate_context(self, context: RetrievedContext) -> List[str]:
        parts: List[str] = []
        if getattr(context, "context_answer", None):
            parts.append(str(context.context_answer))
        parts.extend([str(k) for k in getattr(context, "key_facts", []) or []])
        parts.extend([f"citation:{c}" for c in (getattr(context, "citations", []) or [])])
        return parts
