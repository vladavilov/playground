from workflow_models.agent_models import DraftRequirements, RetrievedContext, AuditFindings
from typing import List, Set, Dict
from pydantic import BaseModel, Field
from orchestrator.experts.clients.llm import get_llm
from orchestrator.prompts import build_chat_prompt, CONSISTENCY_AUDITOR
import structlog
from utils.deepeval_utils import evaluate_with_metrics
from deepeval.test_case import LLMTestCase
from orchestrator.prompts.rubrics import build_metrics_config

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

        # LLM-based consistency audit using BABOK validation principles
        # Returns severity score (0.0-1.0) that will penalize all component scores:
        #   - 0.0-0.3: Minor issues (style, formatting) → minimal penalty
        #   - 0.4-0.6: Moderate issues (ambiguity, missing details) → medium penalty
        #   - 0.7-1.0: Critical issues (contradictions, gaps, untestable) → severe penalty
        payload = self._build_draft_payload(draft)
        class Critique(BaseModel):
            severity: float = 0.0
            suggestions: List[str] = Field(default_factory=list)

        tmpl = build_chat_prompt(CONSISTENCY_AUDITOR)
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
        """Evaluate requirements quality axes using DeepEval metrics."""
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
        
        # Check if context is placeholder-only (no real retrieval data)
        has_real_context = not (len(contexts) == 1 and contexts[0] == "__NO_CONTEXT_AVAILABLE__")

        logger.info(
            "deepeval_evaluation_starting",
            requirements_count=len(draft.business_requirements) + len(draft.functional_requirements),
            contexts_count=len(contexts),
            has_real_context=has_real_context,
            user_prompt_length=len(user_prompt),
        )

        try:
            # Build test case
            test_case = LLMTestCase(
                input=user_prompt,
                actual_output=answer_text,
                retrieval_context=contexts,
                context=contexts,
            )
            logger.debug("deepeval_test_case_created", answer_text_length=len(answer_text))
            
            # Configure metrics via shared rubrics
            metrics_config = build_metrics_config(has_real_context)
            
            # Execute metrics (telemetry disabled, model cached, serialized GEval execution, 30s timeout)
            axes = await evaluate_with_metrics(test_case, metrics_config)
            
            logger.info(
                "deepeval_evaluation_completed_successfully",
                faithfulness=axes.get("faithfulness", 0.0),
                groundedness=axes.get("groundedness", 0.0),
                response_relevancy=axes.get("response_relevancy", 0.0),
                completeness=axes.get("completeness", 0.0),
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

        # Ensure all expected axes exist with fallback
        for k in ("faithfulness", "groundedness", "response_relevancy", "completeness"):
            axes.setdefault(k, 0.0)
        
        return axes

    def _aggregate_context(self, context: RetrievedContext) -> List[str]:
        parts: List[str] = []
        if getattr(context, "context_answer", None):
            parts.append(str(context.context_answer))
        parts.extend([str(k) for k in getattr(context, "key_facts", []) or []])
        parts.extend([f"citation:{c}" for c in (getattr(context, "citations", []) or [])])
        
        # Guard against empty context - add placeholder for metric filtering
        if not parts:
            logger.warning(
                "retrieval_context_empty",
                message="No context returned from retrieval. Using placeholder to skip groundedness metric."
            )
            parts.append("__NO_CONTEXT_AVAILABLE__")
        
        return parts
