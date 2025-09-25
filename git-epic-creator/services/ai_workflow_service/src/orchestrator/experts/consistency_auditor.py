from workflow_models.agent_models import DraftRequirements, RetrievedContext, AuditFindings
from typing import List, Set, Dict
from pydantic import BaseModel, Field
from langchain_core.prompts import ChatPromptTemplate
from orchestrator.llm import get_llm

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

        axes: Dict[str, float] = {}

        try:
            from deepeval.test_case import LLMTestCase, LLMTestCaseParams  # type: ignore
            from deepeval.metrics import (  # type: ignore
                FaithfulnessMetric,
                AnswerRelevancyMetric,
                GEval,
            )

            test_case = LLMTestCase(
                input=user_prompt,
                actual_output=answer_text,
                retrieval_context=contexts,
                context=contexts,
            )

            faithfulness_metric = FaithfulnessMetric()
            faithfulness_metric.measure(test_case)
            axes["faithfulness"] = float(getattr(faithfulness_metric, "score", 0.0) or 0.0)

            groundedness_metric = GEval(
                name="Citations",
                criteria=(
                    "Does the actual output cite or clearly derive from the provided context?"
                ),
                evaluation_params=[LLMTestCaseParams.ACTUAL_OUTPUT, LLMTestCaseParams.CONTEXT],
                strict_mode=False,
            )
            groundedness_metric.measure(test_case)
            axes["groundedness"] = float(getattr(groundedness_metric, "score", 0.0) or 0.0)

            relevancy_metric = AnswerRelevancyMetric()
            relevancy_metric.measure(test_case)
            axes["response_relevancy"] = float(getattr(relevancy_metric, "score", 0.0) or 0.0)

            completeness_metric = GEval(
                name="Completeness",
                criteria=(
                    "All user intents and constraints are fully addressed with grounded requirements and testable acceptance criteria."
                ),
                evaluation_params=[LLMTestCaseParams.INPUT, LLMTestCaseParams.ACTUAL_OUTPUT],
                strict_mode=False,
            )
            completeness_metric.measure(test_case)
            axes["completeness"] = float(getattr(completeness_metric, "score", 0.0) or 0.0)
        except Exception:
            # If deepeval is unavailable or errors, default to zeros
            pass

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
