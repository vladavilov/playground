from typing import List, Optional
from pydantic import BaseModel, Field
from workflow_models.requirements_models import Requirement
from workflow_models.agent_models import DraftRequirements, PromptAnalysis, RetrievedContext, AuditFindings
from langchain_core.prompts import ChatPromptTemplate
from orchestrator.experts.clients.llm import get_llm


class RequirementsEngineer:
    def __init__(self) -> None:
        pass

    async def synthesize(self, analysis: PromptAnalysis, context: RetrievedContext, findings: AuditFindings | None = None) -> DraftRequirements:
        intents = list(analysis.intents or [])
        # Aggregate context: final answer + key facts + citations
        contexts: List[str] = []
        if getattr(context, "context_answer", None):
            contexts.append(str(context.context_answer))
        contexts.extend([str(k) for k in getattr(context, "key_facts", []) or []])
        contexts.extend([f"citation:{c}" for c in (getattr(context, "citations", []) or [])])
        findings_payload = {
            "issues": list((findings.issues if findings else []) or []),
            "suggestions": list((findings.suggestions if findings else []) or []),
        }

        class RequirementOut(BaseModel):
            id: Optional[str] = Field(default=None)
            title: Optional[str] = Field(default=None)
            description: Optional[str] = Field(default=None)
            acceptance_criteria: List[str] = Field(default_factory=list)
            priority: Optional[str] = Field(default=None)

        class DraftOut(BaseModel):
            business_requirements: List[RequirementOut] = Field(default_factory=list)
            functional_requirements: List[RequirementOut] = Field(default_factory=list)
            assumptions: List[str] = Field(default_factory=list)
            risks: List[str] = Field(default_factory=list)

        system = (
            "You are a senior requirements engineer. Produce grounded, testable requirements from intents and evidence. "
            "Use Given/When/Then for all acceptance criteria. Prioritize items (Must/Should/Could/Won't). "
            "Respond ONLY with JSON object with keys: {{business_requirements, functional_requirements, assumptions, risks}}."
        )
        tmpl = ChatPromptTemplate.from_messages([
            ("system", system),
            ("human", "intents: {intents}\ncontexts: {contexts}\nfindings: {findings}"),
        ])
        llm = get_llm()
        chain = tmpl | llm.with_structured_output(DraftOut)
        out: DraftOut = await chain.ainvoke({"intents": intents, "contexts": contexts, "findings": findings_payload})

        def _mk(items: List[RequirementOut]) -> List[Requirement]:
            out_items: List[Requirement] = []
            for it in items:
                try:
                    out_items.append(Requirement(
                        id=str(it.id or "R"),
                        title=str(it.title or "Untitled"),
                        description=str(it.description or ""),
                        acceptance_criteria=[str(x) for x in (it.acceptance_criteria or [])],
                        priority=str(it.priority or "Should"),
                    ))
                except Exception:
                    continue
            return out_items

        br = _mk(out.business_requirements)
        fr = _mk(out.functional_requirements)
        if not br and not fr:
            raise RuntimeError("empty llm output")
        return DraftRequirements(
            business_requirements=br,
            functional_requirements=fr,
            assumptions=[str(a) for a in out.assumptions],
            risks=[str(r) for r in out.risks],
        )



