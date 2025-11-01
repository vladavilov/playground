from typing import List, Optional
from pydantic import BaseModel, Field
from workflow_models.requirements_models import Requirement
from workflow_models.agent_models import DraftRequirements, PromptAnalysis, RetrievedContext, AuditFindings
from orchestrator.experts.clients.llm import get_llm
from orchestrator.prompts import build_chat_prompt, REQUIREMENTS_ENGINEER


class RequirementsEngineer:
    def __init__(self) -> None:
        pass

    async def synthesize(
        self, 
        analysis: PromptAnalysis, 
        context: RetrievedContext, 
        findings: AuditFindings | None = None,
        previous_draft: any = None
    ) -> DraftRequirements:
        intents = list(analysis.intents or [])
        # Aggregate context: final answer + key facts + citations
        contexts: List[str] = []
        if getattr(context, "context_answer", None):
            contexts.append(str(context.context_answer))
        contexts.extend([str(k) for k in getattr(context, "key_facts", []) or []])
        # Format citations as: [Document Name] "text preview..."
        contexts.extend([f"[{c.document_name}] {c.text_preview}" for c in (getattr(context, "citations", []) or [])])
        
        # Include previous requirements for conversation continuity
        if previous_draft:
            prev_text = self._format_previous_requirements(previous_draft)
            if prev_text:
                contexts.insert(0, prev_text)
        
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

        tmpl = build_chat_prompt(REQUIREMENTS_ENGINEER)
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
    
    def _format_previous_requirements(self, bundle: any) -> str:
        """Format previous requirements bundle as context text for refinement."""
        if not bundle:
            return ""
        
        lines = ["# Previous Requirements Context"]
        lines.append("User is refining/enhancing these existing requirements:")
        lines.append("")
        
        # Business Requirements
        if hasattr(bundle, 'business_requirements') and bundle.business_requirements:
            lines.append("## Previous Business Requirements:")
            for req in bundle.business_requirements:
                lines.append(f"- {req.id}: {req.title}")
                if hasattr(req, 'description') and req.description:
                    lines.append(f"  Description: {req.description[:150]}...")
        
        # Functional Requirements
        if hasattr(bundle, 'functional_requirements') and bundle.functional_requirements:
            lines.append("")
            lines.append("## Previous Functional Requirements:")
            for req in bundle.functional_requirements:
                lines.append(f"- {req.id}: {req.title}")
                if hasattr(req, 'description') and req.description:
                    lines.append(f"  Description: {req.description[:150]}...")
        
        lines.append("")
        lines.append("NOTE: Build upon these requirements. Preserve them unless user asks to change/remove specific ones.")
        
        return "\n".join(lines)



