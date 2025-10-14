"""BacklogEngineer expert - synthesizes epics and tasks from requirements and context."""

from typing import List
from pydantic import BaseModel, Field
from langchain_core.prompts import ChatPromptTemplate

from task_models.agent_models import RequirementsAnalysis, RetrievedContext, BacklogDraft, AuditFindings
from task_models.backlog_models import Epic, Task
from orchestrator.experts.clients.llm import get_llm


class BacklogEngineer:
    """Expert that synthesizes epics and tasks following INVEST principles."""

    def __init__(self) -> None:
        pass

    async def synthesize(
        self,
        analysis: RequirementsAnalysis,
        context: RetrievedContext,
        findings: AuditFindings,
    ) -> BacklogDraft:
        """Synthesize epics and tasks from requirements and context.
        
        Args:
            analysis: Parsed requirements analysis
            context: Retrieved technical context
            findings: Audit findings from previous iteration (may be empty)
            
        Returns:
            BacklogDraft with generated epics and tasks
        """
        class TaskOut(BaseModel):
            id: str
            title: str
            description: str
            acceptance_criteria: List[str] = Field(default_factory=list)
            dependencies: List[str] = Field(default_factory=list)

        class EpicOut(BaseModel):
            id: str
            title: str
            description: str
            tasks: List[TaskOut] = Field(default_factory=list)

        class BacklogOut(BaseModel):
            epics: List[EpicOut] = Field(default_factory=list)
            assumptions: List[str] = Field(default_factory=list)
            risks: List[str] = Field(default_factory=list)

        # Build comprehensive prompt
        prompt_content = self._build_prompt(analysis, context, findings)
        
        prompt_tmpl = ChatPromptTemplate.from_messages([
            (
                "system",
                "You are a Senior Delivery Lead. Decompose requirements into epics and tasks following INVEST principles.\n\n"
                "**Epic Requirements:**\n"
                "- Clear business outcome\n"
                "- Scope and constraints summary\n"
                "- Success metrics\n"
                "- Top risks and assumptions\n\n"
                "**Task Requirements:**\n"
                "- INVEST (Independent, Negotiable, Valuable, Estimable, Small, Testable)\n"
                "- Given/When/Then acceptance criteria\n"
                "- Dependencies (task IDs)\n"
                "- Technical specificity (APIs, data models, endpoints, configs)\n\n"
                "Respond ONLY with JSON: {{\"epics\": Epic[], \"assumptions\": string[], \"risks\": string[]}}",
            ),
            ("human", "{prompt_content}"),
        ])
        
        llm = get_llm()
        chain = prompt_tmpl | llm.with_structured_output(BacklogOut)
        out: BacklogOut = await chain.ainvoke({"prompt_content": prompt_content})
        
        # Convert to domain models
        epics = []
        for epic_out in out.epics:
            tasks = []
            for task_out in epic_out.tasks:
                tasks.append(
                    Task(
                        id=task_out.id,
                        title=task_out.title,
                        description=task_out.description,
                        acceptance_criteria=task_out.acceptance_criteria,
                        dependencies=task_out.dependencies,
                    )
                )
            
            epics.append(
                Epic(
                    id=epic_out.id,
                    title=epic_out.title,
                    description=epic_out.description,
                    tasks=tasks,
                )
            )
        
        return BacklogDraft(
            epics=epics,
            assumptions=out.assumptions,
            risks=out.risks,
        )

    def _build_prompt(
        self,
        analysis: RequirementsAnalysis,
        context: RetrievedContext,
        findings: AuditFindings,
    ) -> str:
        """Build comprehensive prompt for backlog synthesis."""
        lines = [
            "### Requirements",
            analysis.requirements_text,
            "",
            "### Intents",
            *([f"- {i}" for i in analysis.intents] if analysis.intents else ["- (none)"]),
            "",
            "### Constraints",
            *([f"- {c}" for c in analysis.constraints] if analysis.constraints else ["- (none)"]),
            "",
            "### Technical Context",
            context.context_answer or "(no context available)",
        ]
        
        # Add key facts if available
        if context.key_facts:
            lines.extend([
                "",
                "**Key Facts:**",
                *(f"- {kf}" for kf in context.key_facts[:5]),
            ])
        
        # Include audit findings if this is a revision
        if findings.issues or findings.suggestions:
            lines.extend(["", "### Previous Iteration Feedback"])
            if findings.issues:
                lines.extend([
                    "**Issues to address:**",
                    *(f"- {issue}" for issue in findings.issues[:3]),
                ])
            if findings.suggestions:
                lines.extend([
                    "**Suggestions:**",
                    *(f"- {sug}" for sug in findings.suggestions[:3]),
                ])
        
        return "\n".join(lines)


