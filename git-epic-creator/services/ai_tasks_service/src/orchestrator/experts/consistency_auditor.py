"""ConsistencyAuditor expert - validates backlog quality and consistency."""

from typing import List
from pydantic import BaseModel, Field
from langchain_core.prompts import ChatPromptTemplate

from task_models.agent_models import BacklogDraft, AuditFindings
from orchestrator.experts.clients.llm import get_llm


class ConsistencyAuditor:
    """Expert that audits backlog for consistency, overlaps, dependencies, and testability."""

    def __init__(self) -> None:
        pass

    async def audit(
        self,
        draft: BacklogDraft,
        requirements: str,
    ) -> AuditFindings:
        """Audit backlog draft for issues and improvement opportunities.
        
        Args:
            draft: Generated backlog draft
            requirements: Original requirements text
            
        Returns:
            AuditFindings with issues, suggestions, and overlaps
        """
        class AuditOut(BaseModel):
            issues: List[str] = Field(default_factory=list, description="Identified issues")
            suggestions: List[str] = Field(default_factory=list, description="Improvement suggestions")
            overlaps: List[str] = Field(default_factory=list, description="Overlapping items")

        # Build audit prompt with backlog summary
        backlog_summary = self._summarize_backlog(draft)
        
        prompt_tmpl = ChatPromptTemplate.from_messages([
            (
                "system",
                "You are a Quality Assurance Lead reviewing a backlog for consistency.\n\n"
                "**Check for:**\n"
                "- Overlapping or duplicate tasks/epics\n"
                "- Missing or invalid dependencies\n"
                "- Incomplete or untestable acceptance criteria\n"
                "- Tasks that are too large (not following INVEST)\n"
                "- Gaps in coverage vs. requirements\n\n"
                "Respond ONLY with JSON: {{\"issues\": string[], \"suggestions\": string[], \"overlaps\": string[]}}",
            ),
            ("human", "### Requirements\n{requirements}\n\n### Backlog\n{backlog_summary}"),
        ])
        
        llm = get_llm()
        chain = prompt_tmpl | llm.with_structured_output(AuditOut)
        out: AuditOut = await chain.ainvoke({
            "requirements": requirements,
            "backlog_summary": backlog_summary,
        })
        
        return AuditFindings(
            issues=out.issues,
            suggestions=out.suggestions,
            overlaps=out.overlaps,
        )

    def _summarize_backlog(self, draft: BacklogDraft) -> str:
        """Create a concise backlog summary for audit."""
        lines: List[str] = []
        
        for epic in draft.epics:
            lines.extend([
                f"**Epic {epic.id}**: {epic.title}",
                f"  Description: {epic.description[:200]}...",
                f"  Tasks: {len(epic.tasks)}",
            ])
            
            # Show first 3 tasks with details
            for task in epic.tasks[:3]:
                lines.append(f"  - Task {task.id}: {task.title}")
                lines.append(f"    AC: {len(task.acceptance_criteria)} criteria")
                if task.dependencies:
                    lines.append(f"    Dependencies: {', '.join(task.dependencies)}")
            
            # Show remaining count
            if remaining := len(epic.tasks) - 3:
                lines.append(f"  - ... and {remaining} more tasks")
            lines.append("")
        
        return "\n".join(lines)


