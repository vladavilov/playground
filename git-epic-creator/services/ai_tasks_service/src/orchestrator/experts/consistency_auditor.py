"""ConsistencyAuditor expert - validates backlog quality and consistency."""

from typing import List
from pydantic import BaseModel, Field

from task_models.agent_models import BacklogDraft, AuditFindings
from utils.llm_client_factory import create_llm
from orchestrator.prompts import CONSISTENCY_AUDITOR, build_chat_prompt
from utils.chunk_utils import truncate_chunk_text


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
        
        prompt_tmpl = build_chat_prompt(CONSISTENCY_AUDITOR)
        
        llm = create_llm(use_fast_model=True)
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
            epic_desc = truncate_chunk_text(epic.description, 2000)
            lines.extend([
                f"**Epic {epic.id}**: {epic.title}",
                f"  Description: {epic_desc}",
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


