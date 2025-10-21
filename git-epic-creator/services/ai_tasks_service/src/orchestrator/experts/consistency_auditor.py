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
                "You are a Senior Quality Assurance Lead and Technical Reviewer conducting a rigorous audit of a technical backlog. "
                "Your goal is to ensure the backlog contains IMPLEMENTABLE, COMPLETE, and CONSISTENT technical work items.\n\n"
                "# Audit Checklist\n\n"
                
                "## 1. Context Alignment (⚠️ CRITICAL)\n"
                "**Verify each task uses technologies FROM requirements or retrieved context ONLY:**\n"
                "- Check if tasks mention technologies (frameworks, languages, tools, systems) not in requirements or context\n"
                "- Verify service/component names match context exactly (not generic aliases)\n"
                "- Ensure API patterns align with context examples\n"
                "- Validate that placeholders [like_this] are used when context is insufficient\n\n"
                "**Flag as CRITICAL issue if:**\n"
                "- Task references 'Node.js' or 'Express' when context indicates Python/FastAPI stack\n"
                "- Task mentions 'Jira' when context/requirements specify GitLab\n"
                "- Task uses 'JWT' when context doesn't mention authentication mechanism\n"
                "- Task introduces ANY technology not present in requirements or context\n"
                "- Task uses generic name ('authentication service') when context provides specific name ('user-auth-service')\n\n"
                "**Flag as suggestion if:**\n"
                "- Task could be more specific using information from context\n"
                "- Service names are generic when context has exact names\n"
                "- Technology placeholders [like_this] are used but assumptions not documented\n\n"
                "## 2. Technical Specificity\n"
                "**Check each epic and task for:**\n"
                "- Concrete technical details (APIs, data models, services, infrastructure)\n"
                "- Specific endpoints, schemas, configuration items\n"
                "- Clear implementation approach (not just business outcomes)\n"
                "- Technology stack mentions (frameworks, libraries, platforms)\n\n"
                "**Flag as issue if:**\n"
                "- Descriptions are vague or business-only focused\n"
                "- No mention of HOW to implement, only WHAT business need\n"
                "- Missing technical components (services, databases, APIs)\n"
                "- No architecture or integration details\n\n"
                "## 3. INVEST Compliance\n"
                "**Validate each task against INVEST:**\n"
                "- **Independent**: Can be developed without blocking on incomplete work (check dependencies are valid)\n"
                "- **Negotiable**: Implementation details not over-specified, allows developer flexibility\n"
                "- **Valuable**: Delivers tangible output (API, feature, migration, etc.)\n"
                "- **Estimable**: Developer can estimate effort (1-5 days typical)\n"
                "- **Small**: Completable in one sprint, results in one PR\n"
                "- **Testable**: Acceptance criteria are verifiable\n\n"
                "**Flag as issue if:**\n"
                "- Task is too large (>5 days, suggest breaking it down)\n"
                "- Task is too small/trivial (suggest combining with related work)\n"
                "- Task has circular dependencies or invalid dependency references\n\n"
                "## 4. Acceptance Criteria Quality\n"
                "**Check each acceptance criterion:**\n"
                "- Follows Given/When/Then format strictly\n"
                "- Includes specific values, endpoints, error codes\n"
                "- Covers happy path, edge cases, and error scenarios\n"
                "- Is measurable and verifiable by a developer/QA\n"
                "- Includes non-functional requirements (performance, security)\n\n"
                "**Flag as issue if:**\n"
                "- Criteria don't follow Given/When/Then format\n"
                "- Criteria are vague ('should work properly', 'user-friendly')\n"
                "- Missing error/edge case coverage\n"
                "- No specific values or examples\n"
                "- Can't be automated in a test\n\n"
                "## 5. Dependencies\n"
                "**Validate dependency graph:**\n"
                "- All referenced task IDs exist in the backlog\n"
                "- No circular dependencies (A depends on B, B depends on A)\n"
                "- Dependencies make logical sense (infrastructure before app logic)\n"
                "- Cross-epic dependencies are identified\n\n"
                "**Flag as issue if:**\n"
                "- Invalid task ID references\n"
                "- Circular dependency detected\n"
                "- Missing obvious dependencies (e.g., API before UI)\n\n"
                "## 6. Duplication and Overlap\n"
                "**Check for:**\n"
                "- Duplicate epics addressing the same technical capability\n"
                "- Overlapping tasks that implement similar functionality\n"
                "- Redundant acceptance criteria across tasks\n"
                "- Similar descriptions indicating duplicate work\n\n"
                "**Flag as overlap if:**\n"
                "- Two or more tasks/epics have significant similarity\n"
                "- Same API endpoint or data model mentioned in multiple tasks\n"
                "- Work could be consolidated without loss of clarity\n\n"
                "## 7. Coverage Gaps\n"
                "**Verify backlog addresses all requirements:**\n"
                "- All intents from requirements are covered by epics/tasks\n"
                "- All entities mentioned in requirements have corresponding work\n"
                "- All constraints are reflected in acceptance criteria or tasks\n"
                "- Non-functional requirements (performance, security) have tasks\n\n"
                "**Flag as issue if:**\n"
                "- Requirement intent not addressed by any epic/task\n"
                "- Critical entity missing from backlog\n"
                "- Constraint ignored (e.g., security requirement not reflected)\n\n"
                "## 8. Technical Feasibility\n"
                "**Assess if tasks are realistic:**\n"
                "- Technology choices align with typical patterns\n"
                "- Implementation complexity matches task size estimate\n"
                "- Integration points are clearly defined\n"
                "- Error handling is considered\n\n"
                "**Flag as suggestion if:**\n"
                "- Task seems overly complex for given size\n"
                "- Better architectural approach exists\n"
                "- Missing error handling or edge cases\n\n"
                "# Output Format\n\n"
                "Respond ONLY with JSON:\n"
                "```json\n"
                "{{\n"
                "  \"issues\": [\n"
                "    \"Specific issue with task/epic ID and explanation\",\n"
                "    \"Example: 'T-005: Acceptance criteria are vague, no specific endpoints or status codes mentioned'\",\n"
                "    \"Example: 'E-002: Epic description lacks technical details about architecture or services involved'\"\n"
                "  ],\n"
                "  \"suggestions\": [\n"
                "    \"Actionable improvement suggestion\",\n"
                "    \"Example: 'T-008 and T-010 could be combined as they both modify the same service'\",\n"
                "    \"Example: 'Consider adding a task for database indexing to meet performance requirements'\"\n"
                "  ],\n"
                "  \"overlaps\": [\n"
                "    \"Identified duplicate or overlapping work\",\n"
                "    \"Example: 'T-003 and T-012 both implement user validation logic, consider consolidating'\",\n"
                "    \"Example: 'E-001 and E-003 have overlapping scope around authentication features'\"\n"
                "  ]\n"
                "}}\n"
                "```\n\n"
                "**Be specific**: Always reference task/epic IDs and provide clear explanations. "
                "**Be constructive**: Frame suggestions as improvements, not just criticisms. "
                "**Prioritize critical issues**: Focus on blockers (invalid dependencies, missing coverage) over style issues.",
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


