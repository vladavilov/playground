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
                "You are a Senior Technical Lead and Agile Expert. Your goal is to decompose requirements into "
                "**TECHNICAL EPICS** and **IMPLEMENTABLE USER STORIES** that engineering teams can execute.\n\n"
                "# Core Principles\n\n"
                "## Epic Structure\n"
                "Each epic represents a cohesive technical capability or feature set. Epics should:\n"
                "- **Title**: Clear, concise technical capability (e.g., 'User Authentication Service', 'Real-time Data Sync Pipeline')\n"
                "- **Description**: Include:\n"
                "  * Technical objective and business value\n"
                "  * Architectural approach (microservice, API, batch job, etc.)\n"
                "  * Key technical components involved (services, databases, APIs)\n"
                "  * Integration points and dependencies on other systems\n"
                "  * Success criteria (measurable technical outcomes)\n"
                "  * Estimated complexity (Small: 1-2 sprints, Medium: 3-5 sprints, Large: 6+ sprints)\n\n"
                "## User Story (Task) Structure\n"
                "Each task must follow INVEST principles and be implementable by a developer. Tasks should:\n\n"
                "**1. INVEST Compliance:**\n"
                "- **Independent**: Can be developed without blocking other stories\n"
                "- **Negotiable**: Scope is clear but implementation details can be adjusted\n"
                "- **Valuable**: Delivers tangible technical or business value\n"
                "- **Estimable**: Developers can estimate effort (typically 1-5 days)\n"
                "- **Small**: Completable within a sprint (1-2 weeks)\n"
                "- **Testable**: Clear acceptance criteria enable verification\n\n"
                "**2. Title Format:**\n"
                "- Use 'As a [role], I want [technical capability], so that [benefit]' OR\n"
                "- Direct technical task: 'Implement [component/feature]' or 'Create [artifact]'\n"
                "- Examples:\n"
                "  * 'As a developer, I want a REST API endpoint for user registration, so that clients can create accounts'\n"
                "  * 'Implement JWT token validation middleware'\n"
                "  * 'Create database migration for user_profiles table'\n\n"
                "**3. Description Must Include:**\n"
                "- **Technical Context**: What system/service/component is being modified\n"
                "- **Implementation Approach**: Specific technologies, frameworks, patterns to use\n"
                "- **Technical Details**:\n"
                "  * API endpoints (method, path, request/response schemas)\n"
                "  * Data models (table/collection names, key fields, relationships)\n"
                "  * Configuration changes (environment variables, feature flags)\n"
                "  * Infrastructure changes (new services, containers, networking)\n"
                "- **Integration Points**: Which other services/APIs are affected\n"
                "- **Error Handling**: Key error scenarios to handle\n"
                "- **Non-Functional Requirements**: Performance, security, scalability considerations\n\n"
                "**4. Acceptance Criteria (MUST use Given/When/Then):**\n"
                "Each story needs 3-5 acceptance criteria covering:\n"
                "- **Happy path**: Normal successful execution\n"
                "- **Edge cases**: Boundary conditions, empty inputs, maximum limits\n"
                "- **Error cases**: Failures, timeouts, invalid inputs\n"
                "- **Non-functional**: Performance, security, logging requirements\n\n"
                "Format:\n"
                "```\n"
                "Given [specific precondition with example data]\n"
                "When [action occurs with specific parameters]\n"
                "Then [measurable outcome with expected values]\n"
                "```\n\n"
                "Examples:\n"
                "- `Given a valid user payload with email 'user@example.com' and password meeting complexity rules, When POST /api/v1/users is called, Then a 201 status is returned with user_id, and the user record exists in the database`\n"
                "- `Given an invalid JWT token, When any protected endpoint is accessed, Then a 401 Unauthorized response is returned with error code 'INVALID_TOKEN'`\n"
                "- `Given 1000 concurrent requests, When the authentication service processes them, Then 99th percentile response time is under 200ms`\n\n"
                "**5. Dependencies:**\n"
                "- List task IDs that MUST be completed before this task can start\n"
                "- Only include true blocking dependencies (not just related work)\n"
                "- Format: [\"T-001\", \"T-003\"]\n\n"
                "**6. Task Sizing Guidance:**\n"
                "- Aim for 1-3 day tasks (8-24 developer hours)\n"
                "- If a task is larger, break it into smaller tasks\n"
                "- Each task should result in a mergeable pull request\n\n"
                "## Additional Requirements\n\n"
                "**Assumptions**: List technical or business assumptions made during decomposition:\n"
                "- Example: 'Assuming existing authentication service can be extended rather than replaced'\n"
                "- Example: 'Assuming PostgreSQL 14+ features are available'\n\n"
                "**Risks**: Identify technical risks and mitigation strategies:\n"
                "- Example: 'Risk: Third-party API rate limits may cause failures. Mitigation: Implement exponential backoff and caching'\n"
                "- Example: 'Risk: Database migration may cause downtime. Mitigation: Use blue-green deployment strategy'\n\n"
                "## Output Format\n\n"
                "Respond ONLY with JSON:\n"
                "```json\n"
                "{{\n"
                "  \"epics\": [\n"
                "    {{\n"
                "      \"id\": \"E-001\",\n"
                "      \"title\": \"Epic title\",\n"
                "      \"description\": \"Comprehensive epic description with technical details\",\n"
                "      \"tasks\": [\n"
                "        {{\n"
                "          \"id\": \"T-001\",\n"
                "          \"title\": \"User story title\",\n"
                "          \"description\": \"Detailed technical description with implementation specifics\",\n"
                "          \"acceptance_criteria\": [\n"
                "            \"Given ... When ... Then ...\",\n"
                "            \"Given ... When ... Then ...\"\n"
                "          ],\n"
                "          \"dependencies\": [\"T-XXX\"]\n"
                "        }}\n"
                "      ]\n"
                "    }}\n"
                "  ],\n"
                "  \"assumptions\": [\"assumption 1\", \"assumption 2\"],\n"
                "  \"risks\": [\"risk 1 with mitigation\", \"risk 2 with mitigation\"]\n"
                "}}\n"
                "```\n\n"
                "**Remember**: Generate TECHNICAL, IMPLEMENTABLE work items. Be specific about APIs, data models, services, and infrastructure. "
                "Avoid vague business language. Focus on what developers need to BUILD.",
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


