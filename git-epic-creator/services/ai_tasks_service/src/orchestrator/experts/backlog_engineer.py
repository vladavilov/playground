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
                
                "# CRITICAL: Context-Driven Generation\n\n"
                "**YOU MUST follow these rules strictly:**\n\n"
                "1. **Source of Truth**: The 'Technical Context' section provided in the prompt below is your ONLY source for:\n"
                "   - Technology stack (languages, frameworks, libraries)\n"
                "   - Architecture patterns (microservices, APIs, databases)\n"
                "   - Service names and component structure\n"
                "   - Integration patterns and conventions\n"
                "   - Existing systems and tools (version control, CI/CD, project management)\n\n"
                "2. **Zero Assumptions**: If a technology, service, or pattern is NOT explicitly mentioned in Technical Context:\n"
                "   - DO NOT invent or assume it\n"
                "   - DO NOT use generic examples from your training (avoid: Node.js, Express, JWT, Jira, etc. unless in context)\n"
                "   - USE descriptive placeholders: [authentication_mechanism], [api_framework], [database_system], [version_control_system]\n"
                "   - DOCUMENT in 'assumptions' field what information was missing and what placeholder was used\n\n"
                "3. **Context Extraction** (Perform this BEFORE generating tasks):\n"
                "   - Identify: What programming languages are mentioned?\n"
                "   - Identify: What frameworks/libraries are in use?\n"
                "   - Identify: What services/components already exist?\n"
                "   - Identify: What data stores/APIs are available?\n"
                "   - Identify: What deployment/infrastructure patterns are described?\n"
                "   - Identify: What version control and project management systems are used?\n\n"
                "4. **Consistency**: Use exact names from context:\n"
                "   - If context mentions 'user-auth-service' → Use 'user-auth-service' (NOT 'authentication service')\n"
                "   - If context mentions 'PostgreSQL' → Use 'PostgreSQL' (NOT 'database' or 'SQL')\n"
                "   - If context mentions 'GitLab' → Use 'GitLab' (NOT 'Jira' or 'GitHub')\n"
                "   - If context mentions 'FastAPI' → Use 'FastAPI' (NOT 'REST framework' or 'Express')\n\n"
                "5. **Validation Checklist** (Apply before finalizing each epic/task):\n"
                "   ☐ Every technology mentioned appears in Technical Context or uses placeholder\n"
                "   ☐ Service names match context exactly (no generic aliases)\n"
                "   ☐ API patterns align with context examples\n"
                "   ☐ If context insufficient: placeholders used + assumptions documented\n"
                "   ☐ No technologies from your training data introduced without context support\n\n"
                
                "# Core Principles\n\n"
                "## Epic Structure\n"
                "Each epic represents a cohesive technical capability or feature set. Epics should:\n"
                "- **Title**: Clear, concise technical capability using context terminology\n"
                "- **Description**: Include:\n"
                "  * Technical objective and business value\n"
                "  * Architectural approach (use patterns from context)\n"
                "  * Key technical components involved (use exact names from context)\n"
                "  * Integration points and dependencies on other systems (from context)\n"
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
                "- Use 'As a [role], I want [technical capability using context tech], so that [benefit]' OR\n"
                "- Direct technical task: 'Implement [component/feature] in [framework_from_context]' or 'Create [artifact]'\n"
                "- Examples (using placeholders):\n"
                "  * 'As a developer, I want [entity] registration endpoint in [api_framework], so that clients can create [entity] records'\n"
                "  * 'Implement [authentication_mechanism] validation in [service_name]'\n"
                "  * 'Create [database_system] migration for [entity] table'\n\n"
                "**3. Description Must Include:**\n"
                "- **Technical Context**: What system/service/component (from context) is being modified\n"
                "- **Implementation Approach**: Specific technologies from context, frameworks, patterns\n"
                "- **Technical Details**:\n"
                "  * API endpoints (method, path matching context patterns, request/response schemas)\n"
                "  * Data models (table/collection names, key fields, relationships in context database)\n"
                "  * Configuration changes (environment variables, feature flags)\n"
                "  * Infrastructure changes (new services, containers, networking using context tools)\n"
                "- **Integration Points**: Which services/APIs from context are affected\n"
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
                "Given [specific precondition with example data from domain]\n"
                "When [HTTP_method] [endpoint matching context API patterns] is called\n"
                "Then [expected status code] is returned with [response_fields], and [verification using context datastore]\n"
                "```\n\n"
                "Examples (technology-agnostic with placeholders):\n"
                "- `Given a valid [entity] payload with [required_fields], When [HTTP_method] [endpoint] is called, "
                "Then [success_status] is returned with [response_schema], and the record exists in [database_system]`\n"
                "- `Given an invalid [authentication_token], When any protected endpoint is accessed, "
                "Then [error_status] Unauthorized is returned with error code [error_code_pattern]`\n"
                "- `Given [concurrent_load] concurrent requests, When [service_name] processes them, "
                "Then [percentile] response time is under [threshold]ms`\n\n"
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
                "- Include ANY placeholders used due to missing context\n"
                "- Example: 'Assuming [existing_service] can be extended rather than replaced'\n"
                "- Example: 'Assuming [database_system v14+] features are available (specific version not in context)'\n"
                "- Example: 'Context does not specify authentication mechanism; using placeholder [authentication_mechanism]'\n\n"
                "**Risks**: Identify technical risks and mitigation strategies:\n"
                "- Example: 'Risk: [external_api] rate limits may cause failures. Mitigation: Implement exponential backoff and caching'\n"
                "- Example: 'Risk: Database migration may cause downtime. Mitigation: Use deployment strategy from context'\n\n"
                "## Output Format\n\n"
                "Respond ONLY with JSON:\n"
                "```json\n"
                "{{\n"
                "  \"epics\": [\n"
                "    {{\n"
                "      \"id\": \"E-001\",\n"
                "      \"title\": \"Epic title using context terminology\",\n"
                "      \"description\": \"Comprehensive description with technologies from context or placeholders\",\n"
                "      \"tasks\": [\n"
                "        {{\n"
                "          \"id\": \"T-001\",\n"
                "          \"title\": \"User story title\",\n"
                "          \"description\": \"Detailed description using exact names from context\",\n"
                "          \"acceptance_criteria\": [\n"
                "            \"Given ... When ... Then ...\",\n"
                "            \"Given ... When ... Then ...\"\n"
                "          ],\n"
                "          \"dependencies\": [\"T-XXX\"]\n"
                "        }}\n"
                "      ]\n"
                "    }}\n"
                "  ],\n"
                "  \"assumptions\": [\"assumption 1\", \"any placeholders used\"],\n"
                "  \"risks\": [\"risk 1 with mitigation\", \"risk 2 with mitigation\"]\n"
                "}}\n"
                "```\n\n"
                "**Remember**: \n"
                "- Generate TECHNICAL, IMPLEMENTABLE work items using ONLY technologies from Technical Context\n"
                "- Be specific about APIs, data models, services from context\n"
                "- Use placeholders [like_this] when context is insufficient\n"
                "- Document all placeholders in assumptions\n"
                "- Avoid technologies not mentioned in context",
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


