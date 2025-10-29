"""ContextRetriever expert - fetches technical context from GraphRAG."""

from typing import Any, List
import structlog

from task_models.agent_models import RequirementsAnalysis, RetrievedContext
from orchestrator.experts.clients.graphrag_client import GraphRAGClient
from config import get_ai_tasks_settings

logger = structlog.get_logger(__name__)


class ContextRetriever:
    """Expert that retrieves technical implementation context via GraphRAG."""

    def __init__(self) -> None:
        settings = get_ai_tasks_settings()
        self.client = GraphRAGClient(
            base_url=settings.http.GRAPH_RAG_SERVICE_URL,
            timeout_sec=settings.http.READ_TIMEOUT,
            max_attempts=settings.RETRY_MAX_ATTEMPTS,
            backoff_base_sec=settings.RETRIEVAL_BACKOFF_BASE_SEC,
        )
        self.top_k = settings.RETRIEVAL_TOP_K

    async def retrieve(
        self,
        analysis: RequirementsAnalysis,
        project_id: Any,
        auth_header: str | None = None,
    ) -> RetrievedContext:
        """Retrieve technical context for epic/task synthesis.
        
        Args:
            analysis: Parsed requirements analysis
            project_id: Project identifier
            auth_header: Optional auth header for GraphRAG service
            
        Returns:
            RetrievedContext with context answer, key facts, and citations
        """
        # Build query emphasizing implementation details
        query = self._build_query(analysis)
        
        try:
            data = await self.client.retrieve(
                query=query,
                project_id=str(project_id),
                top_k=self.top_k,
                auth_header=auth_header,
            )
        except Exception as e:
            logger.warning("GraphRAG retrieval failed", error=str(e))
            # Return empty context on failure
            return RetrievedContext(
                context_answer="",
                key_facts=[],
                citations=[],
            )
        
        # Parse response
        key_facts = []
        citations: List[str] = []
        
        try:
            for kf in data.get("key_facts", []) or []:
                fact = kf.get("fact")
                if isinstance(fact, str) and fact:
                    key_facts.append(fact)
                for cid in kf.get("citations", []) or []:
                    try:
                        citations.append(str(cid))
                    except Exception:
                        continue
            
            # Also include top-level citations if present
            for c in data.get("citations", []) or []:
                try:
                    if isinstance(c, dict) and "chunk_id" in c:
                        citations.append(str(c.get("chunk_id")))
                except Exception:
                    continue
        except Exception:
            pass
        
        # Deduplicate citations while preserving order
        seen = set()
        dedup_citations = []
        for c in citations:
            if c not in seen:
                seen.add(c)
                dedup_citations.append(c)
        
        # Log retrieval results to distinguish genuine absence vs failure
        context_answer = str(data.get("final_answer", ""))
        logger.info(
            "retrieval_context_parsed",
            context_answer_length=len(context_answer),
            key_facts_count=len(key_facts),
            citations_count=len(dedup_citations),
            project_id=str(project_id),
        )
        
        if not key_facts:
            logger.warning(
                "retrieval_key_facts_empty",
                has_context_answer=bool(context_answer),
                raw_response_keys=list(data.keys()),
                message="No key facts returned from retrieval service. This may indicate sparse graph data or retrieval failure."
            )
        
        return RetrievedContext(
            context_answer=context_answer,
            key_facts=key_facts,
            citations=dedup_citations,
        )

    def _build_query(self, analysis: RequirementsAnalysis) -> str:
        """Build GraphRAG query emphasizing technical implementation details."""
        intents = [i.strip() for i in analysis.intents if isinstance(i, str) and i.strip()]
        entities = [e.strip() for e in analysis.entities if isinstance(e, str) and e.strip()]
        constraints = [c.strip() for c in analysis.constraints if isinstance(c, str) and c.strip()]
        
        lines = [
            "### Technical Context Query for Epic/Story Generation",
            "",
            "**Original Requirements:**",
            analysis.requirements_text.strip(),
            "",
            "**Technical Intents:**",
            *([f"- {i}" for i in intents] if intents else ["- (none)"]),
            "",
        ]
        
        # Add entities if present
        if entities:
            lines.extend([
                "**Entities Involved:**",
                f"{', '.join(entities)}",
                "",
            ])
        
        # Add constraints if present
        if constraints:
            lines.extend([
                "**Constraints:**",
                *[f"- {c}" for c in constraints],
                "",
            ])
        
        # Comprehensive technical context request with STRUCTURED output requirement
        lines.extend([
            "**Required Technical Context (STRUCTURED FORMAT):**",
            "",
            "⚠️ CRITICAL: Provide context in the following EXACT structure. Be SPECIFIC with names, versions, and actual technologies found in the codebase/documentation.",
            "",
            "## 1. Technology Stack",
            "List the EXACT technologies, versions, and tools used in this project:",
            "- **Languages**: [specific languages with versions, e.g., Python 3.11, TypeScript 5.0]",
            "- **Backend Frameworks**: [specific frameworks, e.g., FastAPI 0.110, Django 4.2, Spring Boot 3.1]",
            "- **Frontend Frameworks**: [if applicable, e.g., React 18, Vue 3, Angular 16]",
            "- **Databases**: [specific DB technologies and versions, e.g., PostgreSQL 15, Neo4j 5.x, MongoDB 6.0]",
            "- **Caching/Messaging**: [e.g., Redis 7.x, RabbitMQ 3.12, Kafka 3.5]",
            "- **CI/CD & Version Control**: [e.g., GitLab CI/CD, GitHub Actions, Jenkins, Jira, Linear]",
            "- **Infrastructure**: [e.g., Docker 24.x, Kubernetes 1.28, Azure/AWS/GCP]",
            "- **Testing Tools**: [e.g., pytest 7.x, jest 29, JUnit 5]",
            "",
            "## 2. Service Architecture",
            "List existing services/microservices with their EXACT names:",
            "- **[exact-service-name]**: [purpose, tech stack, key endpoints]",
            "  * Example: user-auth-service (FastAPI): Handles authentication, /api/v1/auth/login, /api/v1/auth/token",
            "",
            "## 3. Data Layer",
            "- **Schemas/Models**: [existing data models relevant to requirements, with exact names]",
            "- **Access Patterns**: [ORM/framework used: SQLAlchemy, Prisma, Entity Framework, etc.]",
            "- **Database Names**: [exact database/schema names used in the project]",
            "",
            "## 4. API Conventions",
            "- **Endpoint Patterns**: [actual patterns used, e.g., /api/v1/resource, /v2/resource]",
            "- **Authentication**: [exact mechanism: JWT with Azure AD, OAuth2, API keys, etc.]",
            "- **Response Formats**: [JSON schemas, error handling patterns, status code conventions]",
            "",
            "## 5. Integration & Dependencies",
            "- **External Systems**: [third-party APIs, SaaS services, tools: Stripe, Twilio, SendGrid, etc.]",
            "- **Inter-Service Communication**: [specific patterns: REST APIs, gRPC, message queues with technology]",
            "- **Project Management Tools**: [exact system used: GitLab Issues, Jira, Linear, GitHub Projects]",
            "",
            "## 6. Deployment & Infrastructure",
            "- **Containerization**: [Docker, Podman, etc. with base images if known]",
            "- **Orchestration**: [Kubernetes, Docker Swarm, ECS, etc.]",
            "- **Cloud Provider**: [Azure, AWS, GCP, on-premise]",
            "- **Configuration Management**: [environment variables, config files, Azure App Configuration, etc.]",
            "",
            "**CRITICAL INSTRUCTIONS:**",
            "- Use EXACT names from codebase/documentation (not generic terms)",
            "- If information is not available, explicitly state 'Not found in context'",
            "- Prioritize ACTUAL project details over generic architectural patterns",
            "- Include specific versions when available",
            "- Avoid placeholder examples - only provide what exists in the project",
        ])
        
        return "\n".join(lines)


