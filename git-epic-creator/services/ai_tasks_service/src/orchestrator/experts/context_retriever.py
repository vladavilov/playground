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
            base_url=settings.GRAPH_RAG_BASE_URL,
            timeout_sec=settings.HTTP_TIMEOUT_SEC,
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
        
        return RetrievedContext(
            context_answer=str(data.get("final_answer", "")),
            key_facts=key_facts,
            citations=dedup_citations,
        )

    def _build_query(self, analysis: RequirementsAnalysis) -> str:
        """Build GraphRAG query emphasizing technical implementation details."""
        intents = [i.strip() for i in analysis.intents if isinstance(i, str) and i.strip()]
        
        lines = [
            "### Requirements for Backlog Generation",
            analysis.requirements_text.strip(),
            "",
            "### Key Intents",
            *([f"- {i}" for i in intents] if intents else ["- (none)"]),
            "",
            "### Context Needed",
            "Provide technical implementation context: APIs, services, data models, "
            "integration points, infrastructure constraints, and tech stack specifics.",
        ]
        
        return "\n".join(lines)


