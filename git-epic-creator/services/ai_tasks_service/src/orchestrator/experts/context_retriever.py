"""ContextRetriever expert - fetches technical context from GraphRAG."""

from typing import Any, List
import structlog

from task_models.agent_models import RequirementsAnalysis, RetrievedContext, Citation
from orchestrator.experts.clients.graphrag_client import GraphRAGClient
from config import get_ai_tasks_settings
from utils.citation_parser import parse_citations_from_response, parse_key_facts
from utils.chunk_utils import truncate_chunk_text

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
        # Summarize requirements to reduce query size (token optimization)
        summarized_requirements = self._summarize_requirements(analysis.requirements_text)
        
        # Build query emphasizing implementation details
        query = self._build_query(analysis, summarized_requirements)
        
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
        
        # Parse response using shared utility
        key_facts = parse_key_facts(data)
        citations = parse_citations_from_response(data, Citation)
        
        # Deduplicate citations by chunk_id while preserving order
        seen = set()
        dedup_citations = []
        for c in citations:
            if c.chunk_id not in seen:
                seen.add(c.chunk_id)
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

    def _summarize_requirements(self, requirements_text: str) -> str:
        """Reduce requirements text to core intent for retrieval query (token optimization).
        
        Args:
            requirements_text: Full requirements text
            
        Returns:
            Summarized requirements (max 300 chars)
        """
        MAX_REQ_LENGTH = 10000
        return truncate_chunk_text(requirements_text, MAX_REQ_LENGTH)

    def _build_query(self, analysis: RequirementsAnalysis, summarized_requirements: str) -> str:
        """Build GraphRAG query emphasizing technical implementation details (optimized)."""
        # Limit extracted elements for token efficiency
        intents = [i.strip() for i in analysis.intents if isinstance(i, str) and i.strip()][:5]
        entities = [e.strip() for e in analysis.entities if isinstance(e, str) and e.strip()][:8]
        constraints = [c.strip() for c in analysis.constraints if isinstance(c, str) and c.strip()][:5]
        
        lines = [
            "### Technical Context Query",
            "",
            "**Requirements:**",
            summarized_requirements,
            "",
            "**Intents:**",
            *([f"- {i}" for i in intents] if intents else ["- (none)"]),
            "",
        ]
        
        # Add entities if present (simplified formatting)
        if entities:
            lines.extend([
                "**Entities:** " + ", ".join(entities),
                "",
            ])
        
        # Add constraints if present (simplified formatting)
        if constraints:
            lines.extend([
                "**Constraints:**",
                *[f"- {c}" for c in constraints],
                "",
            ])
        
        # Simplified technical context request (removed verbose formatting)
        lines.extend([
            "**Required Context:**",
            "1. Technology Stack (languages, frameworks, databases, infrastructure)",
            "2. Service Architecture (existing services, APIs, endpoints)",
            "3. Data Layer (models, schemas, access patterns)",
            "4. Integration Points (external systems, inter-service communication)",
            "",
            "Provide SPECIFIC names and versions from project documentation.",
        ])
        
        return "\n".join(lines)


