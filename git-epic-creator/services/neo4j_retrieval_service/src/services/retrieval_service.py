"""Simplified Neo4j Retrieval Service using refactored node architecture.

This module provides the main retrieval service that coordinates:
- Graph creation and caching (singleton pattern)
- Request execution via LangGraph
- Optional Redis-based request deduplication (future enhancement)

Refactored to use:
- Shared utilities from services/shared/src/utils/
- Node-based architecture from retrieval_ms/nodes/
- Direct LLM calls (no retry logic, Azure handles rate limiting)
"""

from typing import Any, Callable, Dict, Optional
import asyncio
from uuid import uuid4
import structlog
from fastapi import HTTPException

from retrieval_ms.graph_factory import create_retrieval_graph

logger = structlog.get_logger(__name__)

# Persistent graph instance (singleton pattern)
_GRAPH_INSTANCE = None
_GRAPH_LOCK = asyncio.Lock()


GetSessionFn = Callable[[], Any]
GetLlmFn = Callable[[], Any]
GetEmbedderFn = Callable[[], Any]


async def _get_or_create_graph(
    get_session: GetSessionFn,
    get_llm: GetLlmFn,
    get_embedder: GetEmbedderFn,
    publisher: Optional[Any] = None,
):
    """Get cached graph or create if not exists (singleton pattern).
    
    Creates the StateGraph only once and reuses it for all requests,
    reducing latency by 200-500ms per request.
    
    Note: Publisher is passed per-request but graph is cached. This is acceptable
    because publisher logic is best-effort (failures don't break retrieval).
    
    Args:
        get_session: Factory function for Neo4j session
        get_llm: Factory function for LLM client
        get_embedder: Factory function for embedder client
        publisher: Optional progress publisher for UI updates
        
    Returns:
        Compiled LangGraph StateGraph
    """
    global _GRAPH_INSTANCE
    if _GRAPH_INSTANCE is None:
        async with _GRAPH_LOCK:
            if _GRAPH_INSTANCE is None:
                logger.info("graph_initialization", message="Creating persistent graph instance")
                _GRAPH_INSTANCE = create_retrieval_graph(
                    get_session,
                    get_llm,
                    get_embedder,
                    publisher
                )
    return _GRAPH_INSTANCE


class Neo4jRetrievalService:
    """Service for executing DRIFT-based retrieval from Neo4j knowledge graph.
    
    Simplified service that delegates to graph nodes for all processing logic.
    Uses singleton pattern for graph caching to minimize overhead.
    
    Examples:
        # Initialize service
        service = Neo4jRetrievalService(
            get_session=get_neo4j_session,
            get_llm=create_llm,
            get_embedder=create_embedder,
            publisher=progress_publisher
        )
        
        # Execute retrieval
        result = await service.retrieve(
            question="What are the main risks?",
            top_k=5,
            project_id="project-uuid"
        )
    """
    
    def __init__(
        self,
        get_session: GetSessionFn,
        get_llm: GetLlmFn,
        get_embedder: GetEmbedderFn,
        publisher: Optional[Any] = None,
    ) -> None:
        """Initialize retrieval service with dependency factories.
        
        Args:
            get_session: Factory function for Neo4j session
            get_llm: Factory function for LLM client
            get_embedder: Factory function for embedder client
            publisher: Optional progress publisher for UI updates
        """
        self._get_session = get_session
        self._get_llm = get_llm
        self._get_embedder = get_embedder
        self._publisher = publisher
    
    async def retrieve(
        self,
        question: str,
        top_k: int,
        project_id: str,
        prompt_id: Optional[str] = None,
    ) -> Dict[str, Any]:
        """Execute DRIFT retrieval pipeline.
        
        Args:
            question: User query
            top_k: Number of results to retrieve per iteration
            project_id: Project UUID
            prompt_id: Optional parent workflow prompt_id for UI tracking
            
        Returns:
            Dictionary with retrieval results:
            - final_answer: Synthesized answer
            - key_facts: List of key facts with citations
            - residual_uncertainty: Remaining uncertainties
            - citations: Deduplicated top-level citations
            
        Raises:
            HTTPException(400): If question is invalid
            HTTPException(502): If retrieval pipeline fails
        """
        # Validate input
        if not isinstance(question, str) or not question.strip():
            raise HTTPException(
                status_code=400,
                detail="query must be a non-empty string"
            )
        
        logger.info(
            "retrieval.request",
            question_len=len(question),
            top_k=int(top_k or 1),
            project_id=project_id,
            prompt_id=prompt_id,
        )
        
        # Get or create graph (singleton pattern)
        graph = await _get_or_create_graph(
            self._get_session,
            self._get_llm,
            self._get_embedder,
            self._publisher,
        )
        
        # Generate retrieval session ID
        retrieval_id = uuid4()
        
        # Execute graph
        try:
            # Provide stable thread_id for checkpointer
            thread_id = f"{project_id}:{abs(hash(question))}"
            
            state = await graph.ainvoke(
                {
                    "question": question,
                    "top_k": int(top_k or 1),
                    "project_id": project_id,
                    "prompt_id": prompt_id,
                    "retrieval_id": retrieval_id,
                },
                {"configurable": {"thread_id": thread_id}},
            )
            
            result = state.get("result_json") or {}
            
            logger.info(
                "retrieval.response",
                has_result=bool(result),
                keys=len(list(result.keys())),
                retrieval_id=str(retrieval_id),
                prompt_id=prompt_id,
            )
            
            return result
            
        except Exception as exc:
            logger.error(
                "retrieval.execution_failed",
                error=str(exc),
                error_type=type(exc).__name__,
                question=question[:100],
                project_id=project_id,
                retrieval_id=str(retrieval_id),
            )
            raise HTTPException(
                status_code=502,
                detail=f"Retrieval pipeline failed: {exc}"
            )
    
    def clear_graph_cache(self):
        """Clear cached graph instance (useful for testing or reconfiguration)."""
        global _GRAPH_INSTANCE
        _GRAPH_INSTANCE = None
        logger.info("graph_cache_cleared", message="Cleared persistent graph instance")

