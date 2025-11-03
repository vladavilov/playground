from typing import Any, Dict
from contextlib import contextmanager

from fastapi import APIRouter, Depends, HTTPException, Request, Query
from utils.local_auth import get_local_user_verified, LocalUser
from pydantic import BaseModel
import structlog

from services.clients import get_llm, get_embedder, get_neo4j_session
from services.retrieval_service import Neo4jRetrievalService
from services.retrieval_status_publisher import RetrievalStatusPublisher
from retrieval_ms.repositories.neo4j_repository import Neo4jRepository

logger = structlog.get_logger(__name__)

retrieval_router = APIRouter()


class RetrievalRequest(BaseModel):
    query: str
    top_k: int = 1
    project_id: str
    prompt_id: str | None = None  # Optional parent workflow prompt_id for UI tracking


@contextmanager
def _get_neo4j_repository():
    """Context manager for Neo4j repository with session management.
    
    Provides consistent session lifecycle management across endpoints.
    """
    with get_neo4j_session() as session:
        yield Neo4jRepository(session)


def _handle_infrastructure_error(exc: Exception, context: str, **log_context) -> None:
    """Centralized infrastructure error handling.
    
    Args:
        exc: The exception that occurred
        context: Description of the operation that failed
        **log_context: Additional context for logging
    
    Raises:
        HTTPException(500): Always raises with formatted error message
    """
    logger.error(
        f"{context}_infrastructure_failure",
        error=str(exc),
        error_type=type(exc).__name__,
        message=f"Infrastructure failure during {context}",
        **log_context
    )
    raise HTTPException(
        status_code=500,
        detail=f"{context.replace('_', ' ').title()} infrastructure error: {type(exc).__name__}"
    ) from exc

@retrieval_router.post("")
async def retrieve(req: RetrievalRequest, request: Request, current_user: LocalUser = Depends(get_local_user_verified)) -> Dict[str, Any]:
    """Retrieve context from Neo4j graph.
    
    Returns 200 with empty structure if no data found (not an error).
    Returns 500 only for actual infrastructure/connection failures.
    """
    try:
        # Initialize publisher with Redis client from app state
        redis_client = getattr(request.app.state, "redis_client", None)
        publisher = RetrievalStatusPublisher(redis_client) if redis_client else None
        
        if not publisher:
            logger.warning("redis_client_not_found", message="Progress updates will not be published")
        
        service = Neo4jRetrievalService(
            get_session=get_neo4j_session, 
            get_llm=get_llm, 
            get_embedder=get_embedder,
            publisher=publisher
        )
        result = await service.retrieve(
            req.query, 
            top_k=req.top_k, 
            project_id=req.project_id,
            prompt_id=req.prompt_id
        )
        
        # Check if result is empty (no data scenario)
        is_empty = (
            not result.get("final_answer") and 
            not result.get("key_facts")
        )
        
        if is_empty:
            logger.warning(
                "retrieval_no_data_found",
                project_id=req.project_id,
                query_length=len(req.query),
                top_k=req.top_k,
                message="No data found in graph for query - returning empty result with 200"
            )
            # Return structured empty response with metadata flag
            return {
                "final_answer": "",
                "key_facts": [],
                "residual_uncertainty": "",
                "no_data_found": True
            }
        
        return result
        
    except HTTPException:
        # Re-raise HTTP exceptions (e.g., 400, 502 from service layer)
        raise
    except Exception as exc:
        # Infrastructure/connection failures -> 500
        _handle_infrastructure_error(exc, "retrieval", project_id=req.project_id)

