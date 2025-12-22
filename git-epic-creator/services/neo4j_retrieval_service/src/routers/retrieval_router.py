from typing import Any, Dict

from fastapi import APIRouter, Depends, HTTPException, Request
from utils.local_auth import get_local_user_verified, LocalUser
from utils.llm_client_factory import create_llm, create_embedder
from pydantic import BaseModel
import structlog

from services.retrieval_service import Neo4jRetrievalService
from services.retrieval_status_publisher import RetrievalStatusPublisher
from retrieval_ms.neo4j_repository_service_client import get_client

logger = structlog.get_logger(__name__)

retrieval_router = APIRouter()


class RetrievalRequest(BaseModel):
    query: str
    top_k: int = 5
    project_id: str
    prompt_id: str | None = None  # Optional parent workflow prompt_id for UI tracking


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
async def retrieve(
    req: RetrievalRequest, 
    request: Request, 
    current_user: LocalUser = Depends(get_local_user_verified)
) -> Dict[str, Any]:
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
        
        # Repository factory (HTTP-backed, no direct Neo4j driver)
        def get_repo_factory():
            return get_client()
        
        service = Neo4jRetrievalService(
            get_repo=get_repo_factory, 
            get_llm=create_llm, 
            get_embedder=create_embedder,
        )
        result = await service.retrieve(
            req.query, 
            top_k=req.top_k, 
            project_id=req.project_id,
            prompt_id=req.prompt_id,
            publisher=publisher,
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

