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


@retrieval_router.get("/project/{project_id}/graph")
async def get_project_graph(
    project_id: str,
    limit: int = Query(default=500, ge=1, le=1000, description="Maximum number of nodes to return"),
    current_user: LocalUser = Depends(get_local_user_verified)
) -> Dict[str, Any]:
    """Get complete project graph for visualization.
    
    Retrieves all nodes and relationships related to a specific project,
    formatted for graph visualization libraries like Neovis.js.
    
    Args:
        project_id: Project UUID
        limit: Maximum number of nodes per type to return (1-1000, default 500)
        current_user: Authenticated user from JWT
    
    Returns:
        {
            "nodes": [...],
            "relationships": [...],
            "stats": {
                "node_count": 150,
                "relationship_count": 320,
                "node_types": {...}
            }
        }
    
    Returns 200 with empty structure if no data found (not an error).
    Returns 500 only for actual infrastructure/connection failures.
    
    Raises:
        HTTPException(400): Invalid project_id format
        HTTPException(500): Neo4j connection or query failure
    """
    try:
        # Validate project_id is non-empty
        if not project_id or not project_id.strip():
            raise HTTPException(
                status_code=400,
                detail="project_id must be a non-empty string"
            )
        
        logger.info(
            "fetching_project_graph",
            project_id=project_id,
            limit=limit,
            user_id=current_user.oid,
            message="Fetching graph data for visualization"
        )
        
        # Use consistent repository pattern with context manager
        with _get_neo4j_repository() as repo:
            graph_data = repo.get_project_graph(project_id, limit)
        
        # Check if any data was found (consistent with retrieve endpoint pattern)
        is_empty = not graph_data or graph_data.get("stats", {}).get("node_count", 0) == 0
        
        if is_empty:
            logger.warning(
                "project_graph_empty",
                project_id=project_id,
                message="No graph data found for project - returning empty result with 200"
            )
            # Return 200 with empty graph (consistent with retrieve endpoint pattern)
            return {
                "nodes": [],
                "relationships": [],
                "stats": {
                    "node_count": 0,
                    "relationship_count": 0,
                    "node_types": {},
                    "message": "No graph data found. Project may not have processed documents yet."
                }
            }
        
        logger.info(
            "project_graph_fetched",
            project_id=project_id,
            node_count=graph_data["stats"]["node_count"],
            relationship_count=graph_data["stats"]["relationship_count"],
            node_types=graph_data["stats"]["node_types"],
            message="Successfully fetched project graph"
        )
        
        return graph_data
        
    except HTTPException:
        # Re-raise HTTP exceptions (consistent pattern)
        raise
    except Exception as exc:
        # Infrastructure/connection failures -> 500 (consistent pattern)
        _handle_infrastructure_error(exc, "project_graph", project_id=project_id)

