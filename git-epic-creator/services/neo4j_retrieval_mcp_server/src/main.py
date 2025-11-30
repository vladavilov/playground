"""Neo4j Retrieval MCP Server - FastMCP entrypoint.

A lightweight MCP facade that enables GitHub Copilot to access the knowledge graph.
Translates MCP protocol requests into HTTP calls to upstream microservices.

Tools:
    - resolve_project: Resolves project name to UUID
    - retrieve_context: DRIFT search on Knowledge Graph

Redis Integration:
    - Listens to retrieval progress updates using shared Redis client
    - Republishes as MCP progress notifications to Copilot
"""

import asyncio
import json
import os
import sys
import uuid
from contextlib import asynccontextmanager
from typing import Any

import httpx
import structlog

from fastmcp import FastMCP

# Import shared library components
from configuration.logging_config import configure_logging
from configuration.common_config import get_app_settings
from constants.streams import UI_RETRIEVAL_PROGRESS_CHANNEL
from utils.redis_client import create_redis_client

# Import local modules
from config import get_mcp_settings, get_project_management_url, get_retrieval_service_url
from adapter import ProjectManagementAdapter, RetrievalServiceAdapter

# Configure logging using shared configuration
configure_logging()
logger = structlog.get_logger(__name__)

# Global state for adapters and Redis
_project_adapter: ProjectManagementAdapter | None = None
_retrieval_adapter: RetrievalServiceAdapter | None = None
_redis_client = None
_redis_listener_task: asyncio.Task | None = None


async def get_project_adapter() -> ProjectManagementAdapter:
    """Get or create project management adapter."""
    global _project_adapter
    if _project_adapter is None:
        _project_adapter = ProjectManagementAdapter()
    return _project_adapter


async def get_retrieval_adapter() -> RetrievalServiceAdapter:
    """Get or create retrieval service adapter."""
    global _retrieval_adapter
    if _retrieval_adapter is None:
        _retrieval_adapter = RetrievalServiceAdapter()
    return _retrieval_adapter


async def get_redis_pubsub_client():
    """Get or create Redis client for pub/sub using shared utilities."""
    global _redis_client
    if _redis_client is None:
        try:
            # Use shared Redis client factory with pub/sub configuration
            _redis_client = create_redis_client(is_pubsub_client=True)
            await _redis_client.ping()
            logger.info("Redis pub/sub client connected")
        except Exception as e:
            logger.warning(
                "Redis connection failed - progress updates disabled",
                error=str(e)
            )
            _redis_client = None
    return _redis_client


# Create FastMCP server
mcp = FastMCP(
    name="Neo4j Retrieval MCP Server",
    instructions="""
This MCP server provides access to a Knowledge Graph for technical context retrieval.

Workflow:
1. First, use resolve_project to get the project UUID from a project name
2. Then, use retrieve_context with the project_id to search the Knowledge Graph

The retrieve_context tool uses DRIFT search (Microsoft Research, 2024) to find
relevant technical context, requirements, and citations from ingested documentation.
""",
)


@mcp.tool
async def resolve_project(project_name: str) -> dict[str, Any]:
    """
    Resolve a project name to a specific Project UUID.
    
    Use this tool first to convert a human-readable project name (e.g., "billing system",
    "risk analytics") into the technical UUID required for retrieve_context.
    
    Args:
        project_name: Name of the project (e.g., "billing", "risk analytics")
        
    Returns:
        On success (exactly one match):
            {"success": true, "project_id": "uuid", "project_name": "Official Name"}
            
        On ambiguous results (multiple matches):
            {"success": false, "error": "ambiguous_results", "message": "...", "matches": [...]}
            
        On not found (zero matches):
            {"success": false, "error": "not_found", "message": "..."}
    """
    logger.info("resolve_project called", project_name=project_name)
    
    adapter = await get_project_adapter()
    
    # TODO: Extract auth token from MCP context when available
    result = await adapter.search_projects(project_name, auth_token=None)
    
    logger.info(
        "resolve_project completed",
        project_name=project_name,
        success=result.get("success", False)
    )
    
    return result


@mcp.tool
async def retrieve_context(
    query: str,
    project_id: str,
    top_k: int = 5
) -> dict[str, Any]:
    """
    Retrieve technical context from the Knowledge Graph using DRIFT search.
    
    This tool searches the ingested documentation and code for a project,
    returning synthesized answers with citations to source documents.
    
    IMPORTANT: You must first call resolve_project to get the project_id.
    
    Args:
        query: The technical question to answer (e.g., "How does authentication work?")
        project_id: The UUID returned by resolve_project (REQUIRED)
        top_k: Number of results to consider (default: 5)
        
    Returns:
        {
            "final_answer": "Synthesized answer to your question",
            "key_facts": [
                {
                    "fact": "A specific finding",
                    "citations": [
                        {"chunk_id": "uuid", "document_name": "file.md", "span": "relevant text"}
                    ]
                }
            ],
            "residual_uncertainty": "What couldn't be answered",
            "no_data_found": false
        }
        
    Note:
        If no_data_found is true, the project may not have ingested data yet,
        or the query didn't match any relevant content.
    """
    logger.info(
        "retrieve_context called",
        query=query[:100],  # Truncate for logging
        project_id=project_id,
        top_k=top_k
    )
    
    adapter = await get_retrieval_adapter()
    
    # Generate a prompt_id for progress tracking
    prompt_id = str(uuid.uuid4())
    
    # TODO: Extract auth token from MCP context when available
    result = await adapter.retrieve(
        query=query,
        project_id=project_id,
        top_k=top_k,
        prompt_id=prompt_id,
        auth_token=None
    )
    
    logger.info(
        "retrieve_context completed",
        project_id=project_id,
        has_answer=bool(result.get("final_answer")),
        no_data_found=result.get("no_data_found", False)
    )
    
    return result


# Redis listener for progress updates
async def redis_progress_listener() -> None:
    """
    Listen to Redis pub/sub for retrieval progress updates.
    
    Uses shared constants for channel name (UI_RETRIEVAL_PROGRESS_CHANNEL).
    Republishes progress to MCP clients (Copilot) as notifications.
    """
    redis_client = await get_redis_pubsub_client()
    
    if redis_client is None:
        logger.warning("Redis not available - progress listener disabled")
        return
    
    try:
        pubsub = redis_client.pubsub()
        await pubsub.subscribe(UI_RETRIEVAL_PROGRESS_CHANNEL)
        
        logger.info(
            "Redis progress listener started",
            channel=UI_RETRIEVAL_PROGRESS_CHANNEL
        )
        
        async for message in pubsub.listen():
            if message["type"] == "message":
                try:
                    # Decode bytes if necessary (pub/sub client uses binary)
                    data_raw = message["data"]
                    if isinstance(data_raw, bytes):
                        data_raw = data_raw.decode("utf-8")
                    data = json.loads(data_raw)
                    
                    # Log progress for debugging
                    logger.debug(
                        "Progress update received",
                        phase=data.get("phase"),
                        progress_pct=data.get("progress_pct"),
                        project_id=data.get("project_id")
                    )
                    
                    # TODO: When FastMCP supports progress notifications,
                    # republish this to the MCP client
                    # For now, we just log it
                    
                except json.JSONDecodeError as e:
                    logger.warning(
                        "Failed to parse progress message",
                        error=str(e)
                    )
                    
    except asyncio.CancelledError:
        logger.info("Redis progress listener cancelled")
        raise
    except Exception as e:
        logger.error(
            "Redis progress listener error",
            error=str(e),
            error_type=type(e).__name__
        )


@asynccontextmanager
async def lifespan(app):
    """Lifespan context manager for resource management."""
    global _redis_listener_task
    
    logger.info("MCP Server starting up")
    
    # Start Redis listener in background
    _redis_listener_task = asyncio.create_task(redis_progress_listener())
    
    try:
        yield {}
    finally:
        # Cleanup
        logger.info("MCP Server shutting down")
        
        # Cancel Redis listener
        if _redis_listener_task:
            _redis_listener_task.cancel()
            try:
                await _redis_listener_task
            except asyncio.CancelledError:
                pass
        
        # Close adapters
        if _project_adapter:
            await _project_adapter.close()
        if _retrieval_adapter:
            await _retrieval_adapter.close()
        
        # Close Redis
        if _redis_client:
            try:
                await _redis_client.aclose()
            except Exception:
                pass
        
        logger.info("MCP Server shutdown complete")


# Apply lifespan to MCP server
mcp.settings.lifespan = lifespan


# Health check endpoint (for HTTP transport)
@mcp.tool
async def health_check() -> dict[str, Any]:
    """
    Check the health of the MCP server and its upstream dependencies.
    
    Returns:
        {
            "status": "healthy" | "degraded",
            "upstream": {
                "project_management_service": "connected" | "disconnected",
                "retrieval_service": "connected" | "disconnected"
            }
        }
    """
    pms_url = get_project_management_url()
    retrieval_url = get_retrieval_service_url()
    
    # Check project management service
    pms_status = "disconnected"
    try:
        async with httpx.AsyncClient(timeout=5.0) as client:
            resp = await client.get(f"{pms_url}/health")
            if resp.status_code == 200:
                pms_status = "connected"
    except Exception:
        pass
    
    # Check retrieval service
    retrieval_status = "disconnected"
    try:
        async with httpx.AsyncClient(timeout=5.0) as client:
            resp = await client.get(f"{retrieval_url}/health")
            if resp.status_code == 200:
                retrieval_status = "connected"
    except Exception:
        pass
    
    overall_status = "healthy" if (
        pms_status == "connected" and retrieval_status == "connected"
    ) else "degraded"
    
    return {
        "status": overall_status,
        "upstream": {
            "project_management_service": pms_status,
            "retrieval_service": retrieval_status
        }
    }


if __name__ == "__main__":
    # Get MCP-specific settings
    mcp_settings = get_mcp_settings()
    
    # Determine transport from environment or command line
    transport = mcp_settings.MCP_TRANSPORT
    
    if len(sys.argv) > 1:
        if "--http" in sys.argv:
            transport = "http"
        elif "--stdio" in sys.argv:
            transport = "stdio"
    
    if transport == "http":
        logger.info("Starting MCP server with HTTP transport", port=mcp_settings.PORT)
        mcp.run(transport="http", host="0.0.0.0", port=mcp_settings.PORT)
    else:
        logger.info("Starting MCP server with stdio transport")
        mcp.run(transport="stdio")
