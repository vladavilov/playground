"""Neo4j Retrieval MCP Server - FastMCP entrypoint.

A lightweight MCP facade that enables GitHub Copilot to access the knowledge graph.
Translates MCP protocol requests into HTTP calls to upstream microservices.

OAuth Discovery:
    - VS Code discovers OAuth settings via /.well-known/oauth-protected-resource
    - Authentication via Azure AD (same as UI Service)
    
Tools:
    - resolve_project: Resolves project name to UUID
    - retrieve_context: DRIFT search on Knowledge Graph

Redis Integration:
    - Listens to retrieval progress updates using shared Redis client
    - Republishes as MCP progress notifications to Copilot via Context
"""

import asyncio
import json
import sys
import uuid
from collections.abc import AsyncIterator
from contextlib import asynccontextmanager
from dataclasses import dataclass, field
from typing import Any

import httpx
import structlog
import uvicorn

from starlette.applications import Starlette
from starlette.middleware import Middleware
from starlette.middleware.base import BaseHTTPMiddleware
from starlette.middleware.cors import CORSMiddleware
from starlette.requests import Request
from starlette.responses import JSONResponse, Response

from fastmcp import FastMCP, Context

# Import shared library components
from configuration.logging_config import configure_logging
from constants.streams import UI_RETRIEVAL_PROGRESS_CHANNEL
from utils.redis_client import create_redis_client

# Import local modules
from config import (
    get_mcp_settings, 
    get_project_management_url, 
    get_retrieval_service_url,
    get_oauth_discovery_metadata,
    get_authorization_server_metadata
)
from adapter import ProjectManagementAdapter, RetrievalServiceAdapter
from auth import MCPAuthHandler


class OAuthAuthenticationMiddleware(BaseHTTPMiddleware):
    """
    Middleware that enforces Bearer token authentication on MCP endpoints.
    
    Returns HTTP 401 with WWW-Authenticate header to trigger VS Code's OAuth flow:
    1. VS Code receives 401 Unauthorized
    2. Reads WWW-Authenticate header for resource metadata URL
    3. Queries /.well-known/oauth-protected-resource
    4. Starts OAuth flow with discovered authorization server
    """
    
    # Endpoints that don't require authentication (or handle it internally)
    PUBLIC_PATHS = {
        "/.well-known/oauth-protected-resource",
        "/.well-known/oauth-authorization-server",
        "/health",
        "/userinfo",  # Handles own authentication for VS Code user discovery
    }
    
    async def dispatch(self, request: Request, call_next) -> Response:
        path = request.url.path
        
        # Allow public endpoints without authentication
        if path in self.PUBLIC_PATHS:
            return await call_next(request)
        
        # Check for Bearer token on MCP endpoints
        auth_header = request.headers.get("Authorization", "")
        if not auth_header.startswith("Bearer "):
            mcp_settings = get_mcp_settings()
            resource_metadata_url = f"{mcp_settings.MCP_SERVER_URL}/.well-known/oauth-protected-resource"
            
            logger.debug("No Bearer token provided, returning 401", path=path)
            return Response(
                content='{"error": "unauthorized", "message": "Authentication required"}',
                status_code=401,
                media_type="application/json",
                headers={
                    "WWW-Authenticate": f'Bearer resource="{resource_metadata_url}"'
                }
            )
        
        return await call_next(request)

# Configure logging using shared configuration
configure_logging()
logger = structlog.get_logger(__name__)


@dataclass
class ProgressUpdate:
    """Represents a progress update from the retrieval service."""
    phase: str
    progress_pct: float
    message: str = ""
    timestamp: float = field(default_factory=lambda: 0.0)


# Shared progress state: maps prompt_id -> latest ProgressUpdate
# Used to correlate Redis progress messages with in-flight tool calls
_progress_state: dict[str, ProgressUpdate] = {}
_progress_lock = asyncio.Lock()

# Global state for adapters, Redis, and auth
_project_adapter: ProjectManagementAdapter | None = None
_retrieval_adapter: RetrievalServiceAdapter | None = None
_redis_client = None
_redis_listener_task: asyncio.Task | None = None
_auth_handler: MCPAuthHandler | None = None


async def get_auth_handler() -> MCPAuthHandler:
    """Get or create MCP auth handler."""
    global _auth_handler
    if _auth_handler is None:
        _auth_handler = MCPAuthHandler()
    return _auth_handler


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


# Progress state management functions
async def update_progress_state(prompt_id: str, update: ProgressUpdate) -> None:
    """Store a progress update for a given prompt_id."""
    async with _progress_lock:
        _progress_state[prompt_id] = update


async def get_progress_state(prompt_id: str) -> ProgressUpdate | None:
    """Get the latest progress update for a given prompt_id."""
    async with _progress_lock:
        return _progress_state.get(prompt_id)


async def clear_progress_state(prompt_id: str) -> None:
    """Remove progress state for a completed prompt_id."""
    async with _progress_lock:
        _progress_state.pop(prompt_id, None)


# Redis listener for progress updates
async def redis_progress_listener() -> None:
    """
    Listen to Redis pub/sub for retrieval progress updates.
    
    Uses shared constants for channel name (UI_RETRIEVAL_PROGRESS_CHANNEL).
    Stores progress in shared state for correlation with in-flight tool calls.
    The retrieve_context tool polls this state and reports progress to MCP clients.
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
                    
                    # Extract prompt_id for correlation with tool calls
                    prompt_id = data.get("prompt_id")
                    if prompt_id:
                        # Store progress update for the in-flight tool call
                        phase = data.get("phase", "processing")
                        progress_pct = data.get("progress_pct", 0)
                        message_text = data.get("message", f"Phase: {phase}")
                        
                        update = ProgressUpdate(
                            phase=phase,
                            progress_pct=progress_pct,
                            message=message_text
                        )
                        await update_progress_state(prompt_id, update)
                        
                        logger.debug(
                            "Progress update stored",
                            prompt_id=prompt_id,
                            phase=phase,
                            progress_pct=progress_pct
                        )
                    else:
                        # Log progress without prompt_id for debugging
                        logger.debug(
                            "Progress update received (no prompt_id)",
                            phase=data.get("phase"),
                            progress_pct=data.get("progress_pct"),
                            project_id=data.get("project_id")
                        )
                    
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


# Create FastMCP server (lifespan managed in create_http_app for HTTP transport)
mcp = FastMCP(
    name="Neo4j Retrieval MCP Server",
    instructions="""
This MCP server provides access to a Knowledge Graph for technical context retrieval.

Workflow:
1. First, use resolve_project to get the project UUID from a project name
2. Then, use retrieve_context with the project_id to search the Knowledge Graph

The retrieve_context tool uses DRIFT search (Microsoft Research, 2024) to find
relevant technical context, requirements, and citations from ingested documentation.

Authentication:
- This server requires OAuth authentication via Azure AD
- VS Code will automatically handle the OAuth flow
- Your Azure AD token will be exchanged for a backend service token
""",
)


@mcp.tool
async def resolve_project(project_name: str, ctx: Context) -> dict[str, Any]:
    """
    Resolve a project name to a specific Project UUID.
    
    Use this tool first to convert a human-readable project name (e.g., "billing system",
    "risk analytics") into the technical UUID required for retrieve_context.
    
    Args:
        project_name: Name of the project (e.g., "billing", "risk analytics")
        ctx: MCP context for authentication
        
    Returns:
        On success (exactly one match):
            {"success": true, "project_id": "uuid", "project_name": "Official Name"}
            
        On ambiguous results (multiple matches):
            {"success": false, "error": "ambiguous_results", "message": "...", "matches": [...]}
            
        On not found (zero matches):
            {"success": false, "error": "not_found", "message": "..."}
    """
    logger.info("resolve_project called", project_name=project_name)
    
    # Get authentication token from MCP context (token exchange: Azure AD -> internal JWT)
    auth_handler = await get_auth_handler()
    auth_token = await auth_handler.get_auth_token(ctx)
    
    adapter = await get_project_adapter()
    result = await adapter.search_projects(project_name, auth_token=auth_token)
    
    logger.info(
        "resolve_project completed",
        project_name=project_name,
        success=result.get("success", False)
    )
    
    return result


async def _progress_reporter(
    prompt_id: str,
    ctx: Context,
    stop_event: asyncio.Event
) -> None:
    """
    Background task that polls progress state and reports to MCP client.
    
    Runs until stop_event is set, polling every 500ms for progress updates.
    Reports progress to the client via ctx.report_progress().
    """
    last_progress = -1.0
    
    try:
        while not stop_event.is_set():
            update = await get_progress_state(prompt_id)
            
            if update and update.progress_pct != last_progress:
                # Report progress to the MCP client
                await ctx.report_progress(
                    progress=update.progress_pct,
                    total=100.0,
                    message=update.message or f"Phase: {update.phase}"
                )
                last_progress = update.progress_pct
                
                logger.debug(
                    "Progress reported to client",
                    prompt_id=prompt_id,
                    progress=update.progress_pct,
                    phase=update.phase
                )
            
            # Poll every 500ms
            try:
                await asyncio.wait_for(stop_event.wait(), timeout=0.5)
            except asyncio.TimeoutError:
                pass  # Continue polling
                
    except asyncio.CancelledError:
        pass
    except Exception as e:
        logger.warning(
            "Progress reporter error",
            prompt_id=prompt_id,
            error=str(e)
        )


@mcp.tool
async def retrieve_context(
    query: str,
    project_id: str,
    ctx: Context,
    top_k: int = 5
) -> dict[str, Any]:
    """
    Retrieve technical context from the Knowledge Graph using DRIFT search.
    
    This tool searches the ingested documentation and code for a project,
    returning synthesized answers with citations to source documents.
    Progress updates are streamed to the client during retrieval.
    
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
    
    # Get authentication token from MCP context (token exchange: Azure AD -> internal JWT)
    auth_handler = await get_auth_handler()
    auth_token = await auth_handler.get_auth_token(ctx)
    
    # Report initial progress
    await ctx.info(f"Starting knowledge graph retrieval for project {project_id}")
    await ctx.report_progress(progress=0, total=100, message="Initializing DRIFT search...")
    
    adapter = await get_retrieval_adapter()
    
    # Generate a prompt_id for progress tracking
    prompt_id = str(uuid.uuid4())
    
    # Start background progress reporter
    stop_event = asyncio.Event()
    progress_task = asyncio.create_task(
        _progress_reporter(prompt_id, ctx, stop_event)
    )
    
    try:
        result = await adapter.retrieve(
            query=query,
            project_id=project_id,
            top_k=top_k,
            prompt_id=prompt_id,
            auth_token=auth_token
        )
    finally:
        # Stop progress reporter and cleanup
        stop_event.set()
        progress_task.cancel()
        try:
            await progress_task
        except asyncio.CancelledError:
            pass
        
        # Clean up progress state
        await clear_progress_state(prompt_id)
    
    # Report completion
    await ctx.report_progress(progress=100, total=100, message="Retrieval complete")
    
    logger.info(
        "retrieve_context completed",
        project_id=project_id,
        has_answer=bool(result.get("final_answer")),
        no_data_found=result.get("no_data_found", False)
    )
    
    if result.get("no_data_found"):
        await ctx.warning(
            f"No data found for project {project_id}. "
            "The project may not have ingested data yet."
        )
    else:
        fact_count = len(result.get("key_facts", []))
        await ctx.info(f"Found {fact_count} relevant facts with citations")
    
    return result


# Custom HTTP routes (using FastMCP's @custom_route decorator)
@mcp.custom_route("/.well-known/oauth-protected-resource", methods=["GET"])
async def oauth_protected_resource_metadata(request: Request) -> JSONResponse:
    """
    OAuth 2.0 Protected Resource Metadata endpoint (RFC 9728).
    
    VS Code queries this endpoint to discover:
    - authorization_servers: Where to authenticate
    - authorization_endpoint: Explicit Azure AD authorization URL
    - token_endpoint: Explicit Azure AD token URL
    - scopes_supported: What scopes are needed
    - bearer_methods_supported: How to send the token
    - userinfo_endpoint: Where to get user info
    - client_id: Azure AD application client ID
    """
    metadata = get_oauth_discovery_metadata()
    logger.info("OAuth protected resource metadata requested")
    return JSONResponse(metadata)


@mcp.custom_route("/.well-known/oauth-authorization-server", methods=["GET"])
async def oauth_authorization_server_metadata(request: Request) -> JSONResponse:
    """
    OAuth 2.0 Authorization Server Metadata endpoint (RFC 8414).
    
    VS Code MCP client may query this endpoint to discover full OAuth
    authorization server configuration. Returns Azure AD metadata with
    MCP-specific extensions.
    
    Returns:
    - issuer: Azure AD v2.0 issuer URL
    - authorization_endpoint: Azure AD authorization URL
    - token_endpoint: Azure AD token URL
    - jwks_uri: Azure AD JWKS URL for token verification
    - response_types_supported: Supported OAuth response types
    - grant_types_supported: Supported OAuth grant types
    - scopes_supported: Available OAuth scopes
    - userinfo_endpoint: MCP server's userinfo endpoint
    - client_id: Azure AD application client ID
    """
    metadata = get_authorization_server_metadata()
    logger.info("OAuth authorization server metadata requested")
    return JSONResponse(metadata)


@mcp.custom_route("/health", methods=["GET"])
async def health_check(request: Request) -> JSONResponse:
    """
    Check the health of the MCP server and its upstream dependencies.
    
    Returns JSON with status and upstream service connectivity.
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
    
    return JSONResponse({
        "status": overall_status,
        "upstream": {
            "project_management_service": pms_status,
            "retrieval_service": retrieval_status
        }
    })


@mcp.custom_route("/userinfo", methods=["GET"])
async def userinfo_endpoint(request: Request) -> Response:
    """
    OpenID Connect UserInfo endpoint for VS Code user discovery.
    
    VS Code's MCP client queries this endpoint to retrieve authenticated user
    details after OAuth flow completion.
    
    Flow:
    1. Extracts Bearer token from Authorization header (Azure AD token)
    2. Exchanges Azure AD token for LOCAL JWT via authentication service
    3. Returns user identity in OpenID Connect format
    
    Returns:
        200: User info in OIDC format
        401: Authentication required (no or invalid token)
    """
    # Extract Bearer token from Authorization header
    auth_header = request.headers.get("Authorization", "")
    if not auth_header.startswith("Bearer "):
        logger.debug("No Bearer token in userinfo request")
        return Response(
            content='{"error": "unauthorized", "message": "Bearer token required"}',
            status_code=401,
            media_type="application/json",
            headers={"WWW-Authenticate": "Bearer"}
        )
    
    azure_token = auth_header[7:]  # Strip "Bearer " prefix
    
    # Get auth handler and exchange token
    auth_handler = await get_auth_handler()
    
    # Exchange Azure AD token for LOCAL JWT and get user context
    _, auth_ctx = await auth_handler._exchange_token(None, azure_token)
    
    if not auth_ctx:
        logger.warning("Token exchange failed in userinfo endpoint")
        return Response(
            content='{"error": "invalid_token", "message": "Token exchange failed"}',
            status_code=401,
            media_type="application/json",
            headers={"WWW-Authenticate": 'Bearer error="invalid_token"'}
        )
    
    # Return user info in OpenID Connect format
    userinfo = {
        "sub": auth_ctx.user_id,
        "preferred_username": auth_ctx.username,
        "email": auth_ctx.username,  # Username is typically email
        "roles": auth_ctx.roles,
    }
    
    logger.info("Userinfo returned", user_id=auth_ctx.user_id)
    return JSONResponse(userinfo)


@asynccontextmanager
async def app_lifespan(app: Any) -> AsyncIterator[None]:
    """
    Lifespan context manager for MCP server resources.
    
    Handles startup and shutdown of:
    - MCP auth handler for authentication
    - Redis pub/sub listener for progress updates
    - HTTP adapters for upstream services
    
    Args:
        app: ASGI application instance (Starlette or any app-like object for stdio)
    """
    global _redis_listener_task, _auth_handler
    
    logger.info("MCP Server starting up")
    
    # Initialize auth handler
    _auth_handler = MCPAuthHandler()
    logger.info("MCP auth handler initialized")
    
    # Start Redis listener in background
    _redis_listener_task = asyncio.create_task(redis_progress_listener())
    
    try:
        yield
    finally:
        logger.info("MCP Server shutting down")
        
        # Cancel Redis listener
        if _redis_listener_task:
            _redis_listener_task.cancel()
            try:
                await _redis_listener_task
            except asyncio.CancelledError:
                pass
        
        # Close auth handler
        if _auth_handler:
            await _auth_handler.close()
        
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


def create_http_app():
    """
    Create ASGI app with MCP server and custom routes.
    
    Uses FastMCP's http_app() with custom routes defined via @mcp.custom_route.
    Per FastMCP docs: mcp_app.lifespan MUST be passed to the parent ASGI app.
    
    Middleware (in order):
    - CORS: Allow cross-origin requests
    - OAuthAuthentication: Return 401 with WWW-Authenticate for unauthenticated MCP requests
    
    Routes (via @mcp.custom_route):
    - /.well-known/oauth-protected-resource: OAuth discovery (RFC 9728)
    - /health: Health check with upstream status
    - /mcp/*: MCP protocol endpoints
    
    Returns:
        Starlette application ready for uvicorn
    """
    # Middleware stack (order matters: CORS first, then auth)
    middleware = [
        Middleware(
            CORSMiddleware,
            allow_origins=["*"],
            allow_credentials=True,
            allow_methods=["*"],
            allow_headers=["*"],
        ),
        Middleware(OAuthAuthenticationMiddleware),
    ]
    
    # Create MCP ASGI app with custom routes and middleware
    mcp_app = mcp.http_app(path="/mcp", middleware=middleware)
    
    # Combine app lifespan with MCP's lifespan (critical for session manager)
    @asynccontextmanager
    async def combined_lifespan(starlette_app: Starlette) -> AsyncIterator[None]:
        async with app_lifespan(starlette_app):
            async with mcp_app.lifespan(starlette_app):
                yield
    
    # Create Starlette wrapper with combined lifespan
    app = Starlette(lifespan=combined_lifespan)
    app.mount("/", mcp_app)
    
    return app


async def run_stdio_with_lifespan() -> None:
    """Run stdio transport with proper resource initialization."""
    async with app_lifespan(None):
        await mcp.run_async(transport="stdio")


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
        
        # Create Starlette app with OAuth discovery + MCP
        http_app = create_http_app()
        uvicorn.run(http_app, host="0.0.0.0", port=mcp_settings.PORT)
    else:
        logger.info("Starting MCP server with stdio transport")
        asyncio.run(run_stdio_with_lifespan())
