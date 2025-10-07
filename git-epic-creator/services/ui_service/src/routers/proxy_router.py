"""
API Proxy Router with Enhanced S2S Authentication

This module proxies API requests to downstream services with enhanced S2S JWT tokens
that include comprehensive claims for proper authorization and audit logging.
"""

from __future__ import annotations

from typing import Dict
import time
import structlog
import httpx
from fastapi import APIRouter, Request
from fastapi.responses import JSONResponse, Response

from configuration.common_config import get_app_settings
from utils.jwt_utils import sign_jwt
from services.gitlab_token_manager import get_gitlab_token_manager
from utils.redis_client import get_redis_client
from constants.streams import UI_PROJECT_PROGRESS_CHANNEL

logger = structlog.get_logger(__name__)
router = APIRouter()

# Target path segments for service routing
GITLAB_ROUTE_SEGMENT = "/gitlab/"

# S2S token cache (in-memory, simple implementation)
_s2s_token_cache: Dict[str, tuple[str, int]] = {}


def _preserve_headers(resp: httpx.Response) -> Dict[str, str]:
    """Preserve important headers from upstream response."""
    preserve = {"content-type", "content-disposition", "cache-control", "location"}
    return {k: v for k, v in resp.headers.items() if k.lower() in preserve}


def _identify_target_service(path: str) -> str:
    """
    Identify target service from request path for audience claim.
    
    Args:
        path: Request path
        
    Returns:
        Service identifier for audience claim
    """
    if "/project" in path:
        return "project-service"
    elif "/workflow" in path:
        return "workflow-service"
    elif "/tasks" in path:
        return "tasks-service"
    elif GITLAB_ROUTE_SEGMENT in path:
        return "gitlab-service"
    else:
        return "unknown-service"


def _mint_s2s_token(session: dict, target_service: str) -> str:
    """
    Mint enhanced S2S JWT token with comprehensive claims.
    
    This token includes all necessary claims for downstream services
    to perform proper authorization and audit logging.
    
    Args:
        session: Session dictionary with user claims
        target_service: Target service identifier for audience claim
        
    Returns:
        Signed JWT token
        
    Raises:
        RuntimeError: If JWT secret is not configured
    """
    now = int(time.time())
    
    # Build comprehensive claims
    claims = {
        # Identity claims
        "oid": str(session.get("oid") or ""),
        "tid": str(session.get("tid") or ""),
        "preferred_username": session.get("username"),
        "roles": session.get("roles") or [],
        
        # Token metadata
        "iss": "ui-service",
        "aud": target_service,
        "iat": now,
        "nbf": now,
        "exp": now + 600,  # 10 minutes
        
        # Original Azure token metadata (for correlation)
        "azure_exp": session.get("exp"),
        "azure_iat": session.get("iat"),
    }
    
    # Sign token (uses LOCAL_JWT_SECRET from environment)
    return sign_jwt(claims, expires_in_seconds=0)  # exp already set


async def _get_or_mint_s2s_token(
    session: dict,
    target_service: str,
    cache_ttl: int = 60
) -> str:
    """
    Get cached S2S token or mint new one.
    
    Caches minted tokens per session for short TTL to reduce signing overhead.
    
    Args:
        session: Session dictionary
        target_service: Target service identifier
        cache_ttl: Cache TTL in seconds
        
    Returns:
        S2S JWT token
    """
    session_id = session.get("sid", "")
    cache_key = f"{session_id}:{target_service}"
    
    # Check cache
    if cache_key in _s2s_token_cache:
        cached_token, cached_at = _s2s_token_cache[cache_key]
        if (time.time() - cached_at) < cache_ttl:
            logger.debug("Using cached S2S token", target_service=target_service)
            return cached_token
    
    # Mint new token
    token = _mint_s2s_token(session, target_service)
    
    # Cache it
    _s2s_token_cache[cache_key] = (token, time.time())
    
    logger.debug("Minted new S2S token", target_service=target_service)
    return token


async def _invalidate_s2s_token_cache(session: dict) -> None:
    """
    Invalidate S2S token cache for session.
    
    Called when session changes (e.g., after token refresh).
    
    Args:
        session: Session dictionary
    """
    session_id = session.get("sid", "")
    if session_id:
        # Remove all cached tokens for this session
        keys_to_remove = [k for k in _s2s_token_cache.keys() if k.startswith(f"{session_id}:")]
        for key in keys_to_remove:
            _s2s_token_cache.pop(key, None)
        
        if keys_to_remove:
            logger.debug("Invalidated S2S token cache", session_id=session_id, count=len(keys_to_remove))


async def _forward(
    request: Request,
    target_url: str,
) -> Response:
    """
    Forward request to upstream service with enhanced S2S authentication.
    
    Args:
        request: Incoming request
        target_url: Upstream service URL
        
    Returns:
        Response from upstream service
    """
    # Check authentication
    session_id = request.session.get("sid")
    if not session_id:
        logger.warning("Proxy auth check failed: no session ID")
        return JSONResponse({"detail": "Not authenticated"}, status_code=401)
    
    oid = request.session.get("oid")
    if not oid:
        logger.warning("Proxy auth check failed: no oid in session", session_id=session_id, session_keys=list(request.session.keys()))
        return JSONResponse({"detail": "Invalid session"}, status_code=401)
    
    # Identify target service
    target_service = _identify_target_service(target_url)
    
    # Mint S2S token with enhanced claims
    try:
        s2s_token = await _get_or_mint_s2s_token(
            session=request.session,
            target_service=target_service
        )
        logger.debug("S2S token minted for request", target_service=target_service, oid=oid, username=request.session.get("username"))
    except Exception as e:
        logger.error("Failed to mint S2S token", error=str(e), error_type=type(e).__name__)
        return JSONResponse({"detail": "Server auth not configured"}, status_code=500)
    
    # Build forward headers
    forward_headers: Dict[str, str] = {
        "Authorization": f"Bearer {s2s_token}"
    }
    
    is_gitlab_route = (GITLAB_ROUTE_SEGMENT in (target_url or ""))
    is_tasks_service = (target_service == "tasks-service")
    requires_gitlab_token = is_gitlab_route or is_tasks_service
    
    if requires_gitlab_token:
        try:
            redis_client = getattr(request.app.state, "redis_client", None) or get_redis_client()
            
            gitlab_base_url = getattr(request.app.state, "gitlab_base_url", "")
            client_id = getattr(request.app.state, "gitlab_client_id", "")
            client_secret = getattr(request.app.state, "gitlab_client_secret", "")
            verify_ssl = getattr(request.app.state, "gitlab_verify_ssl", True)
            
            token_manager = await get_gitlab_token_manager(
                session_id=session_id,
                redis_client=redis_client,
                gitlab_base_url=gitlab_base_url,
                client_id=client_id,
                client_secret=client_secret,
                verify_ssl=verify_ssl
            )
            
            gitlab_token = await token_manager.get_valid_token()
            if gitlab_token:
                forward_headers["GitLab-Access-Token"] = gitlab_token
                logger.debug("Added GitLab token to request", target_service=target_service)
            else:
                logger.warning("GitLab token not found for user", session_id=session_id, target_service=target_service)
                return JSONResponse({"detail": "GitLab access token not found. Please connect GitLab account."}, status_code=401)
        except Exception as e:
            logger.error("Failed to get GitLab token", error=str(e), target_service=target_service)
            return JSONResponse({"detail": "GitLab authentication error"}, status_code=500)
    
    # Preserve content headers
    ct = request.headers.get("content-type")
    if ct:
        forward_headers["Content-Type"] = ct
    
    accept = request.headers.get("accept")
    if accept:
        forward_headers["Accept"] = accept
    
    # Get request body
    body = await request.body()
    
    # Forward request to upstream
    try:
        client = getattr(request.app.state, "upstream_http_client", None)
        close_after = False
        if client is None:
            client = httpx.AsyncClient(timeout=60.0)
            close_after = True
        
        try:
            resp = await client.request(
                method=request.method,
                url=target_url,
                content=body if body else None,
                headers=forward_headers,
            )
            
            logger.debug(
                "Proxied request",
                method=request.method,
                target_service=target_service,
                status_code=resp.status_code
            )
            
        finally:
            if close_after:
                await client.aclose()
                
    except httpx.RequestError as exc:
        logger.error("Upstream request failed", error=str(exc), target_url=target_url)
        return JSONResponse({"detail": f"Upstream request failed: {str(exc)}"}, status_code=502)
    
    # Handle 401 Unauthorized from GitLab-related services
    # This indicates the GitLab token is invalid (e.g., after mock service restart)
    if resp.status_code == 401 and requires_gitlab_token:
        try:
            logger.warning(
                "Received 401 from GitLab-related service, clearing GitLab token",
                session_id=session_id,
                target_service=target_service
            )
            
            # Clear the invalid GitLab token from Redis
            # Note: token_manager was already initialized earlier in this function
            await token_manager.clear_token()
            
            logger.info(
                "GitLab token cleared due to 401 response",
                session_id=session_id,
                target_service=target_service
            )
        except Exception as e:
            logger.error(
                "Failed to clear GitLab token after 401",
                session_id=session_id,
                error=str(e)
            )
    
    # Return response
    headers = _preserve_headers(resp)
    return Response(content=resp.content, status_code=resp.status_code, headers=headers)


@router.get("/config")
async def get_ui_config():
    """
    Get UI configuration for client.
    
    Returns API endpoints and channel names for the browser client.
    """
    return JSONResponse({
        "projectManagementApiBase": "/project",
        "aiWorkflowApiBase": "/workflow",
        "aiTasksApiBase": "/tasks",
        "gitlabApiBase": "/gitlab",
        "gitlabAuthStatusPath": "/auth/gitlab/status",
        "gitlabAuthAuthorizePath": "/auth/gitlab/authorize",
        "progressChannel": UI_PROJECT_PROGRESS_CHANNEL,
    })


@router.api_route("/project/{path:path}", methods=["GET", "POST", "PUT", "DELETE", "PATCH", "OPTIONS"])
async def proxy_to_project_management(path: str, request: Request):
    """Proxy requests to project management service."""
    upstream_base = get_app_settings().http_client.PROJECT_MANAGEMENT_SERVICE_URL.rstrip("/")
    target_url = f"{upstream_base}/{path}"
    if request.url.query:
        target_url = f"{target_url}?{request.url.query}"
    return await _forward(request, target_url)


@router.api_route("/workflow/{path:path}", methods=["GET", "POST", "PUT", "DELETE", "PATCH", "OPTIONS"])
async def proxy_to_ai_requirements(path: str, request: Request):
    """Proxy requests to AI requirements service."""
    upstream_base = get_app_settings().http_client.AI_REQUIREMENTS_SERVICE_URL.rstrip("/")
    target_url = f"{upstream_base}/workflow/{path}"
    if request.url.query:
        target_url = f"{target_url}?{request.url.query}"
    return await _forward(request, target_url)


@router.api_route("/tasks/{path:path}", methods=["GET", "POST", "PUT", "DELETE", "PATCH", "OPTIONS"])
async def proxy_to_ai_tasks(path: str, request: Request):
    """Proxy requests to AI tasks/backlog generation service."""
    upstream_base = get_app_settings().http_client.AI_TASKS_SERVICE_URL.rstrip("/")
    target_url = f"{upstream_base}/tasks/{path}"
    if request.url.query:
        target_url = f"{target_url}?{request.url.query}"
    return await _forward(request, target_url)


@router.api_route("/gitlab/{path:path}", methods=["GET", "POST", "PUT", "DELETE", "PATCH", "OPTIONS"])
async def proxy_to_gitlab_client(path: str, request: Request):
    """Proxy requests to GitLab client service."""
    upstream_base = get_app_settings().http_client.GITLAB_CLIENT_SERVICE_URL.rstrip("/")
    target_url = f"{upstream_base}/gitlab/{path}"
    if request.url.query:
        target_url = f"{target_url}?{request.url.query}"
    return await _forward(request, target_url)