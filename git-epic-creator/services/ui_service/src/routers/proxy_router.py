"""
API Proxy Router - Forwards requests to backend services with LOCAL JWT.

Uses the LOCAL JWT from session (obtained via authentication-service after Azure AD login).
This is the SAME LOCAL JWT format used by MCP Server (VS Code Copilot).
"""

from __future__ import annotations

import time
from typing import Dict

import httpx
import structlog
from configuration.common_config import get_app_settings
from constants.streams import UI_PROJECT_PROGRESS_CHANNEL
from fastapi import APIRouter, Request
from fastapi.responses import JSONResponse, Response

logger = structlog.get_logger(__name__)
router = APIRouter()

# LOCAL JWT refresh buffer (refresh 60 seconds before expiry)
LOCAL_JWT_REFRESH_BUFFER = 60


def _preserve_headers(resp: httpx.Response) -> Dict[str, str]:
    """Preserve important headers from upstream response."""
    preserve = {"content-type", "content-disposition", "cache-control", "location"}
    return {k: v for k, v in resp.headers.items() if k.lower() in preserve}


async def _get_valid_local_jwt(request: Request) -> str | None:
    """
    Get valid LOCAL JWT from session, refreshing if needed.
    
    The LOCAL JWT was obtained via authentication-service after Azure AD login.
    If it's expired, we refresh via MSAL + authentication-service.
    """
    local_jwt = request.session.get("local_jwt")
    local_jwt_exp = request.session.get("local_jwt_exp", 0)
    
    # Check if LOCAL JWT is still valid
    if local_jwt and local_jwt_exp > time.time() + LOCAL_JWT_REFRESH_BUFFER:
        return local_jwt
    
    # LOCAL JWT expired or about to expire - need to refresh
    session_id = request.session.get("sid")
    if not session_id:
        return None
    
    # Import here to avoid circular imports
    from routers.auth_router import _get_msal_app, _exchange_for_local_jwt
    from configuration.azure_auth_config import get_azure_auth_settings
    
    try:
        msal_app = await _get_msal_app(request, session_id=session_id, load_cache=True)
        if not msal_app:
            logger.warning("Failed to get MSAL app for token refresh")
            return None
        
        accounts = msal_app.get_accounts()
        if not accounts:
            logger.warning("No MSAL accounts found for token refresh")
            return None
        
        azure_settings = getattr(request.app.state, "azure_auth_settings", None) or get_azure_auth_settings()
        scopes = [azure_settings.SCOPE_NAME]
        
        # Get fresh Azure AD token
        result = msal_app.acquire_token_silent(scopes=scopes, account=accounts[0])
        
        if not result or "access_token" not in result:
            logger.warning("MSAL silent token acquisition failed")
            return None
        
        # Exchange Azure AD token for LOCAL JWT via authentication-service
        http_client = getattr(request.app.state, "upstream_http_client", None)
        exchange_result = await _exchange_for_local_jwt(result["access_token"], http_client)
        
        if exchange_result:
            # Update session with new LOCAL JWT
            request.session["local_jwt"] = exchange_result["access_token"]
            request.session["local_jwt_exp"] = int(time.time()) + exchange_result["expires_in"]
            
            logger.debug("LOCAL JWT refreshed successfully")
            return exchange_result["access_token"]
        
        logger.warning("Token exchange failed during refresh")
        return None
        
    except Exception as e:
        logger.error("Failed to refresh LOCAL JWT", error=str(e))
        return None


async def _forward(
    request: Request,
    target_url: str,
) -> Response:
    """
    Forward request to upstream service with LOCAL JWT authentication.
    
    Uses the same LOCAL JWT format as MCP Server (VS Code Copilot).
    """
    # Check authentication
    session_id = request.session.get("sid")
    if not session_id:
        logger.warning("Proxy auth check failed: no session ID")
        return JSONResponse({"detail": "Not authenticated"}, status_code=401)

    oid = request.session.get("oid")
    if not oid:
        logger.warning("Proxy auth check failed: no oid in session")
        return JSONResponse({"detail": "Invalid session"}, status_code=401)

    # Get valid LOCAL JWT (refresh if needed)
    local_jwt = await _get_valid_local_jwt(request)
    
    if not local_jwt:
        logger.warning("Proxy auth check failed: no valid LOCAL JWT")
        return JSONResponse({"detail": "Session expired, please re-authenticate"}, status_code=401)

    # Build forward headers with LOCAL JWT
    forward_headers: Dict[str, str] = {
        "Authorization": f"Bearer {local_jwt}"
    }

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
                target_url=target_url,
                status_code=resp.status_code
            )

        finally:
            if close_after:
                await client.aclose()

    except httpx.TimeoutException as exc:
        logger.error(
            "Upstream request timeout",
            error=str(exc),
            target_url=target_url
        )
        return JSONResponse({"detail": "Upstream service timeout"}, status_code=504)
    except httpx.RequestError as exc:
        logger.error(
            "Upstream connection failed",
            error=str(exc),
            target_url=target_url
        )
        return JSONResponse(
            {"detail": f"Upstream connection failed: {type(exc).__name__}"},
            status_code=502
        )

    # Return response
    headers = _preserve_headers(resp)
    return Response(content=resp.content, status_code=resp.status_code, headers=headers)


@router.get("/config")
async def get_ui_config():
    """Get UI configuration for client."""
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


@router.api_route("/auth/gitlab/{path:path}", methods=["GET", "POST"])
async def proxy_gitlab_auth(request: Request, path: str):
    """Proxy GitLab OAuth endpoints to gitlab-client-service."""
    gitlab_client_url = get_app_settings().http_client.GITLAB_CLIENT_SERVICE_URL.rstrip("/")
    target_url = f"{gitlab_client_url}/auth/gitlab/{path}"
    
    if path == "authorize":
        from urllib.parse import urlencode
        query_params = dict(request.query_params)
        query_params["user_id"] = request.session.get("oid", "")
        target_url += f"?{urlencode(query_params)}"
    elif request.url.query:
        target_url += f"?{request.url.query}"
    
    return await _forward(request, target_url)
