"""
GitLab OAuth Authentication Router for gitlab-client-service

This module handles GitLab OAuth authentication flow using Authlib.
Tokens are stored in Redis for session-based authentication.

State Management:
- Uses stateless OAuth state parameter (Base64-encoded JSON)
- Encodes session_id, redirect_uri, and CSRF token into state
- No server-side storage needed - fully scalable and persistent across restarts

Authentication Patterns:
- /authorize: Query parameters (session_id) - browser redirects can't carry headers
- /callback: No auth required - standard OAuth callback from GitLab
- /status: S2S JWT - secure token in Authorization header
- /disconnect: S2S JWT - secure token in Authorization header

This hybrid approach balances security (JWT for API calls) with
OAuth standards (query params for browser redirects).

Endpoints:
- GET /auth/gitlab/authorize - Initiate OAuth flow (query params)
- GET /auth/gitlab/callback - Handle OAuth callback from GitLab (no auth)
- GET /auth/gitlab/status - Check connection status (S2S JWT)
- POST /auth/gitlab/disconnect - Disconnect GitLab integration (S2S JWT)
"""

from __future__ import annotations

import structlog
import secrets
import base64
import json
from typing import Optional
from fastapi import APIRouter, Request
from fastapi.responses import JSONResponse, RedirectResponse
from urllib.parse import urlencode
import time

from services.gitlab_token_manager import save_token, get_token, clear_token
from dependencies import get_session_id_from_jwt

logger = structlog.get_logger(__name__)
router = APIRouter(prefix="/auth/gitlab", tags=["gitlab-auth"])


def _encode_gitlab_state(session_id: str, redirect_uri: str, csrf_token: str) -> str:
    """
    Encode OAuth state parameter with session ID, redirect URI, and CSRF token.
    
    This makes the OAuth flow completely stateless - all necessary data is
    encoded in the state parameter itself, eliminating need for server-side storage.
    
    Args:
        session_id: User session identifier
        redirect_uri: Application URL to redirect to after OAuth completes
        csrf_token: CSRF protection token
        
    Returns:
        Base64-encoded state string
    """
    state_data = {
        "sid": session_id,
        "redirect": redirect_uri,
        "csrf": csrf_token
    }
    encoded = base64.urlsafe_b64encode(
        json.dumps(state_data).encode("utf-8")
    ).decode("utf-8")
    
    return encoded


def _decode_gitlab_state(state: str) -> tuple[Optional[str], Optional[str], Optional[str]]:
    """
    Decode OAuth state parameter to extract session ID, redirect URI, and CSRF token.
    
    Args:
        state: Base64-encoded state string
        
    Returns:
        Tuple of (session_id, redirect_uri, csrf_token) or (None, None, None) if invalid
    """
    try:
        decoded = base64.urlsafe_b64decode(state.encode("utf-8")).decode("utf-8")
        state_data = json.loads(decoded)
        
        session_id = state_data.get("sid")
        redirect_uri = state_data.get("redirect")
        csrf_token = state_data.get("csrf")
        
        if not session_id or not redirect_uri or not csrf_token:
            logger.warning("Incomplete GitLab OAuth state data", state_data=state_data)
            return None, None, None
        
        return session_id, redirect_uri, csrf_token
        
    except Exception as e:
        logger.warning("Failed to decode GitLab OAuth state parameter", error=str(e))
        return None, None, None


@router.get("/authorize")
async def gitlab_authorize(
    request: Request,
    session_id: str,
    redirect_uri: str
):
    """
    Initiate GitLab OAuth authorization flow.
    
    Generates OAuth state parameter encoding session_id and redirect_uri,
    making the flow completely stateless and scalable.
    
    Args:
        request: FastAPI request object
        session_id: User session ID from ui-service (query param)
        redirect_uri: Application URL to redirect to after OAuth completes (query param)
        
    Returns:
        Redirect to GitLab authorization page
        
    Example:
        GET /auth/gitlab/authorize?session_id=abc123&redirect_uri=http://localhost:3000/projects
    """
    try:
        oauth = getattr(request.app.state, "oauth", None)
        if not oauth or not hasattr(oauth, 'gitlab'):
            logger.warning("GitLab OAuth not configured")
            return JSONResponse(
                {"detail": "GitLab OAuth not configured"},
                status_code=501
            )
        
        # Get OAuth settings
        settings = getattr(request.app.state, "gitlab_settings", None)
        if not settings:
            from config import get_gitlab_client_settings
            settings = get_gitlab_client_settings()
        
        callback_uri = settings.GITLAB_OAUTH_REDIRECT_URI
        
        # Generate CSRF token and encode state with all necessary data
        csrf_token = secrets.token_urlsafe(32)
        state = _encode_gitlab_state(session_id, redirect_uri, csrf_token)
        
        logger.info(
            "Initiating GitLab OAuth flow",
            session_id=session_id,
            redirect_uri=redirect_uri,
            callback_uri=callback_uri
        )
        
        # Build authorization URL
        auth_params = {
            'client_id': settings.GITLAB_OAUTH_CLIENT_ID,
            'redirect_uri': callback_uri,
            'response_type': 'code',
            'scope': settings.GITLAB_OAUTH_SCOPES,
            'state': state
        }
        
        # Use GITLAB_BASE_URL for OAuth authorization (this is the GitLab instance URL)
        auth_url = f"{settings.GITLAB_BASE_URL}/oauth/authorize?{urlencode(auth_params)}"
        
        return RedirectResponse(auth_url)
        
    except Exception as e:
        logger.error("Failed to initiate GitLab OAuth", error=str(e), exc_info=True)
        return JSONResponse(
            {"detail": f"GitLab OAuth failed: {str(e)}"},
            status_code=500
        )


@router.get("/callback")
async def gitlab_callback(request: Request):
    """
    Handle GitLab OAuth callback.
    
    Decodes state parameter to extract session_id and redirect_uri,
    validates CSRF token, exchanges authorization code for access token,
    and stores token in Redis. Then redirects browser back to UI.
    
    Args:
        request: FastAPI request object with OAuth callback parameters
        
    Returns:
        Redirect to original application URI
    """
    try:
        # Check for OAuth error responses from GitLab
        error = request.query_params.get("error")
        if error:
            error_desc = request.query_params.get("error_description", "")
            logger.warning("GitLab OAuth error", error=error, description=error_desc)
            
            error_messages = {
                "access_denied": "Access to GitLab was denied",
                "invalid_grant": "Authorization code expired or invalid",
                "invalid_request": "Invalid OAuth request",
                "unauthorized_client": "Application not authorized"
            }
            
            return JSONResponse({
                "detail": error_messages.get(error, error_desc or f"OAuth error: {error}"),
                "error": error
            }, status_code=401)
        
        # Extract state and code from callback
        state = request.query_params.get("state")
        code = request.query_params.get("code")
        
        if not state or not code:
            logger.error("Missing state or code in OAuth callback")
            return JSONResponse(
                {"detail": "Invalid OAuth callback - missing parameters"},
                status_code=400
            )
        
        # Decode state to extract session_id, redirect_uri, and csrf_token
        session_id, redirect_uri, csrf_token = _decode_gitlab_state(state)
        
        if not session_id or not redirect_uri or not csrf_token:
            logger.error("Invalid or malformed OAuth state parameter")
            return JSONResponse(
                {"detail": "Invalid or expired OAuth state"},
                status_code=400
            )
        
        logger.debug(
            "Decoded GitLab OAuth state",
            session_id=session_id,
            redirect_uri=redirect_uri,
            has_csrf=bool(csrf_token)
        )
        
        oauth = getattr(request.app.state, "oauth", None)
        if not oauth or not hasattr(oauth, 'gitlab'):
            logger.warning("GitLab OAuth not configured")
            return JSONResponse(
                {"detail": "GitLab OAuth not configured"},
                status_code=501
            )
        
        # Exchange code for token using Authlib
        # Use Authlib's authorize_access_token which handles the full OAuth callback
        token = await oauth.gitlab.authorize_access_token(request)
        
        if not token or not token.get('access_token'):
            logger.error("GitLab token response missing access_token")
            return JSONResponse(
                {"detail": "Token exchange failed"},
                status_code=500
            )
        
        # Add created_at timestamp for token expiry tracking
        token['created_at'] = int(time.time())
        
        # Get Redis client
        redis_client = getattr(request.app.state, "redis_client", None)
        if not redis_client:
            from utils.redis_client import get_redis_client
            redis_client = get_redis_client()
        
        # Store token in Redis
        await save_token(session_id, redis_client, token)
        
        logger.info(
            "GitLab OAuth completed successfully",
            session_id=session_id,
            has_refresh_token=bool(token.get('refresh_token'))
        )
        
        # Redirect to original application URI
        return RedirectResponse(redirect_uri)
        
    except Exception as e:
        logger.error("GitLab OAuth callback failed", error=str(e), exc_info=True)
        return JSONResponse(
            {"detail": f"OAuth callback failed: {str(e)}"},
            status_code=500
        )


@router.get("/status")
async def gitlab_status(request: Request):
    """
    Get GitLab connection status for a session.
    
    Requires S2S JWT in Authorization header with 'oid' claim containing session_id.
    
    Args:
        request: FastAPI request object
    
    Returns:
        JSON with connection and configuration status
        
    Example response:
        ```json
        {
            "connected": true,
            "configured": true
        }
        ```
    """
    
    # Check if GitLab OAuth is configured
    settings = getattr(request.app.state, "gitlab_settings", None)
    if not settings:
        from config import get_gitlab_client_settings
        settings = get_gitlab_client_settings()
    
    configured = bool(
        settings.GITLAB_OAUTH_CLIENT_ID and
        settings.GITLAB_OAUTH_CLIENT_SECRET and
        settings.GITLAB_OAUTH_REDIRECT_URI
    )
    
    # Check if session has valid GitLab token
    connected = False
    try:
        # Extract session_id from S2S JWT
        session_id = get_session_id_from_jwt(request)
        
        redis_client = getattr(request.app.state, "redis_client", None)
        if not redis_client:
            from utils.redis_client import get_redis_client
            redis_client = get_redis_client()
        
        token = await get_token(session_id, redis_client)
        connected = bool(token and token.get("access_token"))
        
        logger.debug(
            "GitLab status check",
            session_id=session_id,
            connected=connected
        )
    except Exception as e:
        logger.warning(
            "Error checking GitLab connection status",
            error=str(e)
        )
    
    return JSONResponse({
        "connected": connected,
        "configured": configured
    })


@router.post("/disconnect")
async def gitlab_disconnect(request: Request):
    """
    Disconnect GitLab integration.
    
    Requires S2S JWT in Authorization header with 'oid' claim containing session_id.
    
    Clears the OAuth token from Redis. Token revocation on GitLab server
    is not implemented as it requires additional API calls and the token
    will naturally expire.
    
    Args:
        request: FastAPI request object
        
    Returns:
        JSON confirmation
    """
    
    try:
        # Extract session_id from S2S JWT
        session_id = get_session_id_from_jwt(request)
        
        redis_client = getattr(request.app.state, "redis_client", None)
        if not redis_client:
            from utils.redis_client import get_redis_client
            redis_client = get_redis_client()
        
        # Clear token from Redis
        await clear_token(session_id, redis_client)
        
        logger.info("GitLab disconnected", session_id=session_id)
        
        return JSONResponse({"disconnected": True})
        
    except Exception as e:
        logger.error("Failed to disconnect GitLab", error=str(e))
        return JSONResponse(
            {"detail": f"Failed to disconnect: {str(e)}"},
            status_code=500
        )

