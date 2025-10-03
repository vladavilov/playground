"""
GitLab OAuth Authentication Router

This module handles GitLab OAuth authentication flow separately from Azure SSO.
It manages GitLab token lifecycle including authorization, callback, and status.
"""

from __future__ import annotations

import structlog
from fastapi import APIRouter, Request
from fastapi.responses import JSONResponse, RedirectResponse

from services.gitlab_token_manager import get_gitlab_token_manager
from services.token_store import get_session_id

logger = structlog.get_logger(__name__)
router = APIRouter(prefix="/auth/gitlab", tags=["gitlab-auth"])


@router.get("/authorize")
async def gitlab_authorize(
    request: Request,
    redirect_uri: str,
    scopes: str | None = None
):
    """
    Initiate GitLab OAuth authorization flow with PKCE and state validation.
    
    Args:
        request: FastAPI request object
        redirect_uri: URL to redirect after GitLab OAuth completes
        scopes: Space or comma-separated GitLab scopes (defaults to config)
        
    Returns:
        Redirect to GitLab authorization page
        
    Example:
        GET /auth/gitlab/authorize?redirect_uri=/projects&scopes=read_api,write_repository
    """
    try:
        oauth = getattr(request.app.state, "oauth", None)
        if oauth is None:
            logger.warning("OAuth not configured on app state")
            return JSONResponse(
                {"detail": "OAuth not configured"},
                status_code=501
            )
        
        # Get GitLab OAuth settings from app state
        base_url = getattr(request.app.state, "gitlab_base_url", "")
        client_id = getattr(request.app.state, "gitlab_client_id", "")
        client_secret = getattr(request.app.state, "gitlab_client_secret", "")
        callback_uri = getattr(request.app.state, "gitlab_redirect_uri", "")
        default_scopes = getattr(request.app.state, "gitlab_scopes", "read_api")
        
        if not (base_url and client_id and client_secret and callback_uri):
            logger.warning("GitLab OAuth settings not fully configured")
            return JSONResponse(
                {"detail": "GitLab OAuth not configured"},
                status_code=501
            )
        
        # Determine scopes (allows override per-request)
        scope_val = (scopes or default_scopes).replace(",", " ")
        
        # GitLab OAuth client is registered at startup (see main.py)
        # Verify it's available
        if not hasattr(oauth, 'gitlab'):
            logger.error("GitLab OAuth client not registered at startup")
            return JSONResponse(
                {"detail": "GitLab OAuth not properly initialized"},
                status_code=500
            )
        
        # Store return URI and requested scopes in session for callback
        request.session["gitlab_return_uri"] = redirect_uri
        request.session["gitlab_requested_scopes"] = scope_val
        
        logger.info(
            "Initiating GitLab OAuth flow",
            scopes=scope_val,
            redirect_uri=redirect_uri
        )
        
        # Redirect to GitLab authorization page (Authlib generates state automatically)
        return await oauth.gitlab.authorize_redirect(request, redirect_uri=callback_uri)
        
    except Exception as e:
        logger.error("Failed to initiate GitLab OAuth", error=str(e))
        return JSONResponse(
            {"detail": f"GitLab OAuth failed: {str(e)}"},
            status_code=500
        )


@router.get("/callback")
async def gitlab_callback(request: Request):
    """
    Handle GitLab OAuth callback with comprehensive error handling.
    
    This endpoint receives the authorization code from GitLab and exchanges
    it for an access token. Implements proper error handling for OAuth errors.
    
    Args:
        request: FastAPI request object with code parameter
        
    Returns:
        Redirect to original return URI
    """
    try:
        # Check for OAuth error responses from GitLab
        error = request.query_params.get("error")
        if error:
            error_desc = request.query_params.get("error_description", "")
            logger.warning("GitLab OAuth error", error=error, description=error_desc)
            
            # Map OAuth errors to user-friendly messages per GitLab docs
            error_messages = {
                "access_denied": "You denied access to GitLab. Please try again if you want to connect.",
                "invalid_grant": "Authorization code expired. Please try connecting to GitLab again.",
                "invalid_request": "Invalid OAuth request. Please contact support if this persists.",
                "unauthorized_client": "This application is not authorized. Please contact your administrator."
            }
            
            return JSONResponse({
                "detail": error_messages.get(error, error_desc or f"GitLab OAuth error: {error}"),
                "error": error
            }, status_code=401)
        
        oauth = getattr(request.app.state, "oauth", None)
        if oauth is None:
            logger.warning("OAuth not configured on app state")
            return JSONResponse(
                {"detail": "OAuth not configured"},
                status_code=501
            )
        
        # Exchange authorization code for access token
        # Authlib validates state internally and raises error if mismatch
        token_response = await oauth.gitlab.authorize_access_token(request)
        
        access_token = token_response.get("access_token")
        refresh_token = token_response.get("refresh_token")
        expires_in = token_response.get("expires_in")
        received_scopes = token_response.get("scope", [])
        
        # Validate scopes
        if isinstance(received_scopes, str):
            received_scopes = received_scopes.split()
        
        if not access_token:
            logger.error("GitLab token response missing access_token")
            return JSONResponse(
                {"detail": "GitLab token missing"},
                status_code=500
            )
        
        # Get session ID
        session_id = get_session_id(request)
        
        # Get GitLab settings for token manager
        gitlab_base_url = getattr(request.app.state, "gitlab_base_url", "")
        client_id = getattr(request.app.state, "gitlab_client_id", "")
        client_secret = getattr(request.app.state, "gitlab_client_secret", "")
        verify_ssl = getattr(request.app.state, "gitlab_verify_ssl", True)
        
        # Get Redis client
        redis_client = getattr(request.app.state, "redis_client", None)
        if not redis_client:
            from utils.redis_client import get_redis_client
            redis_client = get_redis_client()
        
        # Save GitLab token using token manager
        token_manager = await get_gitlab_token_manager(
            session_id=session_id,
            redis_client=redis_client,
            gitlab_base_url=gitlab_base_url,
            client_id=client_id,
            client_secret=client_secret,
            verify_ssl=verify_ssl
        )
        
        await token_manager.save_token(
            access_token=access_token,
            refresh_token=refresh_token,
            expires_in=expires_in
        )
        
        logger.info(
            "GitLab OAuth completed successfully",
            session_id=session_id,
            expires_in=expires_in,
            scopes=received_scopes
        )
        
        # Redirect to original return URI
        return_uri = request.session.get("gitlab_return_uri") or "/"
        return RedirectResponse(return_uri)
        
    except Exception as e:
        logger.error("GitLab OAuth callback failed", error=str(e))
        return JSONResponse(
            {"detail": f"GitLab auth failed: {str(e)}"},
            status_code=500
        )


@router.get("/status")
async def gitlab_status(request: Request):
    """
    Get GitLab connection status for current session.
    
    Returns whether the current session has a valid GitLab access token
    and whether GitLab OAuth is configured in the application.
    
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
    # Check if GitLab OAuth is configured (no exceptions expected here)
    base_url = getattr(request.app.state, "gitlab_base_url", "")
    client_id = getattr(request.app.state, "gitlab_client_id", "")
    client_secret = getattr(request.app.state, "gitlab_client_secret", "")
    redirect_uri = getattr(request.app.state, "gitlab_redirect_uri", "")
    
    configured = bool(base_url and client_id and client_secret and redirect_uri)
    
    logger.debug(
        "GitLab configuration check",
        configured=configured,
        has_base_url=bool(base_url),
        has_client_id=bool(client_id),
        has_client_secret=bool(client_secret),
        has_redirect_uri=bool(redirect_uri)
    )
    
    # Check if session has GitLab token
    connected = False
    try:
        session_id = request.session.get("sid")
        
        if not session_id:
            logger.debug("No session ID found in request, GitLab not connected")
        else:
            redis_client = getattr(request.app.state, "redis_client", None)
            if not redis_client:
                logger.warning("Redis client not found on app.state, using fallback")
                from utils.redis_client import get_redis_client
                redis_client = get_redis_client()
            
            token_manager = await get_gitlab_token_manager(
                session_id=session_id,
                redis_client=redis_client
            )
            
            token_data = await token_manager.load_token()
            connected = bool(token_data and token_data.get("access_token"))
            
            logger.debug(
                "GitLab token check",
                session_id=session_id,
                has_token=connected
            )
    except Exception as e:
        logger.warning(
            "Error checking GitLab connection status",
            error=str(e),
            error_type=type(e).__name__
        )
    
    response_data = {
        "connected": connected,
        "configured": configured
    }
    
    logger.info(
        "GitLab status response",
        connected=connected,
        configured=configured
    )
    
    return JSONResponse(response_data)


@router.post("/disconnect")
async def gitlab_disconnect(request: Request):
    """
    Disconnect GitLab integration and revoke token on GitLab server.
    
    This endpoint:
    1. Revokes the token on GitLab server using /oauth/revoke
    2. Removes the stored GitLab access token from Redis
    
    Per GitLab OAuth docs: https://docs.gitlab.com/api/oauth2/#revoke-a-token
    
    Args:
        request: FastAPI request object
        
    Returns:
        JSON confirmation
    """
    try:
        session_id = request.session.get("sid")
        if not session_id:
            return JSONResponse({"disconnected": True})
        
        redis_client = getattr(request.app.state, "redis_client", None)
        if not redis_client:
            from utils.redis_client import get_redis_client
            redis_client = get_redis_client()
        
        # Get GitLab settings
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
        
        # Revoke token on GitLab server before clearing locally
        if gitlab_base_url and client_id and client_secret:
            await token_manager.revoke_token()
        
        # Clear token from Redis
        await token_manager.clear_token()
        
        logger.info("GitLab disconnected and token revoked", session_id=session_id)
        
        return JSONResponse({"disconnected": True})
        
    except Exception as e:
        logger.error("Failed to disconnect GitLab", error=str(e))
        return JSONResponse(
            {"detail": f"Failed to disconnect: {str(e)}"},
            status_code=500
        )
