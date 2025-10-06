"""
Azure SSO Authentication Router using MSAL Python

This module handles Azure AD authentication using Microsoft's official MSAL Python library.
It implements best practices including:
- MSAL ConfidentialClientApplication for web apps
- Redis-backed token cache for distributed sessions
- Proper error handling for Conditional Access and MFA
- Account management and token refresh
"""

from __future__ import annotations

import base64
import json
import secrets
from typing import Optional
from urllib.parse import urlparse
import structlog
from fastapi import APIRouter, Request
from fastapi.responses import JSONResponse, RedirectResponse
from msal import ConfidentialClientApplication
import urllib3
from services.msal_token_cache import MSALRedisTokenCache
from utils.redis_client import get_redis_client

from configuration.azure_auth_config import get_azure_auth_settings
from services.token_store import get_session_id

logger = structlog.get_logger(__name__)
router = APIRouter(prefix="/auth", tags=["auth"])

# Disable SSL warnings for development with self-signed certificates
urllib3.disable_warnings(urllib3.exceptions.InsecureRequestWarning)


def _validate_redirect_uri(redirect_uri: Optional[str], request: Request) -> str:
    """
    Validate and sanitize redirect URI to prevent open redirect attacks.
    
    Args:
        redirect_uri: The requested redirect URI
        request: FastAPI request object for extracting server info
        
    Returns:
        Safe redirect URI (relative path) or default fallback
    """
    # Default fallback
    default_redirect = "/projects.html"
    
    if not redirect_uri or not redirect_uri.strip():
        return default_redirect
    
    redirect_uri = redirect_uri.strip()
    
    # Reject javascript: and data: URIs (XSS prevention)
    if redirect_uri.lower().startswith(("javascript:", "data:", "vbscript:", "file:")):
        logger.warning("Rejected dangerous redirect URI", uri=redirect_uri)
        return default_redirect
    
    # Parse the URI
    try:
        parsed = urlparse(redirect_uri)
        
        # If absolute URL, verify it's same origin
        if parsed.scheme or parsed.netloc:
            # Get request origin
            request_host = request.headers.get("host", "")
            
            # Check if same origin
            if parsed.netloc and parsed.netloc != request_host:
                logger.warning(
                    "Rejected external redirect URI",
                    uri=redirect_uri,
                    request_host=request_host,
                    target_host=parsed.netloc
                )
                return default_redirect
            
            # Convert to relative path (safe same-origin URL)
            path = parsed.path or "/"
            if parsed.query:
                path += f"?{parsed.query}"
            if parsed.fragment:
                path += f"#{parsed.fragment}"
            return path
        
        # Already relative, ensure it starts with /
        if not redirect_uri.startswith("/"):
            redirect_uri = "/" + redirect_uri
        
        return redirect_uri
        
    except Exception as e:
        logger.warning("Failed to parse redirect URI", uri=redirect_uri, error=str(e))
        return default_redirect


def _encode_state(csrf_token: str, redirect_uri: str) -> str:
    """
    Encode OAuth state parameter with CSRF token and redirect URI.
    
    Args:
        csrf_token: Random CSRF protection token
        redirect_uri: Validated redirect URI
        
    Returns:
        Base64-encoded state string
    """
    state_data = {
        "csrf": csrf_token,
        "redirect": redirect_uri
    }
    json_str = json.dumps(state_data)
    encoded = base64.urlsafe_b64encode(json_str.encode("utf-8")).decode("utf-8")
    return encoded


def _decode_state(state: str) -> tuple[Optional[str], Optional[str]]:
    """
    Decode OAuth state parameter to extract CSRF token and redirect URI.
    
    Args:
        state: Base64-encoded state string
        
    Returns:
        Tuple of (csrf_token, redirect_uri) or (None, None) if invalid
    """
    try:
        decoded = base64.urlsafe_b64decode(state.encode("utf-8")).decode("utf-8")
        state_data = json.loads(decoded)
        
        csrf_token = state_data.get("csrf")
        redirect_uri = state_data.get("redirect")
        
        return csrf_token, redirect_uri
        
    except Exception as e:
        logger.warning("Failed to decode state parameter", error=str(e))
        return None, None


async def _get_msal_app(
    request: Request, 
    session_id: Optional[str] = None,
    load_cache: bool = False
) -> Optional[ConfidentialClientApplication]:
    """
    Get or create MSAL ConfidentialClientApplication for the request.
    
    Args:
        request: FastAPI request object
        session_id: Optional session ID for token cache
        load_cache: If True, load existing tokens from Redis into cache
        
    Returns:
        Configured MSAL application or None if not configured
    """
    try:
        azure_settings = getattr(request.app.state, "azure_auth_settings", None)
        if not azure_settings:
            azure_settings = get_azure_auth_settings()
        
        # Build authority URL
        authority = f"{azure_settings.AZURE_AD_AUTHORITY}/{azure_settings.AZURE_TENANT_ID}"
        
        # Get token cache if session_id provided
        token_cache = None
        if session_id:
            redis_client = getattr(request.app.state, "redis_client", None) or get_redis_client()
            token_cache = MSALRedisTokenCache(session_id=session_id, redis_client=redis_client)
            
            if load_cache:
                # Load existing tokens from Redis (for auth checks)
                await token_cache.load_from_redis()
                logger.debug("Loaded MSAL cache from Redis", session_id=session_id)
            else:
                # Create empty cache (for new logins)
                logger.debug("Created empty MSAL cache", session_id=session_id)
        
        # Configure HTTP client for SSL verification
        http_client_params = {}
        if not azure_settings.AZURE_AD_VERIFY_SSL:
            # For development with self-signed certificates, disable SSL verification
            import requests
            session = requests.Session()
            session.verify = False
            http_client_params["http_client"] = session
            logger.debug("SSL verification disabled for MSAL (development mode)")
        
        # Create MSAL application
        msal_app = ConfidentialClientApplication(
            client_id=azure_settings.AZURE_CLIENT_ID,
            client_credential=azure_settings.AZURE_CLIENT_SECRET,
            authority=authority,
            token_cache=token_cache,
            instance_discovery=False,  # Disable for local mock auth service
            **http_client_params
        )
        
        return msal_app
        
    except Exception as e:
        logger.error("Failed to create MSAL application", error=str(e))
        return None


@router.get("/login")
async def auth_login(request: Request, redirect_uri: Optional[str] = None):
    """
    Initiate Azure AD login flow using MSAL.
    
    This endpoint generates an authorization URL and redirects the user
    to Azure AD for authentication. Optionally accepts a redirect_uri
    parameter to specify where to redirect after successful authentication.
    
    Args:
        request: FastAPI request object
        redirect_uri: Optional URL to redirect to after authentication (validated for security)
    
    Returns:
        Redirect to Azure AD authorization page
    """
    try:
        msal_app = await _get_msal_app(request)
        if not msal_app:
            logger.warning("MSAL application not configured")
            return JSONResponse(
                {"detail": "Azure AD authentication not configured"},
                status_code=501
            )
        
        azure_settings = getattr(request.app.state, "azure_auth_settings", None) or get_azure_auth_settings()
        
        # Build scopes - MSAL automatically adds OpenID Connect scopes (openid, profile, email, offline_access)
        # Only specify the custom API scope here
        scopes = [azure_settings.SCOPE_NAME]
        
        # Validate and sanitize redirect URI
        safe_redirect_uri = _validate_redirect_uri(redirect_uri, request)
        
        # Generate CSRF token
        csrf_token = secrets.token_urlsafe(32)
        
        # Encode state with both CSRF token and redirect URI
        state = _encode_state(csrf_token, safe_redirect_uri)
        
        # Store CSRF token in session for validation
        request.session["auth_state"] = csrf_token
        
        # Build callback URL
        callback_url = str(request.url_for("auth_callback"))
        
        # Get authorization URL from MSAL
        auth_url = msal_app.get_authorization_request_url(
            scopes=scopes,
            state=state,
            redirect_uri=callback_url
        )
        
        logger.info(
            "Initiating Azure AD login",
            scopes=scopes,
            redirect_after_auth=safe_redirect_uri
        )
        
        return RedirectResponse(auth_url)
        
    except Exception as e:
        logger.error("Failed to initiate Azure AD login", error=str(e))
        return JSONResponse(
            {"detail": f"Login failed: {str(e)}"},
            status_code=500
        )


@router.get("/callback")
async def auth_callback(request: Request):
    """
    Handle Azure AD authentication callback using MSAL.
    
    This endpoint receives the authorization code from Azure AD and exchanges
    it for tokens using MSAL's acquire_token_by_authorization_code method.
    
    Implements proper error handling for:
    - Conditional Access challenges
    - MFA requirements
    - Invalid grant errors
    
    Returns:
        Redirect to original page (or default) on success, error response on failure
    """
    try:
        # Get and decode state parameter
        encoded_state = request.query_params.get("state")
        if not encoded_state:
            logger.warning("Missing OAuth state parameter")
            return JSONResponse(
                {"detail": "Invalid state parameter"},
                status_code=400
            )
        
        # Decode state to extract CSRF token and redirect URI
        csrf_token, redirect_uri = _decode_state(encoded_state)
        
        if not csrf_token or not redirect_uri:
            logger.warning("Failed to decode OAuth state parameter")
            return JSONResponse(
                {"detail": "Invalid state parameter"},
                status_code=400
            )
        
        # Validate CSRF token matches session
        session_csrf = request.session.get("auth_state")
        
        if not session_csrf or csrf_token != session_csrf:
            logger.warning(
                "OAuth state CSRF mismatch",
                session_csrf=session_csrf,
                received_csrf=csrf_token
            )
            return JSONResponse(
                {"detail": "Invalid state parameter"},
                status_code=400
            )
        
        # Get authorization code
        code = request.query_params.get("code")
        if not code:
            error = request.query_params.get("error")
            error_description = request.query_params.get("error_description")
            logger.error("Authorization failed", error=error, description=error_description)
            return JSONResponse(
                {"detail": error_description or error or "Authorization failed"},
                status_code=401
            )
        
        # Get or create session ID
        session_id = get_session_id(request)
        
        # Get MSAL app with token cache
        msal_app = await _get_msal_app(request, session_id=session_id)
        if not msal_app:
            return JSONResponse(
                {"detail": "Azure AD authentication not configured"},
                status_code=501
            )
        
        azure_settings = getattr(request.app.state, "azure_auth_settings", None) or get_azure_auth_settings()
        
        # Build scopes - MSAL automatically adds OpenID Connect scopes (openid, profile, email, offline_access)
        # Only specify the custom API scope here
        scopes = [azure_settings.SCOPE_NAME]
        
        # Build callback URL
        callback_url = str(request.url_for("auth_callback"))
        
        # Exchange authorization code for tokens
        result = msal_app.acquire_token_by_authorization_code(
            code=code,
            scopes=scopes,
            redirect_uri=callback_url
        )
        
        # Handle errors
        if "error" in result:
            error = result.get("error")
            error_description = result.get("error_description", "")
            claims = result.get("claims")
            
            # Handle Conditional Access challenge
            if error == "interaction_required" or claims:
                logger.warning(
                    "Conditional Access or MFA required",
                    error=error,
                    description=error_description
                )
                
                # Store claims challenge for next authentication attempt
                if claims:
                    request.session["claims_challenge"] = claims
                
                return JSONResponse(
                    {
                        "detail": "Additional authentication required",
                        "error": error,
                        "claims": claims
                    },
                    status_code=401
                )
            
            # Handle invalid grant (expired/revoked code)
            elif error == "invalid_grant":
                logger.error("Invalid grant error", description=error_description)
                return JSONResponse(
                    {"detail": f"Authentication expired: {error_description}"},
                    status_code=401
                )
            
            # Generic error
            else:
                logger.error("Token acquisition failed", error=error, description=error_description)
                return JSONResponse(
                    {"detail": f"Authentication failed: {error_description}"},
                    status_code=401
                )
        
        # Success - extract user claims
        id_token_claims = result.get("id_token_claims", {})
        
        username = (
            id_token_claims.get("preferred_username") or
            id_token_claims.get("email") or
            id_token_claims.get("upn") or
            None
        )
        
        oid = str(id_token_claims.get("oid") or id_token_claims.get("sub") or "")
        tid = str(id_token_claims.get("tid") or "")
        
        # Extract roles
        roles_val = id_token_claims.get("roles") or []
        if isinstance(roles_val, str):
            roles_val = [r.strip() for r in roles_val.split(" ") if r.strip()]
        roles = roles_val if isinstance(roles_val, list) else []
        
        # Store claims in session for S2S token minting
        request.session["username"] = username
        request.session["oid"] = oid
        request.session["tid"] = tid
        request.session["roles"] = roles
        
        # Store additional claims
        request.session["exp"] = id_token_claims.get("exp")
        request.session["iat"] = id_token_claims.get("iat")
        request.session["nbf"] = id_token_claims.get("nbf")
        
        # Save token cache to Redis if available
        if hasattr(msal_app.token_cache, 'save_to_redis'):
            try:
                await msal_app.token_cache.save_to_redis()
            except Exception as e:
                logger.warning("Failed to save token cache to Redis", error=str(e))
        
        logger.info(
            "Azure AD authentication successful",
            username=username,
            oid=oid,
            roles=roles,
            redirect_to=redirect_uri
        )
        
        # Clear auth state
        request.session.pop("auth_state", None)
        
        # Redirect to the original page (from decoded state)
        return RedirectResponse(redirect_uri)
        
    except Exception as e:
        logger.error("Authentication callback failed", error=str(e))
        return JSONResponse(
            {"detail": f"Authentication failed: {str(e)}"},
            status_code=500
        )


@router.post("/logout")
async def auth_logout(request: Request):
    """
    Logout user and clear tokens.
    
    This clears the session, MSAL token cache, and optionally GitLab tokens.
    
    Returns:
        JSON confirmation
    """
    try:
        session_id = request.session.get("sid")
        
        # Clear MSAL token cache
        if session_id:
            redis_client = getattr(request.app.state, "redis_client", None) or get_redis_client()
            token_cache = MSALRedisTokenCache(session_id=session_id, redis_client=redis_client)
            await token_cache.clear_from_redis()
        
        # Clear session
        request.session.clear()
        
        logger.info("User logged out", session_id=session_id)
        
        return JSONResponse({"authenticated": False})
        
    except Exception as e:
        logger.error("Logout failed", error=str(e))
        # Even if error, clear session
        request.session.clear()
        return JSONResponse({"authenticated": False})


@router.get("/me")
async def auth_me(request: Request):
    """
    Get current user authentication status and profile.
    
    Checks if the user has a valid token in the MSAL cache using
    acquire_token_silent (MSAL best practice).
    
    Returns:
        JSON with authentication status and user info
    """
    try:
        session_id = request.session.get("sid")
        if not session_id:
            return JSONResponse({
                "authenticated": False,
                "username": None
            })
        
        # Get MSAL app with loaded cache
        msal_app = await _get_msal_app(request, session_id=session_id, load_cache=True)
        if not msal_app:
            return JSONResponse({
                "authenticated": False,
                "username": None
            })
        
        # Get accounts from cache
        accounts = msal_app.get_accounts()
        
        if accounts:
            # Try to acquire token silently
            azure_settings = getattr(request.app.state, "azure_auth_settings", None) or get_azure_auth_settings()
            scopes = [azure_settings.SCOPE_NAME]
            
            result = msal_app.acquire_token_silent(scopes=scopes, account=accounts[0])
            
            if result and "access_token" in result:
                username = request.session.get("username")
                return JSONResponse({
                    "authenticated": True,
                    "username": username
                })
        
        # No valid token
        return JSONResponse({
            "authenticated": False,
            "username": None
        })
        
    except Exception as e:
        logger.error("Failed to get user info", error=str(e))
        return JSONResponse({
            "authenticated": False,
            "username": None
        })