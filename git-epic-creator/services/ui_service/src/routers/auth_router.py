"""
Azure SSO Authentication Router using MSAL Python

This module handles Azure AD authentication using Microsoft's official MSAL Python library.
After authentication, it exchanges the Azure AD token for a LOCAL JWT via authentication-service.

Flow:
1. Browser → /auth/login → Azure AD
2. Azure AD → /auth/callback → MSAL exchanges code for Azure AD tokens
3. UI Service → authentication-service /auth/exchange → LOCAL JWT
4. LOCAL JWT stored in session for backend service calls

Authentication-service is the authority for LOCAL JWT validation (/auth/userinfo).
"""

from __future__ import annotations

import base64
import json
import secrets
from typing import Optional
from urllib.parse import urlparse
import httpx
import structlog
from fastapi import APIRouter, Request
from fastapi.responses import JSONResponse, RedirectResponse
from msal import ConfidentialClientApplication
import urllib3
from services.msal_token_cache import MSALRedisTokenCache
from utils.redis_client import get_redis_client
from configuration.common_config import get_app_settings

from configuration.azure_auth_config import get_azure_auth_settings
from services.token_store import get_session_id

logger = structlog.get_logger(__name__)
router = APIRouter(prefix="/auth", tags=["auth"])

# Disable SSL warnings for development with self-signed certificates
urllib3.disable_warnings(urllib3.exceptions.InsecureRequestWarning)


def _validate_redirect_uri(redirect_uri: Optional[str], request: Request) -> str:
    """
    Validate and sanitize redirect URI to prevent open redirect attacks.
    """
    default_redirect = "/pages/projects.html"
    
    if not redirect_uri or not redirect_uri.strip():
        return default_redirect
    
    redirect_uri = redirect_uri.strip()
    
    # Reject dangerous URIs
    if redirect_uri.lower().startswith(("javascript:", "data:", "vbscript:", "file:")):
        logger.warning("Rejected dangerous redirect URI", uri=redirect_uri)
        return default_redirect
    
    try:
        parsed = urlparse(redirect_uri)
        
        # If absolute URL, verify same origin
        if parsed.scheme or parsed.netloc:
            request_host = request.headers.get("host", "")
            
            if parsed.netloc and parsed.netloc != request_host:
                logger.warning(
                    "Rejected external redirect URI",
                    uri=redirect_uri,
                    request_host=request_host,
                    target_host=parsed.netloc
                )
                return default_redirect
            
            path = parsed.path or "/"
            if parsed.query:
                path += f"?{parsed.query}"
            if parsed.fragment:
                path += f"#{parsed.fragment}"
            return path
        
        if not redirect_uri.startswith("/"):
            redirect_uri = "/" + redirect_uri
        
        return redirect_uri
        
    except Exception as e:
        logger.warning("Failed to parse redirect URI", uri=redirect_uri, error=str(e))
        return default_redirect


def _encode_state(csrf_token: str, redirect_uri: str) -> str:
    """Encode OAuth state parameter with CSRF token and redirect URI."""
    state_data = {"csrf": csrf_token, "redirect": redirect_uri}
    json_str = json.dumps(state_data)
    return base64.urlsafe_b64encode(json_str.encode("utf-8")).decode("utf-8")


def _decode_state(state: str) -> tuple[Optional[str], Optional[str]]:
    """Decode OAuth state parameter to extract CSRF token and redirect URI."""
    try:
        decoded = base64.urlsafe_b64decode(state.encode("utf-8")).decode("utf-8")
        state_data = json.loads(decoded)
        return state_data.get("csrf"), state_data.get("redirect")
    except Exception as e:
        logger.warning("Failed to decode state parameter", error=str(e))
        return None, None


async def _get_msal_app(
    request: Request, 
    session_id: Optional[str] = None,
    load_cache: bool = False
) -> Optional[ConfidentialClientApplication]:
    """Get or create MSAL ConfidentialClientApplication for the request."""
    try:
        azure_settings = getattr(request.app.state, "azure_auth_settings", None)
        if not azure_settings:
            azure_settings = get_azure_auth_settings()
        
        authority = f"{azure_settings.AZURE_AD_AUTHORITY}/{azure_settings.AZURE_TENANT_ID}"
        
        token_cache = None
        if session_id:
            redis_client = getattr(request.app.state, "redis_client", None) or get_redis_client()
            token_cache = MSALRedisTokenCache(session_id=session_id, redis_client=redis_client)
            
            if load_cache:
                await token_cache.load_from_redis()
                logger.debug("Loaded MSAL cache from Redis", session_id=session_id)
        
        http_client_params = {}
        if not azure_settings.AZURE_AD_VERIFY_SSL:
            import requests
            session = requests.Session()
            session.verify = False
            http_client_params["http_client"] = session
        
        msal_app = ConfidentialClientApplication(
            client_id=azure_settings.AZURE_CLIENT_ID,
            client_credential=azure_settings.AZURE_CLIENT_SECRET,
            authority=authority,
            token_cache=token_cache,
            instance_discovery=False,
            **http_client_params
        )
        
        return msal_app
        
    except Exception as e:
        logger.error("Failed to create MSAL application", error=str(e))
        return None


async def _exchange_for_local_jwt(
    access_token: str,
    http_client: httpx.AsyncClient | None = None
) -> dict | None:
    """
    Exchange Azure AD access token for LOCAL JWT via authentication-service.
    
    This is the same mechanism used by MCP Server (VS Code Copilot).
    """
    auth_service_url = get_app_settings().http_client.AUTH_SERVICE_URL.rstrip("/")
    
    close_after = False
    if http_client is None:
        http_client = httpx.AsyncClient(timeout=10.0)
        close_after = True
    
    try:
        response = await http_client.post(
            f"{auth_service_url}/auth/exchange",
            json={"access_token": access_token}
        )
        
        if response.status_code == 200:
            return response.json()
        else:
            logger.error(
                "Token exchange failed",
                status_code=response.status_code,
                response=response.text[:200]
            )
            return None
            
    except Exception as e:
        logger.error("Failed to exchange token", error=str(e))
        return None
    finally:
        if close_after:
            await http_client.aclose()


async def _get_userinfo_from_auth_service(
    local_jwt: str,
    http_client: httpx.AsyncClient | None = None
) -> dict | None:
    """
    Get user info from LOCAL JWT via authentication-service.
    
    Authentication-service is the authority for LOCAL JWT validation.
    This centralizes all JWT validation logic in one service.
    """
    auth_service_url = get_app_settings().http_client.AUTH_SERVICE_URL.rstrip("/")
    
    close_after = False
    if http_client is None:
        http_client = httpx.AsyncClient(timeout=10.0)
        close_after = True
    
    try:
        response = await http_client.get(
            f"{auth_service_url}/auth/userinfo",
            headers={"Authorization": f"Bearer {local_jwt}"}
        )
        
        if response.status_code == 200:
            return response.json()
        else:
            # Token invalid or expired
            logger.debug(
                "Userinfo request failed",
                status_code=response.status_code
            )
            return None
            
    except Exception as e:
        logger.error("Failed to get userinfo", error=str(e))
        return None
    finally:
        if close_after:
            await http_client.aclose()


@router.get("/login")
async def auth_login(request: Request, redirect_uri: Optional[str] = None):
    """Initiate Azure AD login flow using MSAL."""
    try:
        msal_app = await _get_msal_app(request)
        if not msal_app:
            return JSONResponse(
                {"detail": "Azure AD authentication not configured"},
                status_code=501
            )
        
        azure_settings = getattr(request.app.state, "azure_auth_settings", None) or get_azure_auth_settings()
        scopes = [azure_settings.SCOPE_NAME]
        
        safe_redirect_uri = _validate_redirect_uri(redirect_uri, request)
        csrf_token = secrets.token_urlsafe(32)
        state = _encode_state(csrf_token, safe_redirect_uri)
        
        request.session["auth_state"] = csrf_token
        callback_url = str(request.url_for("auth_callback"))
        
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
        return JSONResponse({"detail": f"Login failed: {str(e)}"}, status_code=500)


@router.get("/callback")
async def auth_callback(request: Request):
    """
    Handle Azure AD authentication callback.
    
    After MSAL exchanges the code for tokens, we exchange the Azure AD
    access token for a LOCAL JWT via authentication-service.
    """
    try:
        encoded_state = request.query_params.get("state")
        if not encoded_state:
            return JSONResponse({"detail": "Invalid state parameter"}, status_code=400)
        
        csrf_token, redirect_uri = _decode_state(encoded_state)
        
        if not csrf_token or not redirect_uri:
            return JSONResponse({"detail": "Invalid state parameter"}, status_code=400)
        
        session_csrf = request.session.get("auth_state")
        if not session_csrf or csrf_token != session_csrf:
            logger.warning("OAuth state CSRF mismatch")
            return JSONResponse({"detail": "Invalid state parameter"}, status_code=400)
        
        code = request.query_params.get("code")
        if not code:
            error = request.query_params.get("error")
            error_description = request.query_params.get("error_description")
            logger.error("Authorization failed", error=error, description=error_description)
            return JSONResponse(
                {"detail": error_description or error or "Authorization failed"},
                status_code=401
            )
        
        session_id = get_session_id(request)
        
        msal_app = await _get_msal_app(request, session_id=session_id)
        if not msal_app:
            return JSONResponse(
                {"detail": "Azure AD authentication not configured"},
                status_code=501
            )
        
        azure_settings = getattr(request.app.state, "azure_auth_settings", None) or get_azure_auth_settings()
        scopes = [azure_settings.SCOPE_NAME]
        callback_url = str(request.url_for("auth_callback"))
        
        # Exchange authorization code for Azure AD tokens
        result = msal_app.acquire_token_by_authorization_code(
            code=code,
            scopes=scopes,
            redirect_uri=callback_url
        )
        
        if "error" in result:
            error = result.get("error")
            error_description = result.get("error_description", "")
            claims = result.get("claims")
            
            if error == "interaction_required" or claims:
                logger.warning("Conditional Access or MFA required", error=error)
                if claims:
                    request.session["claims_challenge"] = claims
                return JSONResponse(
                    {"detail": "Additional authentication required", "error": error, "claims": claims},
                    status_code=401
                )
            elif error == "invalid_grant":
                logger.error("Invalid grant error", description=error_description)
                return JSONResponse(
                    {"detail": f"Authentication expired: {error_description}"},
                    status_code=401
                )
            else:
                logger.error("Token acquisition failed", error=error, description=error_description)
                return JSONResponse(
                    {"detail": f"Authentication failed: {error_description}"},
                    status_code=401
                )
        
        # Get the Azure AD access token
        azure_access_token = result.get("access_token")
        if not azure_access_token:
            logger.error("No access token in MSAL result")
            return JSONResponse({"detail": "Authentication failed: no access token"}, status_code=401)
        
        # Exchange Azure AD token for LOCAL JWT via authentication-service
        # This is the SAME mechanism used by MCP Server (VS Code Copilot)
        http_client = getattr(request.app.state, "upstream_http_client", None)
        exchange_result = await _exchange_for_local_jwt(azure_access_token, http_client)
        
        if not exchange_result:
            logger.error("Failed to exchange Azure AD token for LOCAL JWT")
            return JSONResponse(
                {"detail": "Authentication failed: token exchange error"},
                status_code=500
            )
        
        # Store LOCAL JWT and user info in session
        # Note: We store the token but rely on authentication-service for validation
        request.session["local_jwt"] = exchange_result["access_token"]
        request.session["oid"] = exchange_result["user_id"]
        request.session["username"] = exchange_result.get("username")
        request.session["roles"] = exchange_result.get("roles", [])
        
        # Store Azure AD tenant ID (from id_token_claims) for reference
        id_token_claims = result.get("id_token_claims", {})
        request.session["tid"] = str(id_token_claims.get("tid") or "")
        
        # Save MSAL token cache to Redis (for token refresh)
        if hasattr(msal_app.token_cache, 'save_to_redis'):
            try:
                await msal_app.token_cache.save_to_redis()
            except Exception as e:
                logger.warning("Failed to save token cache to Redis", error=str(e))
        
        logger.info(
            "Authentication successful",
            username=exchange_result.get("username"),
            oid=exchange_result["user_id"],
            roles=exchange_result.get("roles", []),
            redirect_to=redirect_uri
        )
        
        request.session.pop("auth_state", None)
        
        return RedirectResponse(redirect_uri)
        
    except Exception as e:
        logger.error("Authentication callback failed", error=str(e))
        return JSONResponse({"detail": f"Authentication failed: {str(e)}"}, status_code=500)


@router.post("/logout")
async def auth_logout(request: Request):
    """Logout user and clear tokens."""
    try:
        session_id = request.session.get("sid")
        
        if session_id:
            redis_client = getattr(request.app.state, "redis_client", None) or get_redis_client()
            token_cache = MSALRedisTokenCache(session_id=session_id, redis_client=redis_client)
            await token_cache.clear_from_redis()
        
        request.session.clear()
        logger.info("User logged out", session_id=session_id)
        
        return JSONResponse({"authenticated": False})
        
    except Exception as e:
        logger.error("Logout failed", error=str(e))
        request.session.clear()
        return JSONResponse({"authenticated": False})


@router.get("/me")
async def auth_me(request: Request):
    """
    Get current user authentication status and profile.
    
    This endpoint delegates LOCAL JWT validation to authentication-service,
    which is the authority for JWT validation. If the token is expired,
    it attempts to refresh via MSAL and get a new LOCAL JWT.
    """
    try:
        session_id = request.session.get("sid")
        if not session_id:
            return JSONResponse({"authenticated": False, "username": None})
        
        local_jwt = request.session.get("local_jwt")
        if not local_jwt:
            return JSONResponse({"authenticated": False, "username": None})
        
        # Validate LOCAL JWT via authentication-service (single source of truth)
        http_client = getattr(request.app.state, "upstream_http_client", None)
        userinfo = await _get_userinfo_from_auth_service(local_jwt, http_client)
        
        if userinfo and userinfo.get("authenticated"):
            # Token is valid
            return JSONResponse({
                "authenticated": True,
                "username": userinfo.get("username")
            })
        
        # LOCAL JWT expired or invalid - try to refresh via MSAL
        msal_app = await _get_msal_app(request, session_id=session_id, load_cache=True)
        if not msal_app:
            return JSONResponse({"authenticated": False, "username": None})
        
        accounts = msal_app.get_accounts()
        
        if accounts:
            azure_settings = getattr(request.app.state, "azure_auth_settings", None) or get_azure_auth_settings()
            scopes = [azure_settings.SCOPE_NAME]
            
            # Try silent token refresh via MSAL
            result = msal_app.acquire_token_silent(scopes=scopes, account=accounts[0])
            
            if result and "access_token" in result:
                # Exchange refreshed Azure AD token for new LOCAL JWT
                exchange_result = await _exchange_for_local_jwt(result["access_token"], http_client)
                
                if exchange_result:
                    # Update session with new LOCAL JWT
                    request.session["local_jwt"] = exchange_result["access_token"]
                    request.session["oid"] = exchange_result["user_id"]
                    request.session["username"] = exchange_result.get("username")
                    request.session["roles"] = exchange_result.get("roles", [])
                    
                    return JSONResponse({
                        "authenticated": True,
                        "username": exchange_result.get("username")
                    })
        
        # No valid token and couldn't refresh
        return JSONResponse({"authenticated": False, "username": None})
        
    except Exception as e:
        logger.error("Failed to get user info", error=str(e))
        return JSONResponse({"authenticated": False, "username": None})
