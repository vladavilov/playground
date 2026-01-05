"""
Browser Azure SSO Authentication Router using MSAL Python.

This router establishes a **browser session** by storing identity/roles in the session cookie.
Downstream API authorization is enforced by Envoy via `ext_authz` (implemented separately).
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

from configuration.azure_auth_config import get_azure_auth_settings

logger = structlog.get_logger(__name__)
router = APIRouter(prefix="/auth", tags=["auth"])


def _validate_redirect_uri(redirect_uri: Optional[str], request: Request) -> str:
    """
    Validate and sanitize redirect URI to prevent open redirect attacks.
    """
    default_redirect = "/projects"

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
                    target_host=parsed.netloc,
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
    state_data = {"csrf": csrf_token, "redirect": redirect_uri}
    json_str = json.dumps(state_data)
    return base64.urlsafe_b64encode(json_str.encode("utf-8")).decode("utf-8")


def _decode_state(state: str) -> tuple[Optional[str], Optional[str]]:
    try:
        decoded = base64.urlsafe_b64decode(state.encode("utf-8")).decode("utf-8")
        state_data = json.loads(decoded)
        return state_data.get("csrf"), state_data.get("redirect")
    except Exception as e:
        logger.warning("Failed to decode state parameter", error=str(e))
        return None, None


async def _get_msal_app(request: Request) -> Optional[ConfidentialClientApplication]:
    try:
        azure_settings = get_azure_auth_settings()
        authority = f"{azure_settings.AZURE_AD_AUTHORITY}/{azure_settings.AZURE_TENANT_ID}"

        http_client_params = {}
        if not azure_settings.AZURE_AD_VERIFY_SSL:
            import requests

            session = requests.Session()
            session.verify = False
            http_client_params["http_client"] = session

        return ConfidentialClientApplication(
            client_id=azure_settings.AZURE_CLIENT_ID,
            client_credential=azure_settings.AZURE_CLIENT_SECRET,
            authority=authority,
            instance_discovery=False,
            **http_client_params,
        )
    except Exception as e:
        logger.error("Failed to create MSAL application", error=str(e))
        return None


def _extract_username(id_token_claims: dict) -> str | None:
    for key in ("preferred_username", "upn", "email"):
        val = id_token_claims.get(key)
        if isinstance(val, str) and val.strip():
            return val.strip()
    return None


@router.get("/login")
async def auth_login(request: Request, redirect_uri: Optional[str] = None):
    """Initiate Azure AD login flow using MSAL."""
    msal_app = await _get_msal_app(request)
    if not msal_app:
        return JSONResponse({"detail": "Azure AD authentication not configured"}, status_code=501)

    azure_settings = getattr(request.app.state, "azure_auth_settings", None) or get_azure_auth_settings()
    scopes = [azure_settings.SCOPE_NAME]

    safe_redirect_uri = _validate_redirect_uri(redirect_uri, request)
    csrf_token = secrets.token_urlsafe(32)
    state = _encode_state(csrf_token, safe_redirect_uri)

    request.session["auth_state"] = csrf_token
    callback_url = str(request.url_for("auth_callback"))

    auth_url = msal_app.get_authorization_request_url(scopes=scopes, state=state, redirect_uri=callback_url)

    logger.info("Initiating Azure AD login", scopes=scopes, redirect_after_auth=safe_redirect_uri)
    return RedirectResponse(auth_url)


@router.get("/callback")
async def auth_callback(request: Request):
    """
    Handle Azure AD authentication callback.

    After MSAL exchanges the code for tokens, we store user identity/roles in the session.
    """
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
        return JSONResponse({"detail": error_description or error or "Authorization failed"}, status_code=401)

    msal_app = await _get_msal_app(request)
    if not msal_app:
        return JSONResponse({"detail": "Azure AD authentication not configured"}, status_code=501)

    azure_settings = getattr(request.app.state, "azure_auth_settings", None) or get_azure_auth_settings()
    scopes = [azure_settings.SCOPE_NAME]
    callback_url = str(request.url_for("auth_callback"))

    result = msal_app.acquire_token_by_authorization_code(code=code, scopes=scopes, redirect_uri=callback_url)

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
                status_code=401,
            )
        if error == "invalid_grant":
            logger.error("Invalid grant error", description=error_description)
            return JSONResponse({"detail": f"Authentication expired: {error_description}"}, status_code=401)

        logger.error("Token acquisition failed", error=error, description=error_description)
        return JSONResponse({"detail": f"Authentication failed: {error_description}"}, status_code=401)

    id_token_claims = result.get("id_token_claims", {}) or {}
    oid = id_token_claims.get("oid")
    if not isinstance(oid, str) or not oid.strip():
        return JSONResponse({"detail": "Authentication failed: missing oid"}, status_code=401)

    request.session["oid"] = oid.strip()
    request.session["username"] = _extract_username(id_token_claims)
    roles = id_token_claims.get("roles")
    request.session["roles"] = roles if isinstance(roles, list) else []
    request.session["tid"] = str(id_token_claims.get("tid") or "")
    request.session.pop("auth_state", None)

    logger.info(
        "Authentication successful",
        username=request.session.get("username"),
        oid=request.session.get("oid"),
        roles=request.session.get("roles") or [],
        redirect_to=redirect_uri,
    )
    return RedirectResponse(redirect_uri)


@router.post("/logout")
async def auth_logout(request: Request):
    """Logout user and clear session."""
    request.session.clear()
    logger.info("User logged out")
    return JSONResponse({"authenticated": False})


@router.get("/me")
async def auth_me(request: Request):
    """Get current user authentication status and profile."""
    oid = request.session.get("oid")
    if not isinstance(oid, str) or not oid.strip():
        return JSONResponse({"authenticated": False, "username": None})

    username = request.session.get("username")
    return JSONResponse(
        {
            "authenticated": True,
            "username": username if username is None or isinstance(username, str) else None,
        }
    )


