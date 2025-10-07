"""
Mock Azure AD authentication service.
"""
import os
import time
import uuid
import secrets
import hashlib
import base64
from typing import Dict, Any, Optional
from fastapi import FastAPI, Response, Request, status
from fastapi.responses import RedirectResponse, JSONResponse
from jose import jwt
import uvicorn

from config import settings
from key_manager import KeyManager

# Initialize persistent key management
key_manager = KeyManager()

MOCK_TENANT_ID: str = settings.AZURE_AD_TENANT_ID
MOCK_CLIENT_ID: str = settings.AZURE_AD_CLIENT_ID
MOCK_CLIENT_SECRET: str = settings.AZURE_CLIENT_SECRET or ""
BASE_URL: str = str(settings.AZURE_AD_AUTHORITY).rstrip('/')

app = FastAPI(
    title="Mock Azure AD OIDC Server",
    description="Emulates Azure AD for local development."
)

ISSUER = f"{BASE_URL}/{MOCK_TENANT_ID}/v2.0"

# In-memory stores for mock flows (development only)
auth_codes: Dict[str, Dict[str, Any]] = {}
refresh_sessions: Dict[str, Dict[str, Any]] = {}


@app.get("/health")
async def health():
    return {"status": "ok"}


@app.get("/{tenant_id}/v2.0/.well-known/openid-configuration")
async def openid_configuration(tenant_id: str):
    if tenant_id != MOCK_TENANT_ID:
        return Response(status_code=status.HTTP_404_NOT_FOUND, content="Tenant not found")

    return {
        "authorization_endpoint": f"{BASE_URL}/{MOCK_TENANT_ID}/oauth2/v2.0/authorize",
        "token_endpoint": f"{BASE_URL}/{MOCK_TENANT_ID}/oauth2/v2.0/token",
        "device_authorization_endpoint": f"{BASE_URL}/{MOCK_TENANT_ID}/oauth2/v2.0/devicecode",
        "end_session_endpoint": f"{BASE_URL}/{MOCK_TENANT_ID}/oauth2/v2.0/logout",
        "jwks_uri": f"{BASE_URL}/{MOCK_TENANT_ID}/discovery/v2.0/keys",
        "issuer": ISSUER,
        "response_modes_supported": ["query", "fragment", "form_post"],
        "subject_types_supported": ["pairwise"],
        "id_token_signing_alg_values_supported": ["RS256"],
        "response_types_supported": ["code", "id_token", "code id_token", "id_token token"],
        "grant_types_supported": ["authorization_code", "refresh_token"],
        "scopes_supported": ["openid", "profile", "email", "offline_access"],
        "claims_supported": [
            "sub", "iss", "aud", "exp", "iat", "nonce", "preferred_username", "name", "tid", "ver", "oid", "email", "roles"
        ],
        "code_challenge_methods_supported": ["S256", "plain"],
        "userinfo_endpoint": f"{BASE_URL}/{MOCK_TENANT_ID}/v2.0/userinfo",
    }


@app.get("/{tenant_id}/discovery/v2.0/keys")
async def get_jwks(tenant_id: str):
    if tenant_id != MOCK_TENANT_ID:
        return Response(status_code=status.HTTP_404_NOT_FOUND, content="Tenant not found")
    return {"keys": [key_manager.get_jwk()]}


def _base_user_profile() -> Dict[str, Any]:
    user_object_id = str(uuid.uuid4())
    return {
        "oid": user_object_id,
        "preferred_username": "mock.user@example.com",
        "name": "Mock User",
        "email": "mock.user@example.com",
        "roles": ["User", "Admin"],
        "sub": str(uuid.uuid4()),
    }


def _sign_access_token(profile: Dict[str, Any], expires_in: int = 3600) -> str:
    now = int(time.time())
    claims = {
        "aud": MOCK_CLIENT_ID,
        "iss": ISSUER,
        "iat": now,
        "nbf": now,
        "exp": now + int(expires_in),
        "ver": "2.0",
        "tid": MOCK_TENANT_ID,
        **{k: v for k, v in profile.items() if k in {"oid", "preferred_username", "name", "email", "roles", "sub"}},
    }
    return jwt.encode(
        claims,
        key_manager.get_private_key(),
        algorithm="RS256",
        headers={"kid": key_manager.get_key_id()},
    )


def _sign_id_token(profile: Dict[str, Any], nonce: Optional[str]) -> str:
    now = int(time.time())
    claims = {
        "aud": MOCK_CLIENT_ID,
        "iss": ISSUER,
        "iat": now,
        "exp": now + 3600,
        "ver": "2.0",
        "tid": MOCK_TENANT_ID,
        "nonce": nonce or "",
        "preferred_username": profile.get("preferred_username"),
        "name": profile.get("name"),
        "email": profile.get("email"),
        "sub": profile.get("sub"),
        "oid": profile.get("oid"),
        "roles": profile.get("roles", []),
    }
    return jwt.encode(
        claims,
        key_manager.get_private_key(),
        algorithm="RS256",
        headers={"kid": key_manager.get_key_id()},
    )


def _validate_client(client_id: str, client_secret: Optional[str]) -> bool:
    if client_id != MOCK_CLIENT_ID:
        return False
    # If a secret is configured, require it; otherwise allow public client
    if MOCK_CLIENT_SECRET:
        return (client_secret or "") == MOCK_CLIENT_SECRET
    return True


def _verify_pkce(code_verifier: Optional[str], code_challenge: Optional[str], method: Optional[str]) -> bool:
    if not code_challenge:
        return True  # PKCE not used
    if not code_verifier:
        return False
    if (method or "").upper() == "S256":
        digest = hashlib.sha256(code_verifier.encode("ascii")).digest()
        calc = base64.urlsafe_b64encode(digest).rstrip(b"=").decode("ascii")
        return calc == code_challenge
    # plain
    return code_verifier == code_challenge


@app.get("/{tenant_id}/oauth2/v2.0/authorize")
async def authorize_endpoint(tenant_id: str, request: Request):
    if tenant_id != MOCK_TENANT_ID:
        return Response(status_code=status.HTTP_404_NOT_FOUND, content="Tenant not found")
    q = request.query_params
    client_id = q.get("client_id") or ""
    redirect_uri = q.get("redirect_uri") or q.get("redirect_url") or ""
    response_type = q.get("response_type") or "code"
    scope = q.get("scope") or ""
    state = q.get("state")
    nonce = q.get("nonce")
    code_challenge = q.get("code_challenge")
    code_challenge_method = q.get("code_challenge_method")

    if response_type != "code" or not client_id or not redirect_uri:
        return JSONResponse({"error": "invalid_request"}, status_code=400)
    if client_id != MOCK_CLIENT_ID:
        return JSONResponse({"error": "unauthorized_client"}, status_code=401)

    # Create authorization code representing an authenticated session
    profile = _base_user_profile()
    code = secrets.token_urlsafe(32)
    auth_codes[code] = {
        "client_id": client_id,
        "redirect_uri": redirect_uri,
        "scope": scope,
        "nonce": nonce,
        "created_at": int(time.time()),
        "code_challenge": code_challenge,
        "code_challenge_method": (code_challenge_method or "plain").upper(),
        "profile": profile,
    }

    # Redirect back with code and preserved state
    sep = "&" if ("?" in redirect_uri) else "?"
    location = f"{redirect_uri}{sep}code={code}"
    if state:
        location += f"&state={state}"
    return RedirectResponse(location)


@app.post("/{tenant_id}/oauth2/v2.0/token")
async def token_endpoint(tenant_id: str, request: Request):
    if tenant_id != MOCK_TENANT_ID:
        return Response(status_code=status.HTTP_404_NOT_FOUND, content="Tenant not found")

    form = await request.form()
    grant_type = (form.get("grant_type") or "").strip()
    client_id = (form.get("client_id") or "").strip()
    client_secret = (form.get("client_secret") or "").strip()

    if not _validate_client(client_id, client_secret):
        return JSONResponse({"error": "invalid_client"}, status_code=401)

    if grant_type == "authorization_code":
        code = (form.get("code") or "").strip()
        redirect_uri = (form.get("redirect_uri") or "").strip()
        code_verifier = (form.get("code_verifier") or "").strip() or None
        data = auth_codes.pop(code, None)
        if not data or data.get("client_id") != client_id or data.get("redirect_uri") != redirect_uri:
            return JSONResponse({"error": "invalid_grant"}, status_code=400)
        if not _verify_pkce(code_verifier, data.get("code_challenge"), data.get("code_challenge_method")):
            return JSONResponse({"error": "invalid_grant", "error_description": "PKCE verification failed"}, status_code=400)

        scope = data.get("scope") or "openid profile email"
        nonce = data.get("nonce")
        profile = data.get("profile") or _base_user_profile()

        access_token = _sign_access_token(profile, expires_in=3600)
        id_token = _sign_id_token(profile, nonce)
        refresh_token = secrets.token_urlsafe(48)
        # Store refresh session
        refresh_sessions[refresh_token] = {
            "client_id": client_id,
            "profile": profile,
            "scope": scope,
            "created_at": int(time.time()),
        }
        return {
            "token_type": "Bearer",
            "scope": scope,
            "expires_in": 3599,
            "ext_expires_in": 3599,
            "access_token": access_token,
            "id_token": id_token,
            "refresh_token": refresh_token,
        }

    if grant_type == "refresh_token":
        rtoken = (form.get("refresh_token") or "").strip()
        session = refresh_sessions.get(rtoken)
        if not session or session.get("client_id") != client_id:
            return JSONResponse({"error": "invalid_grant"}, status_code=400)
        profile = session.get("profile") or _base_user_profile()
        scope = session.get("scope") or "openid profile email offline_access"
        access_token = _sign_access_token(profile, expires_in=3600)
        # Optionally rotate refresh token
        new_refresh = secrets.token_urlsafe(48)
        refresh_sessions[new_refresh] = session
        refresh_sessions.pop(rtoken, None)
        return {
            "token_type": "Bearer",
            "scope": scope,
            "expires_in": 3599,
            "ext_expires_in": 3599,
            "access_token": access_token,
            "refresh_token": new_refresh,
        }

    return JSONResponse({"error": "unsupported_grant_type"}, status_code=400)


@app.post("/{tenant_id}/oauth2/v2.0/devicecode")
async def device_code_endpoint(tenant_id: str):
    if tenant_id != MOCK_TENANT_ID:
        return Response(status_code=status.HTTP_404_NOT_FOUND, content="Tenant not found")
    return {"message": "Mock device code endpoint", "tenant_id": tenant_id}


@app.post("/{tenant_id}/oauth2/v2.0/logout")
async def logout_endpoint(tenant_id: str):
    if tenant_id != MOCK_TENANT_ID:
        return Response(status_code=status.HTTP_404_NOT_FOUND, content="Tenant not found")
    return {"message": "Mock logout endpoint", "tenant_id": tenant_id}


@app.get("/{tenant_id}/kerberos")
async def kerberos_endpoint(tenant_id: str):
    if tenant_id != MOCK_TENANT_ID:
        return Response(status_code=status.HTTP_404_NOT_FOUND, content="Tenant not found")
    return {"message": "Mock Kerberos endpoint", "tenant_id": tenant_id}


@app.get("/{tenant_id}/v2.0/userinfo")
async def userinfo_endpoint(tenant_id: str):
    if tenant_id != MOCK_TENANT_ID:
        return Response(status_code=status.HTTP_404_NOT_FOUND, content="Tenant not found")
    profile = _base_user_profile()
    return {
        "sub": profile["sub"],
        "name": profile["name"],
        "preferred_username": profile["preferred_username"],
        "email": profile["email"],
    }


if __name__ == "__main__":
    port = int(os.getenv("API_PORT", 8000))
    
    # Configure SSL/TLS for HTTPS if enabled
    ssl_config = {}
    if settings.ENABLE_HTTPS:
        if os.path.exists(settings.SSL_CERTFILE) and os.path.exists(settings.SSL_KEYFILE):
            ssl_config = {
                "ssl_certfile": settings.SSL_CERTFILE,
                "ssl_keyfile": settings.SSL_KEYFILE,
            }
            print(f"Starting mock auth service with HTTPS on port {port}")
            print(f"Certificate: {settings.SSL_CERTFILE}")
            print(f"Private key: {settings.SSL_KEYFILE}")
        else:
            print(f"Warning: HTTPS enabled but certificates not found. Falling back to HTTP.")
            print(f"Looked for: {settings.SSL_CERTFILE}, {settings.SSL_KEYFILE}")
    
    uvicorn.run(app, host="0.0.0.0", port=port, **ssl_config)
