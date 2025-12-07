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
from configuration.logging_config import configure_logging

# Initialize persistent key management
key_manager = KeyManager()

MOCK_TENANT_ID: str = settings.AZURE_AD_TENANT_ID
MOCK_CLIENT_ID: str = settings.AZURE_AD_CLIENT_ID
MOCK_CLIENT_SECRET: str = settings.AZURE_CLIENT_SECRET or ""
BASE_URL: str = str(settings.AZURE_AD_AUTHORITY).rstrip('/')

# Configure shared logging for consistent JSON logs
configure_logging()

app = FastAPI(
    title="Mock Azure AD OIDC Server",
    description="Emulates Azure AD for local development."
)

ISSUER = f"{BASE_URL}/{MOCK_TENANT_ID}/v2.0"

# In-memory stores for mock flows (development only)
auth_codes: Dict[str, Dict[str, Any]] = {}
refresh_sessions: Dict[str, Dict[str, Any]] = {}

# Dynamic Client Registration store (RFC 7591)
# Maps client_id -> client registration data
registered_clients: Dict[str, Dict[str, Any]] = {
    # Pre-register the default mock client
    MOCK_CLIENT_ID: {
        "client_id": MOCK_CLIENT_ID,
        "client_secret": MOCK_CLIENT_SECRET,
        "client_name": "Default Mock Client",
        "redirect_uris": [],  # Accept any redirect URI for the default client
        "grant_types": ["authorization_code", "refresh_token"],
        "response_types": ["code"],
        "token_endpoint_auth_method": "client_secret_post" if MOCK_CLIENT_SECRET else "none",
    }
}


@app.get("/health")
async def health():
    return {"status": "ok"}


def _get_authorization_server_metadata() -> dict:
    """
    Build Authorization Server Metadata (RFC 8414 / OpenID Connect Discovery).
    
    This is the source of truth for OAuth endpoints that MCP clients discover.
    Includes registration_endpoint for Dynamic Client Registration (RFC 7591).
    """
    return {
        "issuer": ISSUER,
        "authorization_endpoint": f"{BASE_URL}/{MOCK_TENANT_ID}/oauth2/v2.0/authorize",
        "token_endpoint": f"{BASE_URL}/{MOCK_TENANT_ID}/oauth2/v2.0/token",
        "userinfo_endpoint": f"{BASE_URL}/{MOCK_TENANT_ID}/v2.0/userinfo",
        "jwks_uri": f"{BASE_URL}/{MOCK_TENANT_ID}/discovery/v2.0/keys",
        "device_authorization_endpoint": f"{BASE_URL}/{MOCK_TENANT_ID}/oauth2/v2.0/devicecode",
        "end_session_endpoint": f"{BASE_URL}/{MOCK_TENANT_ID}/oauth2/v2.0/logout",
        # Dynamic Client Registration (RFC 7591) - required for VS Code MCP client
        "registration_endpoint": f"{BASE_URL}/{MOCK_TENANT_ID}/oauth2/v2.0/register",
        # Supported OAuth features
        "response_types_supported": ["code", "id_token", "code id_token", "id_token token"],
        "response_modes_supported": ["query", "fragment", "form_post"],
        "grant_types_supported": ["authorization_code", "refresh_token"],
        "scopes_supported": ["openid", "profile", "email", "offline_access"],
        "token_endpoint_auth_methods_supported": ["client_secret_basic", "client_secret_post", "none"],
        "code_challenge_methods_supported": ["S256", "plain"],
        # OpenID Connect features
        "subject_types_supported": ["pairwise"],
        "id_token_signing_alg_values_supported": ["RS256"],
        "claims_supported": [
            "sub", "iss", "aud", "exp", "iat", "nonce", "preferred_username",
            "name", "tid", "ver", "oid", "email", "roles"
        ],
    }


# RFC 8414 path-insertion format: /.well-known/oauth-authorization-server/{issuer-path}
# Per MCP spec, clients try this FIRST for authorization server discovery
@app.get("/.well-known/oauth-authorization-server/{tenant_id}/v2.0")
async def oauth_authorization_server_metadata_path_insertion(tenant_id: str):
    """
    OAuth 2.0 Authorization Server Metadata (RFC 8414) - path insertion format.
    
    MCP clients try this endpoint FIRST per the MCP Authorization spec.
    URL format: /.well-known/oauth-authorization-server/{tenant_id}/v2.0
    """
    if tenant_id != MOCK_TENANT_ID:
        return Response(status_code=status.HTTP_404_NOT_FOUND, content="Tenant not found")
    return _get_authorization_server_metadata()


# OIDC path-insertion format: /.well-known/openid-configuration/{issuer-path}
# Per MCP spec, clients try this SECOND for authorization server discovery
@app.get("/.well-known/openid-configuration/{tenant_id}/v2.0")
async def openid_configuration_path_insertion(tenant_id: str):
    """
    OpenID Connect Discovery (path insertion format).
    
    MCP clients try this endpoint SECOND per the MCP Authorization spec.
    URL format: /.well-known/openid-configuration/{tenant_id}/v2.0
    """
    if tenant_id != MOCK_TENANT_ID:
        return Response(status_code=status.HTTP_404_NOT_FOUND, content="Tenant not found")
    return _get_authorization_server_metadata()


# OIDC path-appending format: /{issuer-path}/.well-known/openid-configuration
# Per MCP spec, clients try this THIRD for authorization server discovery
@app.get("/{tenant_id}/v2.0/.well-known/openid-configuration")
async def openid_configuration(tenant_id: str):
    """
    OpenID Connect Discovery (path appending format).
    
    MCP clients try this endpoint THIRD per the MCP Authorization spec.
    URL format: /{tenant_id}/v2.0/.well-known/openid-configuration
    """
    if tenant_id != MOCK_TENANT_ID:
        return Response(status_code=status.HTTP_404_NOT_FOUND, content="Tenant not found")
    return _get_authorization_server_metadata()


@app.get("/{tenant_id}/discovery/v2.0/keys")
async def get_jwks(tenant_id: str):
    if tenant_id != MOCK_TENANT_ID:
        return Response(status_code=status.HTTP_404_NOT_FOUND, content="Tenant not found")
    return {"keys": [key_manager.get_jwk()]}


@app.post("/{tenant_id}/oauth2/v2.0/register")
async def dynamic_client_registration(tenant_id: str, request: Request):
    """
    OAuth 2.0 Dynamic Client Registration endpoint (RFC 7591).
    
    Allows MCP clients (like VS Code) to automatically register themselves
    with the authorization server without manual configuration.
    
    Accepts:
        - redirect_uris: List of allowed redirect URIs (required)
        - client_name: Human-readable client name (optional)
        - grant_types: Requested grant types (optional, defaults to authorization_code)
        - response_types: Requested response types (optional, defaults to code)
        - token_endpoint_auth_method: Client auth method (optional, defaults to none for public clients)
    
    Returns:
        Client registration response with client_id and optional client_secret
    """
    if tenant_id != MOCK_TENANT_ID:
        return Response(status_code=status.HTTP_404_NOT_FOUND, content="Tenant not found")
    
    try:
        body = await request.json()
    except Exception:
        return JSONResponse(
            {"error": "invalid_request", "error_description": "Invalid JSON body"},
            status_code=400
        )
    
    # Extract registration parameters
    redirect_uris = body.get("redirect_uris", [])
    if not redirect_uris or not isinstance(redirect_uris, list):
        return JSONResponse(
            {"error": "invalid_redirect_uri", "error_description": "redirect_uris is required and must be a list"},
            status_code=400
        )
    
    client_name = body.get("client_name", "Dynamic Client")
    grant_types = body.get("grant_types", ["authorization_code"])
    response_types = body.get("response_types", ["code"])
    token_endpoint_auth_method = body.get("token_endpoint_auth_method", "none")
    
    # Generate client credentials
    client_id = str(uuid.uuid4())
    
    # For public clients (token_endpoint_auth_method=none), no secret is issued
    # For confidential clients, generate a secret
    client_secret = None
    if token_endpoint_auth_method in ["client_secret_basic", "client_secret_post"]:
        client_secret = secrets.token_urlsafe(32)
    
    # Store the registration
    registration = {
        "client_id": client_id,
        "client_secret": client_secret,
        "client_name": client_name,
        "redirect_uris": redirect_uris,
        "grant_types": grant_types,
        "response_types": response_types,
        "token_endpoint_auth_method": token_endpoint_auth_method,
        "registered_at": int(time.time()),
    }
    registered_clients[client_id] = registration
    
    # Build response per RFC 7591
    response = {
        "client_id": client_id,
        "client_name": client_name,
        "redirect_uris": redirect_uris,
        "grant_types": grant_types,
        "response_types": response_types,
        "token_endpoint_auth_method": token_endpoint_auth_method,
        "client_id_issued_at": registration["registered_at"],
    }
    
    # Only include client_secret for confidential clients
    if client_secret:
        response["client_secret"] = client_secret
        response["client_secret_expires_at"] = 0  # Never expires
    
    return JSONResponse(response, status_code=201)


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


def _sign_access_token(profile: Dict[str, Any], client_id: str, expires_in: int = 3600) -> str:
    now = int(time.time())
    claims = {
        "aud": client_id,
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


def _sign_id_token(profile: Dict[str, Any], client_id: str, nonce: Optional[str]) -> str:
    now = int(time.time())
    claims = {
        "aud": client_id,
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
    """
    Validate client credentials against registered clients.
    
    Supports both pre-configured mock client and dynamically registered clients.
    """
    registration = registered_clients.get(client_id)
    if not registration:
        return False
    
    expected_secret = registration.get("client_secret")
    auth_method = registration.get("token_endpoint_auth_method", "none")
    
    # For public clients (none), no secret required
    if auth_method == "none":
        return True
    
    # For confidential clients, verify secret
    return (client_secret or "") == (expected_secret or "")


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


def _is_registered_client(client_id: str) -> bool:
    """Check if client_id is registered (pre-configured or dynamically)."""
    return client_id in registered_clients


def _validate_redirect_uri(client_id: str, redirect_uri: str) -> bool:
    """
    Validate redirect_uri against registered client's allowed URIs.
    
    For the default mock client, any redirect_uri is allowed.
    For dynamically registered clients, must match registered redirect_uris.
    """
    registration = registered_clients.get(client_id)
    if not registration:
        return False
    
    allowed_uris = registration.get("redirect_uris", [])
    # Empty list means any URI is allowed (for default mock client)
    if not allowed_uris:
        return True
    
    return redirect_uri in allowed_uris


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
    
    # Validate client is registered (supports both pre-configured and dynamic clients)
    if not _is_registered_client(client_id):
        return JSONResponse({"error": "unauthorized_client", "error_description": "Client not registered"}, status_code=401)
    
    # Validate redirect_uri is allowed for this client
    if not _validate_redirect_uri(client_id, redirect_uri):
        return JSONResponse({"error": "invalid_redirect_uri", "error_description": "Redirect URI not registered"}, status_code=400)

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


def _extract_client_credentials(request: Request, form: dict) -> tuple[str, str]:
    """
    Extract client credentials from request.
    
    Supports both authentication methods:
    - client_secret_basic: Authorization header (Basic base64(client_id:client_secret))
    - client_secret_post: Form body (client_id, client_secret fields)
    """
    # Try client_secret_basic (Authorization header)
    auth_header = request.headers.get("Authorization", "")
    if auth_header.lower().startswith("basic "):
        try:
            encoded = auth_header[6:]
            decoded = base64.b64decode(encoded).decode("utf-8")
            if ":" in decoded:
                return decoded.split(":", 1)
        except Exception:
            pass
    
    # Fall back to client_secret_post (form body)
    client_id = (form.get("client_id") or "").strip()
    client_secret = (form.get("client_secret") or "").strip()
    return client_id, client_secret


@app.post("/{tenant_id}/oauth2/v2.0/token")
async def token_endpoint(tenant_id: str, request: Request):
    """
    OAuth 2.0 Token endpoint.
    
    Supports:
    - grant_type=authorization_code (with PKCE)
    - grant_type=refresh_token
    - client_secret_basic and client_secret_post authentication
    """
    if tenant_id != MOCK_TENANT_ID:
        return Response(status_code=status.HTTP_404_NOT_FOUND, content="Tenant not found")

    form = await request.form()
    grant_type = (form.get("grant_type") or "").strip()
    client_id, client_secret = _extract_client_credentials(request, form)

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

        access_token = _sign_access_token(profile, client_id, expires_in=3600)
        id_token = _sign_id_token(profile, client_id, nonce)
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
        access_token = _sign_access_token(profile, client_id, expires_in=3600)
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
