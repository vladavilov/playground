"""
Mock Azure AD authentication service.
"""
import os
import time
import uuid
from fastapi import FastAPI, Response, Request, status
from jose import jwt
import uvicorn

from .config import settings
from .key_manager import KeyManager

# Initialize persistent key management
key_manager = KeyManager()

MOCK_TENANT_ID: str = settings.AZURE_AD_TENANT_ID
MOCK_CLIENT_ID: str = settings.AZURE_AD_CLIENT_ID
BASE_URL: str = str(settings.AZURE_AD_AUTHORITY).rstrip('/')

app = FastAPI(
    title="Mock Azure AD OIDC Server",
    description="Emulates Azure AD for local development."
)

ISSUER = f"{BASE_URL}/{MOCK_TENANT_ID}/v2.0"

@app.get("/health")
async def health():
    """
    Health check endpoint.
    """
    return {"status": "ok"}

@app.get("/{tenant_id}/v2.0/.well-known/openid-configuration")
async def openid_configuration(tenant_id: str):
    """
    The OIDC discovery endpoint. It provides client applications with all the URLs
    and configuration required to interact with the auth server.
    """
    if tenant_id != MOCK_TENANT_ID:
        return Response(status_code=status.HTTP_404_NOT_FOUND, content="Tenant not found")

    return {
        "token_endpoint": f"{BASE_URL}/{MOCK_TENANT_ID}/oauth2/v2.0/token",
        "token_endpoint_auth_methods_supported": ["client_secret_post", "private_key_jwt", "client_secret_basic"],
        "jwks_uri": f"{BASE_URL}/{MOCK_TENANT_ID}/discovery/v2.0/keys",
        "response_modes_supported": ["query", "fragment", "form_post"],
        "subject_types_supported": ["pairwise"],
        "id_token_signing_alg_values_supported": ["RS256"],
        "response_types_supported": ["code", "id_token", "code id_token", "id_token token"],
        "scopes_supported": ["openid", "profile", "email", "offline_access"],
        "issuer": ISSUER,
        "request_uri_parameter_supported": False,
        "userinfo_endpoint": f"{BASE_URL}/v1.0/me", # Mock userinfo endpoint
        "authorization_endpoint": f"{BASE_URL}/{MOCK_TENANT_ID}/oauth2/v2.0/authorize",
        "device_authorization_endpoint": f"{BASE_URL}/{MOCK_TENANT_ID}/oauth2/v2.0/devicecode",
        "http_logout_supported": True,
        "frontchannel_logout_supported": True,
        "end_session_endpoint": f"{BASE_URL}/{MOCK_TENANT_ID}/oauth2/v2.0/logout",
        "claims_supported": ["sub", "iss", "cloud_instance_name", "cloud_instance_host_name", "cloud_graph_host_name", "msgraph_host", "aud", "exp", "iat", "auth_time", "acr", "nonce", "preferred_username", "name", "tid", "ver", "at_hash", "c_hash", "email"],
        "kerberos_endpoint": f"{BASE_URL}/{MOCK_TENANT_ID}/kerberos",
        "tenant_region_scope": "NA",
        "cloud_instance_name": "mock-cloud",
        "cloud_graph_host_name": "graph.mock.com",
        "msgraph_host": "graph.mock.com",
        "rbac_url": f"{BASE_URL}/v1.0/rbac"
    }

@app.get("/{tenant_id}/discovery/v2.0/keys")
async def get_jwks(tenant_id: str):
    """
    Serves the public key (in JWK format) that clients can use to verify
    the signature of the JWTs issued by this server.
    """
    if tenant_id != MOCK_TENANT_ID:
        return Response(status_code=status.HTTP_404_NOT_FOUND, content="Tenant not found")
    return {"keys": [key_manager.get_jwk()]}


@app.post("/{tenant_id}/oauth2/v2.0/token")
async def get_token(tenant_id: str, request: Request):
    """
    The token endpoint. In a real scenario, this would validate user credentials.
    Here, it just issues a signed JWT with mock user data.
    It can accept form data to simulate a real token request.
    """
    if tenant_id != MOCK_TENANT_ID:
        return Response(status_code=status.HTTP_404_NOT_FOUND, content="Tenant not found")

    now = int(time.time())
    user_object_id = str(uuid.uuid4())

    # These claims are structured to look like a real Azure AD access token.
    claims = {
        "aud": MOCK_CLIENT_ID,  # Audience - your app's client ID
        "iss": ISSUER,          # Issuer - must match the issuer in the OIDC config
        "iat": now,             # Issued at
        "nbf": now,             # Not before
        "exp": now + 3600,      # Expiration time (1 hour)
        "aio": str(uuid.uuid4()),
        "name": "Mock User",
        "oid": user_object_id,  # Object ID of the user
        "preferred_username": "mock.user@example.com",
        "roles": [              # Add roles here to test RBAC
            "User",
            "Admin"
        ],
        "sub": str(uuid.uuid4()), # Subject - a unique identifier for the user
        "tid": MOCK_TENANT_ID,  # Tenant ID
        "uti": str(uuid.uuid4()),
        "ver": "2.0"
    }

    # Sign the token with the private key
    token = jwt.encode(
        claims,
        key_manager.get_private_key(),
        algorithm="RS256",
        headers={"kid": key_manager.get_key_id()} # Include the Key ID in the header
    )

    return {
        "token_type": "Bearer",
        "scope": "User.Read Admin.Write",
        "expires_in": 3599,
        "ext_expires_in": 3599,
        "access_token": token
    }


@app.get("/{tenant_id}/oauth2/v2.0/authorize")
async def authorize_endpoint(tenant_id: str):
    """
    Mock authorization endpoint. In a real scenario, this would handle OAuth2 authorization flow.
    For mock purposes, returns a simple response.
    """
    if tenant_id != MOCK_TENANT_ID:
        return Response(status_code=status.HTTP_404_NOT_FOUND, content="Tenant not found")
    
    return {"message": "Mock authorization endpoint", "tenant_id": tenant_id}


@app.post("/{tenant_id}/oauth2/v2.0/devicecode")
async def device_code_endpoint(tenant_id: str):
    """
    Mock device authorization endpoint. In a real scenario, this would handle device code flow.
    For mock purposes, returns a simple response.
    """
    if tenant_id != MOCK_TENANT_ID:
        return Response(status_code=status.HTTP_404_NOT_FOUND, content="Tenant not found")
    
    return {"message": "Mock device code endpoint", "tenant_id": tenant_id}


@app.post("/{tenant_id}/oauth2/v2.0/logout")
async def logout_endpoint(tenant_id: str):
    """
    Mock logout endpoint. In a real scenario, this would handle logout flow.
    For mock purposes, returns a simple response.
    """
    if tenant_id != MOCK_TENANT_ID:
        return Response(status_code=status.HTTP_404_NOT_FOUND, content="Tenant not found")
    
    return {"message": "Mock logout endpoint", "tenant_id": tenant_id}


@app.get("/{tenant_id}/kerberos")
async def kerberos_endpoint(tenant_id: str):
    """
    Mock Kerberos endpoint. In a real scenario, this would handle Kerberos authentication.
    For mock purposes, returns a simple response.
    """
    if tenant_id != MOCK_TENANT_ID:
        return Response(status_code=status.HTTP_404_NOT_FOUND, content="Tenant not found")
    
    return {"message": "Mock Kerberos endpoint", "tenant_id": tenant_id}


if __name__ == "__main__":
    port = int(os.getenv("API_PORT", 8000))
    uvicorn.run(app, host="0.0.0.0", port=port)
