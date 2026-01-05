"""Token endpoints for authentication-service.

Provides endpoints for:
- Minting service-bound S2S tokens for internal callers (/auth/s2s/mint)
- Validating Azure AD access tokens and returning OIDC userinfo (/auth/azure/userinfo)
"""

import os

from fastapi import APIRouter, HTTPException, Header, status
from pydantic import BaseModel, Field
import structlog

from services.token_service import TokenService

logger = structlog.get_logger(__name__)
router = APIRouter()

def get_token_service() -> TokenService:
    # TokenService is lightweight (lazy-loads Azure config). Avoid module-level singletons
    # to keep tests isolated and prevent cross-test pollution when TokenService is patched.
    return TokenService()


class AzureUserInfoResponse(BaseModel):
    """OIDC-like userinfo response derived from an Azure AD access token."""

    sub: str = Field(..., description="Subject (oid/sub) from Azure AD token")
    preferred_username: str | None = Field(None, description="preferred_username/email from Azure AD token")
    email: str | None = Field(None, description="Email (best-effort, may equal preferred_username)")
    roles: list[str] = Field(default_factory=list, description="roles claim (if present)")


class S2STokenMintRequest(BaseModel):
    aud: str = Field(default="internal-services", description="Target audience for the service token")


class S2STokenMintResponse(BaseModel):
    access_token: str = Field(..., description="Service-bound JWT for internal service calls")
    token_type: str = Field(default="Bearer", description="Token type")
    expires_in: int = Field(..., description="Token TTL in seconds")


@router.post("/s2s/mint", response_model=S2STokenMintResponse)
async def mint_s2s_token(
    request: S2STokenMintRequest,
    x_api_gateway_secret: str | None = Header(None, alias="X-Api-Gateway-Secret"),
) -> S2STokenMintResponse:
    """
    Mint a service-bound JWT for the API gateway (POC caller auth via shared secret).
    """
    expected = (os.getenv("API_GATEWAY_MINT_SECRET") or "").strip()
    provided = (x_api_gateway_secret or "").strip()

    if not expected or not provided or provided != expected:
        raise HTTPException(status_code=status.HTTP_401_UNAUTHORIZED, detail="Unauthorized")

    service = get_token_service()
    result = service.mint_gateway_service_token(aud=request.aud)

    return S2STokenMintResponse(
        access_token=result["access_token"],
        token_type="Bearer",
        expires_in=result["expires_in"],
    )

@router.get("/azure/userinfo", response_model=AzureUserInfoResponse)
async def azure_userinfo(
    authorization: str | None = Header(None, description="Bearer token: 'Bearer <AZURE_AD_ACCESS_TOKEN>'"),
) -> AzureUserInfoResponse:
    """
    Validate an Azure AD access token and return an OIDC-like userinfo response.

    This endpoint exists to support OAuth/OIDC user discovery for browser-based MCP clients
    without relying on the removed `/auth/exchange` "LOCAL JWT" mechanism.
    """
    raw = (authorization or "").strip()
    if not raw.lower().startswith("bearer "):
        raise HTTPException(status_code=status.HTTP_401_UNAUTHORIZED, detail="Bearer token required")

    token = raw[7:].strip()
    if not token:
        raise HTTPException(status_code=status.HTTP_401_UNAUTHORIZED, detail="Bearer token required")

    svc = get_token_service()
    try:
        claims = await svc.validate_azure_access_token(token)
    except Exception:
        raise HTTPException(status_code=status.HTTP_401_UNAUTHORIZED, detail="Invalid token")

    sub = str(claims.get("oid") or claims.get("sub") or "").strip()
    if not sub:
        raise HTTPException(status_code=status.HTTP_401_UNAUTHORIZED, detail="Token missing subject")

    username = (
        claims.get("preferred_username")
        or claims.get("email")
        or claims.get("upn")
        or None
    )
    roles = claims.get("roles") or []
    if isinstance(roles, str):
        roles = [r.strip() for r in roles.split() if r.strip()]

    return AzureUserInfoResponse(
        sub=sub,
        preferred_username=username,
        email=username,
        roles=roles if isinstance(roles, list) else [],
    )
