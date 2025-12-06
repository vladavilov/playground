"""Unified token exchange router.

Provides endpoints for:
- Exchanging Azure AD tokens for LOCAL JWTs
- Validating LOCAL JWTs
- Getting user info from LOCAL JWTs

Both UI Service (browser) and MCP Server (VS Code Copilot) use the same mechanism.
"""

from fastapi import APIRouter, HTTPException, Header, status
from pydantic import BaseModel, Field
import structlog

from services.token_service import TokenService

logger = structlog.get_logger(__name__)
router = APIRouter()

# Initialize token service
_token_service: TokenService | None = None


def get_token_service() -> TokenService:
    """Get or create token service singleton."""
    global _token_service
    if _token_service is None:
        _token_service = TokenService()
    return _token_service


class TokenExchangeRequest(BaseModel):
    """Request model for token exchange.
    
    Both UI Service and MCP Server use this same request format.
    Azure AD authentication is REQUIRED - there is no service account fallback.
    """
    
    access_token: str = Field(
        ...,
        description="Azure AD access token to exchange for LOCAL JWT"
    )


class TokenExchangeResponse(BaseModel):
    """Response model for token exchange."""
    
    access_token: str = Field(..., description="LOCAL JWT for backend service authentication")
    token_type: str = Field(default="Bearer", description="Token type")
    expires_in: int = Field(..., description="Token TTL in seconds")
    user_id: str = Field(..., description="Authenticated user ID (oid)")
    username: str | None = Field(None, description="Authenticated username")
    roles: list[str] = Field(default_factory=list, description="User roles")


class UserInfoResponse(BaseModel):
    """Response model for user info endpoint."""
    
    authenticated: bool = Field(..., description="Whether the token is valid")
    user_id: str | None = Field(None, description="User ID (oid)")
    username: str | None = Field(None, description="Username (preferred_username)")
    roles: list[str] = Field(default_factory=list, description="User roles")
    tenant_id: str | None = Field(None, description="Azure AD tenant ID")


@router.post("/exchange", response_model=TokenExchangeResponse)
async def exchange_token(request: TokenExchangeRequest) -> TokenExchangeResponse:
    """
    Exchange an Azure AD access token for a LOCAL JWT.
    
    This is the ONLY token exchange endpoint. Both UI Service and MCP Server
    use this same endpoint after authenticating with Azure AD.
    
    Flow:
    1. Client authenticates with Azure AD (browser or VS Code)
    2. Client receives Azure AD access token
    3. Client calls this endpoint with the access token
    4. This service validates the token and returns a LOCAL JWT
    5. Client uses LOCAL JWT for all backend service calls
    
    Args:
        request: Token exchange request with Azure AD access token
        
    Returns:
        TokenExchangeResponse with LOCAL JWT and user info
        
    Raises:
        HTTPException 401: If token validation fails
    """
    service = get_token_service()
    
    logger.info("Token exchange requested")
    
    try:
        result = await service.exchange_azure_token(
            token=request.access_token
        )
        
        logger.info(
            "Token exchange successful",
            user_id=result["user_id"],
            username=result.get("username")
        )
        
        return TokenExchangeResponse(
            access_token=result["access_token"],
            token_type="Bearer",
            expires_in=result["expires_in"],
            user_id=result["user_id"],
            username=result.get("username"),
            roles=result.get("roles", [])
        )
        
    except ValueError as e:
        logger.warning("Token exchange failed - validation error", error=str(e))
        raise HTTPException(
            status_code=status.HTTP_401_UNAUTHORIZED,
            detail=str(e)
        )


@router.get("/userinfo", response_model=UserInfoResponse)
async def get_userinfo(
    authorization: str = Header(..., description="Bearer token: 'Bearer <LOCAL_JWT>'")
) -> UserInfoResponse:
    """
    Get user information from a LOCAL JWT.
    
    This endpoint validates the LOCAL JWT and returns the authenticated user's
    information. It centralizes JWT validation logic in authentication-service.
    
    UI Service calls this endpoint for /auth/me functionality.
    
    Args:
        authorization: Authorization header with Bearer token
        
    Returns:
        UserInfoResponse with user info if authenticated
        
    Raises:
        HTTPException 401: If token is missing, invalid, or expired
    """
    # Extract token from Authorization header
    if not authorization.lower().startswith("bearer "):
        raise HTTPException(
            status_code=status.HTTP_401_UNAUTHORIZED,
            detail="Invalid authorization header format. Expected 'Bearer <token>'"
        )
    
    token = authorization[7:].strip()  # Remove "Bearer " prefix
    
    if not token:
        raise HTTPException(
            status_code=status.HTTP_401_UNAUTHORIZED,
            detail="Missing token"
        )
    
    service = get_token_service()
    
    try:
        claims = service.validate_local_jwt(token)
        
        return UserInfoResponse(
            authenticated=True,
            user_id=claims.get("oid"),
            username=claims.get("preferred_username"),
            roles=claims.get("roles", []),
            tenant_id=claims.get("tid")
        )
        
    except Exception as e:
        logger.debug("Token validation failed", error=str(e))
        raise HTTPException(
            status_code=status.HTTP_401_UNAUTHORIZED,
            detail="Invalid or expired token"
        )


@router.post("/validate")
async def validate_token(token: str) -> dict:
    """
    Validate a LOCAL JWT token (low-level validation).
    
    For user info, prefer GET /userinfo with Authorization header.
    
    Args:
        token: LOCAL JWT to validate
        
    Returns:
        Token claims if valid
        
    Raises:
        HTTPException 401: If token is invalid or expired
    """
    service = get_token_service()
    
    try:
        claims = service.validate_local_jwt(token)
        return {
            "valid": True,
            "claims": claims
        }
    except Exception as e:
        logger.warning("Token validation failed", error=str(e))
        raise HTTPException(
            status_code=status.HTTP_401_UNAUTHORIZED,
            detail="Invalid or expired token"
        )
