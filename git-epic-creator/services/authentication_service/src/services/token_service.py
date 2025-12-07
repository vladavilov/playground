"""Token service for Azure AD token validation and LOCAL JWT minting.

Provides unified authentication:
- Azure AD token validation (via JWKS)
- LOCAL JWT minting for backend services
"""

import time
from typing import Any
import httpx
import structlog
from jose import jwt, JWTError

from utils.jwt_utils import sign_jwt, verify_jwt
from configuration.azure_auth_config import get_azure_auth_settings

logger = structlog.get_logger(__name__)

# Default TTL for S2S tokens (matches jwt_utils.sign_jwt default)
DEFAULT_S2S_TOKEN_TTL_SECONDS = 3600  # 1 hour


class TokenService:
    """
    Unified token service for authentication.
    
    Validates Azure AD tokens and mints LOCAL JWTs for service-to-service
    authentication within the microservices ecosystem.
    """
    
    def __init__(self):
        self._azure_settings = get_azure_auth_settings()
        self._jwks_cache: dict[str, Any] | None = None
        self._jwks_cache_time: float = 0
        self._jwks_cache_ttl: float = 300  # 5 minutes
    
    async def exchange_azure_token(self, token: str) -> dict[str, Any]:
        """
        Exchange an Azure AD access token for a LOCAL JWT.
        
        This is the ONLY authentication flow. Both UI Service and MCP Server
        authenticate via Azure AD.
        
        Args:
            token: Azure AD access token
            
        Returns:
            Dict with access_token, expires_in, user_id, username, roles
            
        Raises:
            ValueError: If token validation fails
        """
        # Validate Azure AD token
        claims = await self._validate_azure_ad_token(token)
        
        # Extract standardized claims
        user_id = str(claims.get("oid") or claims.get("sub") or "")
        if not user_id:
            raise ValueError("Token missing user identifier (oid/sub)")
        
        username = (
            claims.get("preferred_username") or
            claims.get("email") or
            claims.get("upn") or
            None
        )
        
        roles = claims.get("roles") or []
        if isinstance(roles, str):
            roles = [r.strip() for r in roles.split() if r.strip()]
        
        tid = claims.get("tid") or ""
        
        # Mint LOCAL JWT
        local_jwt = self._mint_local_jwt(
            oid=user_id,
            preferred_username=username,
            roles=roles,
            tid=tid,
            source="azure_ad_exchange"
        )
        
        return {
            "access_token": local_jwt,
            "expires_in": DEFAULT_S2S_TOKEN_TTL_SECONDS,
            "user_id": user_id,
            "username": username,
            "roles": roles
        }
    
    def validate_local_jwt(self, token: str) -> dict[str, Any]:
        """
        Validate a LOCAL JWT token.
        
        Args:
            token: LOCAL JWT to validate
            
        Returns:
            Token claims
            
        Raises:
            ValueError: If token is invalid
        """
        try:
            claims = verify_jwt(token, verify_exp=True)
            return claims
        except Exception as e:
            raise ValueError(f"Invalid LOCAL JWT: {e}")
    
    def _mint_local_jwt(
        self,
        oid: str,
        preferred_username: str | None,
        roles: list[str],
        tid: str | None,
        source: str
    ) -> str:
        """
        Mint a LOCAL JWT with standardized claims.
        
        Args:
            oid: User object ID
            preferred_username: User's preferred username
            roles: User roles
            tid: Tenant ID
            source: Token source for auditing
            
        Returns:
            Signed LOCAL JWT
        """
        claims = {
            # Identity claims
            "oid": oid,
            "preferred_username": preferred_username,
            "roles": roles,
            "tid": tid or "",
            
            # Token metadata
            "iss": "authentication-service",
            "aud": "backend-services",
            
            # Audit metadata
            "source": source,
        }
        
        # Use shared jwt_utils for signing with default TTL
        return sign_jwt(claims, expires_in_seconds=DEFAULT_S2S_TOKEN_TTL_SECONDS)
    
    async def _validate_azure_ad_token(self, token: str) -> dict[str, Any]:
        """
        Validate Azure AD token using JWKS.
        
        Args:
            token: Azure AD JWT to validate
            
        Returns:
            Token claims
            
        Raises:
            ValueError: If validation fails
        """
        try:
            # Decode header to get key ID
            unverified_header = jwt.get_unverified_header(token)
            kid = unverified_header.get("kid")
            
            if not kid:
                raise ValueError("Token missing key ID (kid)")
            
            # Get JWKS
            jwks = await self._get_jwks()
            
            # Find matching key
            rsa_key = None
            for key in jwks.get("keys", []):
                if key.get("kid") == kid:
                    rsa_key = key
                    break
            
            if not rsa_key:
                raise ValueError(f"Unable to find matching key for kid: {kid}")
            
            # Verify token
            # Note: Audience verification is DISABLED to support MCP clients like VS Code
            # that use dynamically registered client_ids different from AZURE_CLIENT_ID.
            # The mock auth service auto-registers these clients, resulting in tokens
            # with varying audience claims.
            claims = jwt.decode(
                token,
                rsa_key,
                algorithms=["RS256"],
                options={
                    "verify_aud": False,  # Disabled for MCP client compatibility
                    "verify_exp": True
                }
            )
            
            logger.debug(
                "Azure AD token validated",
                oid=claims.get("oid"),
                preferred_username=claims.get("preferred_username")
            )
            
            return claims
            
        except JWTError as e:
            logger.warning("Azure AD token validation failed", error=str(e))
            raise ValueError(f"Invalid Azure AD token: {e}")
        except Exception as e:
            logger.error("Azure AD token validation error", error=str(e))
            raise ValueError(f"Token validation error: {e}")
    
    async def _get_jwks(self) -> dict[str, Any]:
        """
        Get JWKS from Azure AD (or mock auth service) with caching.
        
        Returns:
            JWKS dictionary
            
        Raises:
            ValueError: If JWKS retrieval fails
        """
        now = time.time()
        
        # Check cache
        if self._jwks_cache and (now - self._jwks_cache_time) < self._jwks_cache_ttl:
            return self._jwks_cache
        
        # Build JWKS URL
        jwks_url = (
            f"{self._azure_settings.AZURE_AD_AUTHORITY}/"
            f"{self._azure_settings.AZURE_TENANT_ID}/discovery/v2.0/keys"
        )
        
        # Fetch JWKS
        try:
            async with httpx.AsyncClient(verify=self._azure_settings.AZURE_AD_VERIFY_SSL) as client:
                response = await client.get(jwks_url, timeout=10.0)
                response.raise_for_status()
                
                self._jwks_cache = response.json()
                self._jwks_cache_time = now
                
                logger.debug("JWKS fetched and cached", url=jwks_url)
                return self._jwks_cache
                
        except Exception as e:
            logger.error("Failed to fetch JWKS", error=str(e))
            
            # Return cached JWKS if available (stale cache is better than failure)
            if self._jwks_cache:
                logger.warning("Using stale JWKS cache")
                return self._jwks_cache
            
            raise ValueError(f"Failed to fetch JWKS: {e}")
