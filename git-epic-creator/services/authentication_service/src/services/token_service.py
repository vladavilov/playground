"""Token service for Azure AD token validation and S2S token minting."""

import time
from typing import Any

import httpx
import structlog
from jose import JWTError, jwt

from configuration.azure_auth_config import get_azure_auth_settings
from utils.jwt_utils import sign_jwt

logger = structlog.get_logger(__name__)

GATEWAY_SERVICE_TOKEN_TTL_SECONDS = 600  # 10 minutes


class TokenService:
    """
    Unified token service for authentication.
    
    Validates Azure AD tokens and mints LOCAL JWTs for service-to-service
    authentication within the microservices ecosystem.
    """
    
    def __init__(self):
        # Lazy-load Azure settings so callers that only mint S2S tokens (e.g., Envoy ext_authz
        # in cookie-session mode) don't require Azure env vars.
        self._azure_settings = None
        self._jwks_cache: dict[str, Any] | None = None
        self._jwks_cache_time: float = 0
        self._jwks_cache_ttl: float = 300  # 5 minutes

    def _get_azure_settings(self):
        if self._azure_settings is None:
            self._azure_settings = get_azure_auth_settings()
        return self._azure_settings
    
    async def validate_azure_access_token(self, token: str) -> dict[str, Any]:
        """
        Validate an Azure AD access token and return its claims.

        This is used by Envoy `ext_authz` for non-browser callers (e.g., MCP via Envoy)
        that authenticate with a Bearer Azure AD token instead of a cookie session.
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
    
    def mint_gateway_service_token(self, aud: str = "internal-services") -> dict[str, Any]:
        """
        Mint a service-bound JWT for the API gateway to call internal services.

        Contract:
        - sub: "api-gateway"
        - iss: "authentication-service"
        - aud: caller-provided audience (default: "internal-services")
        - iat/exp: set by jwt_utils.sign_jwt()
        - no user claims (oid/roles/etc.)
        """
        claims = {
            "sub": "api-gateway",
            "iss": "authentication-service",
            "aud": aud,
        }
        token = sign_jwt(claims, expires_in_seconds=GATEWAY_SERVICE_TOKEN_TTL_SECONDS)
        return {"access_token": token, "expires_in": GATEWAY_SERVICE_TOKEN_TTL_SECONDS}
    
    
        
    
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
        azure = self._get_azure_settings()
        jwks_url = f"{azure.AZURE_AD_AUTHORITY}/{azure.AZURE_TENANT_ID}/discovery/v2.0/keys"
        
        # Fetch JWKS
        try:
            async with httpx.AsyncClient(verify=azure.AZURE_AD_VERIFY_SSL) as client:
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
