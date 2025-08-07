"""
Azure token provider for service-to-service authentication using client credentials flow.
"""
from typing import Optional, Dict, Any
from datetime import datetime, timezone, timedelta
import asyncio
from dataclasses import dataclass
import httpx
import structlog
from tenacity import (
    retry,
    stop_after_attempt,
    wait_exponential,
    retry_if_exception_type
)

from configuration.azure_auth_config import get_azure_auth_settings

logger = structlog.get_logger(__name__)


@dataclass
class TokenInfo:
    """Information about an access token."""
    access_token: str
    expires_at: datetime
    token_type: str = "Bearer"

    @property
    def is_expired(self) -> bool:
        """Check if token is expired (with 5 minute buffer)."""
        buffer = timedelta(minutes=5)
        return datetime.now(timezone.utc) >= (self.expires_at - buffer)

    @property
    def authorization_header(self) -> str:
        """Get the authorization header value."""
        return f"{self.token_type} {self.access_token}"


class AzureTokenProvider:
    """
    Provides Azure AD access tokens using client credentials flow for service-to-service authentication.
    
    This class handles token acquisition, caching, and automatic refresh for Azure AD authentication
    in a microservices environment where services need to authenticate with each other.
    """

    def __init__(self):
        """Initialize the Azure token provider."""
        self.settings = get_azure_auth_settings()
        self._client: Optional[httpx.AsyncClient] = None
        self._token: Optional[TokenInfo] = None
        self._token_lock = asyncio.Lock()

    async def __aenter__(self) -> 'AzureTokenProvider':
        """Async context manager entry."""
        await self._ensure_client()
        return self

    async def __aexit__(self, exc_type, exc_val, exc_tb):
        """Async context manager exit."""
        await self.close()

    async def _ensure_client(self) -> None:
        """Ensure HTTP client is initialized."""
        if self._client is None:
            timeout = httpx.Timeout(connect=30.0, read=30.0, write=30.0, pool=30.0)
            self._client = httpx.AsyncClient(timeout=timeout)

    async def close(self) -> None:
        """Close the HTTP client and cleanup resources."""
        if self._client:
            await self._client.aclose()
            self._client = None

    @retry(
        stop=stop_after_attempt(3),
        wait=wait_exponential(multiplier=1, max=10, exp_base=2),
        retry=retry_if_exception_type((httpx.ConnectError, httpx.TimeoutException))
    )
    async def _acquire_token(self) -> TokenInfo:
        """
        Acquire a new access token using client credentials flow.
        
        Returns:
            TokenInfo: Token information including access token and expiration
            
        Raises:
            httpx.HTTPError: If token acquisition fails
            ValueError: If response format is invalid
        """
        await self._ensure_client()

        # Build token request
        token_data = {
            "grant_type": "client_credentials",
            "client_id": self.settings.AZURE_CLIENT_ID,
            "client_secret": self.settings.AZURE_CLIENT_SECRET,
            "scope": self.settings.SCOPE_NAME
        }

        logger.info(
            "Requesting Azure access token",
            client_id=self.settings.AZURE_CLIENT_ID,
            scope=self.settings.SCOPE_NAME,
            token_url=self.settings.OPENAPI_TOKEN_URL
        )

        response = await self._client.post(
            self.settings.OPENAPI_TOKEN_URL,
            data=token_data,
            headers={"Content-Type": "application/x-www-form-urlencoded"}
        )

        if response.status_code != 200:
            error_msg = f"Token acquisition failed with status {response.status_code}"
            try:
                error_data = response.json()
                if "error" in error_data:
                    error_msg += f": {error_data['error']}"
                if "error_description" in error_data:
                    error_msg += f" - {error_data['error_description']}"
            except (ValueError, TypeError):
                error_msg += f": {response.text}"

            logger.error(
                "Failed to acquire Azure access token",
                status_code=response.status_code,
                error=error_msg
            )
            raise httpx.HTTPStatusError(error_msg, request=response.request, response=response)

        try:
            token_response = response.json()
            access_token = token_response["access_token"]
            expires_in = token_response.get("expires_in", 3600)  # Default to 1 hour
            token_type = token_response.get("token_type", "Bearer")

            # Calculate expiration time
            expires_at = datetime.now(timezone.utc) + timedelta(seconds=expires_in)

            logger.info(
                "Azure access token acquired successfully",
                expires_in=expires_in,
                expires_at=expires_at.isoformat()
            )

            return TokenInfo(
                access_token=access_token,
                expires_at=expires_at,
                token_type=token_type
            )

        except (KeyError, ValueError, TypeError) as e:
            error_msg = f"Invalid token response format: {e}"
            logger.error("Failed to parse token response", error=error_msg, response=response.text)
            raise ValueError(error_msg) from e

    async def get_access_token(self) -> str:
        """
        Get a valid access token, acquiring or refreshing as needed.
        
        This method handles token caching and automatic refresh. It's thread-safe
        and can be called concurrently from multiple coroutines.
        
        Returns:
            str: Valid access token
            
        Raises:
            httpx.HTTPError: If token acquisition fails
            ValueError: If token response is invalid
        """
        async with self._token_lock:
            # Check if we have a valid cached token
            if self._token and not self._token.is_expired:
                logger.debug("Using cached Azure access token")
                return self._token.access_token

            # Acquire new token
            logger.info("Acquiring new Azure access token")
            self._token = await self._acquire_token()
            return self._token.access_token

    async def get_authorization_header(self) -> Dict[str, str]:
        """
        Get authorization headers with a valid access token.
        
        Returns:
            Dict[str, str]: Authorization headers ready for HTTP requests
            
        Raises:
            httpx.HTTPError: If token acquisition fails
            ValueError: If token response is invalid
        """
        async with self._token_lock:
            # Check if we have a valid cached token
            if self._token and not self._token.is_expired:
                logger.debug("Using cached Azure access token for authorization header")
                return {"Authorization": self._token.authorization_header}

            # Acquire new token
            logger.info("Acquiring new Azure access token for authorization header")
            self._token = await self._acquire_token()
            return {"Authorization": self._token.authorization_header}

    def invalidate_token(self) -> None:
        """
        Invalidate the current cached token.
        
        This forces the next token request to acquire a fresh token.
        Useful when receiving 401 responses that might indicate token expiration.
        """
        logger.info("Invalidating cached Azure access token")
        self._token = None