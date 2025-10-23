"""
GitLab Token Manager

This module manages the lifecycle of GitLab OAuth tokens including:
- Token storage and retrieval
- Automatic token refresh
- Token validation
- Token expiration handling
"""

from __future__ import annotations

import json
import time
from typing import Optional, Dict, Any
import structlog
import httpx
import os

logger = structlog.get_logger(__name__)

GITLAB_TOKEN_KEY_PREFIX = "ui:gitlab:"
DEFAULT_TOKEN_TTL = 7200  # 2 hours
TOKEN_REFRESH_THRESHOLD = 300  # Refresh if expiring within 5 minutes


class GitLabTokenManager:
    """
    Manages GitLab OAuth token lifecycle.
    
    This class handles storage, retrieval, validation, and automatic refresh
    of GitLab access tokens for user sessions.
    """
    
    def __init__(
        self,
        session_id: str,
        redis_client,
        gitlab_base_url: Optional[str] = None,
        client_id: Optional[str] = None,
        client_secret: Optional[str] = None,
        verify_ssl: bool = True,
        ca_cert_path: Optional[str] = None
    ):
        """
        Initialize GitLab token manager.
        
        Args:
            session_id: Unique session identifier
            redis_client: Async Redis client instance
            gitlab_base_url: GitLab instance base URL (for token refresh)
            client_id: GitLab OAuth client ID (for token refresh)
            client_secret: GitLab OAuth client secret (for token refresh)
            verify_ssl: Whether to verify SSL certificates for GitLab API calls
            ca_cert_path: Optional path to custom CA cert file
        """
        self.session_id = session_id
        self.redis_client = redis_client
        self.gitlab_base_url = (gitlab_base_url or "").rstrip("/")
        self.client_id = client_id
        self.client_secret = client_secret
        self.verify_ssl = verify_ssl
        self.ca_cert_path = (ca_cert_path or "").strip("/") if ca_cert_path else ""
        self._cache_key = f"{GITLAB_TOKEN_KEY_PREFIX}{session_id}"

        self._httpx_verify: Optional[str | bool] = self.verify_ssl
        if self.verify_ssl and self.ca_cert_path and os.path.isfile(self.ca_cert_path):
            self._httpx_verify = self.ca_cert_path
            logger.info(f"Using custom CA cert file at {self.ca_cert_path}")
        elif self.verify_ssl and self.ca_cert_path:
            logger.info(f"Custom CA bundle not found at {self.ca_cert_path}; falling back to system trust store")
        elif not self.verify_ssl:
            logger.warning("Gitlab SSL verification disabled")

        logger.debug("GitLab token manager initialized", session_id=session_id)
    
    async def save_token(
        self,
        access_token: str,
        refresh_token: Optional[str] = None,
        expires_in: Optional[int] = None
    ) -> None:
        """
        Save GitLab tokens to Redis.
        
        Args:
            access_token: GitLab access token
            refresh_token: GitLab refresh token (optional)
            expires_in: Token lifetime in seconds (optional)
        """
        try:
            expires_at = None
            if expires_in:
                expires_at = int(time.time()) + int(expires_in)
            
            token_data = {
                "access_token": access_token,
                "expires_at": expires_at,
                "updated_at": int(time.time())
            }
            
            if refresh_token:
                token_data["refresh_token"] = refresh_token
            
            # Calculate TTL for Redis
            ttl = expires_in if expires_in else DEFAULT_TOKEN_TTL
            
            await self.redis_client.set(
                self._cache_key,
                json.dumps(token_data),
                ex=int(ttl) + 600  # Add 10 minutes buffer
            )
            
            logger.info(
                "Saved GitLab token",
                session_id=self.session_id,
                expires_at=expires_at
            )
            
        except Exception as e:
            logger.error(
                "Failed to save GitLab token",
                session_id=self.session_id,
                error=str(e)
            )
            raise
    
    async def load_token(self) -> Optional[Dict[str, Any]]:
        """
        Load GitLab token data from Redis.
        
        Returns:
            Token data dictionary or None if not found
        """
        try:
            raw_data = await self.redis_client.get(self._cache_key)
            if raw_data:
                token_data = json.loads(raw_data)
                logger.debug("Loaded GitLab token from Redis", session_id=self.session_id)
                return token_data
            else:
                logger.debug("No GitLab token found in Redis", session_id=self.session_id)
                return None
                
        except json.JSONDecodeError:
            logger.warning(
                "Corrupted GitLab token data in Redis",
                session_id=self.session_id
            )
            return None
        except Exception as e:
            logger.error(
                "Failed to load GitLab token",
                session_id=self.session_id,
                error=str(e)
            )
            return None
    
    async def get_valid_token(self) -> Optional[str]:
        """
        Get a valid GitLab access token, refreshing if necessary.
        
        This method checks if the cached token is expired or expiring soon,
        and automatically refreshes it if needed.
        
        Returns:
            Valid access token or None if unavailable
        """
        token_data = await self.load_token()
        if not token_data:
            logger.debug("No GitLab token available", session_id=self.session_id)
            return None
        
        access_token = token_data.get("access_token")
        expires_at = token_data.get("expires_at")
        refresh_token = token_data.get("refresh_token")
        
        # Check if token is expired or expiring soon
        if expires_at:
            time_until_expiry = expires_at - int(time.time())
            
            if time_until_expiry <= 0:
                logger.info(
                    "GitLab token expired",
                    session_id=self.session_id,
                    expired_at=expires_at
                )
                
                if refresh_token:
                    # Attempt to refresh
                    new_token = await self.refresh_token(refresh_token)
                    return new_token
                else:
                    logger.warning("No refresh token available", session_id=self.session_id)
                    return None
            
            elif time_until_expiry <= TOKEN_REFRESH_THRESHOLD:
                logger.info(
                    "GitLab token expiring soon, refreshing",
                    session_id=self.session_id,
                    time_until_expiry=time_until_expiry
                )
                
                if refresh_token:
                    # Proactive refresh
                    new_token = await self.refresh_token(refresh_token)
                    return new_token if new_token else access_token
        
        return access_token
    
    async def refresh_token(self, refresh_token: str) -> Optional[str]:
        """
        Refresh GitLab access token using refresh token.
        
        Per GitLab OAuth docs when using PKCE, the refresh token request should
        include the original code_verifier parameter. However, since Authlib manages
        PKCE internally and we don't have access to the original code_verifier,
        we proceed without it. GitLab should accept this for refresh flows.
        
        Args:
            refresh_token: GitLab refresh token
            
        Returns:
            New access token or None if refresh failed
        """
        if not self.gitlab_base_url or not self.client_id or not self.client_secret:
            logger.error(
                "GitLab OAuth credentials not configured, cannot refresh token",
                session_id=self.session_id
            )
            return None
        
        try:
            token_url = f"{self.gitlab_base_url}/oauth/token"
            
            # Per GitLab docs: https://docs.gitlab.com/api/oauth2/#refresh-access-token
            data = {
                "grant_type": "refresh_token",
                "refresh_token": refresh_token,
                "client_id": self.client_id,
                "client_secret": self.client_secret
            }
            
            async with httpx.AsyncClient(verify=self.verify_ssl) as client:
                response = await client.post(token_url, data=data)
                
                if response.status_code == 200:
                    token_response = response.json()
                    new_access_token = token_response.get("access_token")
                    new_refresh_token = token_response.get("refresh_token")
                    expires_in = token_response.get("expires_in")
                    
                    # Save refreshed token
                    await self.save_token(
                        access_token=new_access_token,
                        refresh_token=new_refresh_token or refresh_token,
                        expires_in=expires_in
                    )
                    
                    logger.info(
                        "Successfully refreshed GitLab token",
                        session_id=self.session_id
                    )
                    
                    return new_access_token
                else:
                    logger.error(
                        "Failed to refresh GitLab token",
                        session_id=self.session_id,
                        status_code=response.status_code,
                        response=response.text
                    )
                    return None
                    
        except Exception as e:
            logger.error(
                "Exception during GitLab token refresh",
                session_id=self.session_id,
                error=str(e)
            )
            return None
    
    async def validate_token(self, access_token: str) -> bool:
        """
        Validate GitLab access token using /oauth/token/info endpoint.
        
        This is more efficient than calling /api/v4/user as it's specifically
        designed for token validation.
        
        Per GitLab docs: https://docs.gitlab.com/api/oauth2/#retrieve-the-token-information
        
        Args:
            access_token: GitLab access token to validate
            
        Returns:
            True if token is valid, False otherwise
        """
        if not self.gitlab_base_url:
            logger.warning("GitLab base URL not configured, cannot validate token")
            return False
        
        try:
            # Use /oauth/token/info for efficient token validation
            token_info_url = f"{self.gitlab_base_url}/oauth/token/info"
            
            # Can pass token as param or in Authorization header
            headers = {"Authorization": f"Bearer {access_token}"}
            
            async with httpx.AsyncClient(verify=self.verify_ssl) as client:
                response = await client.get(token_info_url, headers=headers)
                
                if response.status_code == 200:
                    token_info = response.json()
                    logger.debug(
                        "GitLab token validated successfully",
                        scopes=token_info.get("scope", []),
                        resource_owner_id=token_info.get("resource_owner_id")
                    )
                    return True
                else:
                    logger.warning(
                        "GitLab token validation failed",
                        status_code=response.status_code
                    )
                    return False
                    
        except Exception as e:
            logger.error(
                "Exception during GitLab token validation",
                error=str(e)
            )
            return False
    
    async def revoke_token(self) -> bool:
        """
        Revoke GitLab token on the GitLab server.
        
        Uses the /oauth/revoke endpoint to properly revoke the token server-side.
        Per GitLab docs: https://docs.gitlab.com/api/oauth2/#revoke-a-token
        
        Returns:
            True if successfully revoked, False otherwise
        """
        if not self.gitlab_base_url or not self.client_id or not self.client_secret:
            logger.warning(
                "GitLab OAuth credentials not configured, cannot revoke token",
                session_id=self.session_id
            )
            return False
        
        try:
            token_data = await self.load_token()
            if not token_data or not token_data.get("access_token"):
                logger.debug("No token to revoke", session_id=self.session_id)
                return True
            
            revoke_url = f"{self.gitlab_base_url}/oauth/revoke"
            
            # Per GitLab docs, revoke endpoint requires client credentials and token
            data = {
                "client_id": self.client_id,
                "client_secret": self.client_secret,
                "token": token_data["access_token"]
            }
            
            async with httpx.AsyncClient(verify=self.verify_ssl) as client:
                response = await client.post(revoke_url, data=data)
                
                # GitLab returns 200 with empty JSON on success
                if response.status_code == 200:
                    logger.info(
                        "Successfully revoked GitLab token",
                        session_id=self.session_id
                    )
                    return True
                else:
                    logger.warning(
                        "Failed to revoke GitLab token",
                        session_id=self.session_id,
                        status_code=response.status_code,
                        response=response.text
                    )
                    return False
                    
        except Exception as e:
            logger.error(
                "Exception during GitLab token revocation",
                session_id=self.session_id,
                error=str(e)
            )
            return False
    
    async def clear_token(self) -> None:
        """
        Clear GitLab token from Redis.
        
        This method is called during logout. For proper token revocation,
        call revoke_token() before this method.
        """
        try:
            await self.redis_client.delete(self._cache_key)
            logger.info("Cleared GitLab token", session_id=self.session_id)
        except Exception as e:
            logger.error(
                "Failed to clear GitLab token",
                session_id=self.session_id,
                error=str(e)
            )


async def get_gitlab_token_manager(
    session_id: str,
    redis_client,
    gitlab_base_url: Optional[str] = None,
    client_id: Optional[str] = None,
    client_secret: Optional[str] = None,
    verify_ssl: bool = True
) -> GitLabTokenManager:
    """
    Factory function to create GitLab token manager.
    
    Args:
        session_id: Unique session identifier
        redis_client: Async Redis client instance
        gitlab_base_url: GitLab instance base URL
        client_id: GitLab OAuth client ID
        client_secret: GitLab OAuth client secret
        verify_ssl: Whether to verify SSL certificates
        
    Returns:
        GitLabTokenManager instance
        
    Example:
        ```python
        manager = await get_gitlab_token_manager(
            session_id="session123",
            redis_client=redis,
            gitlab_base_url="https://gitlab.example.com",
            client_id="client_id",
            client_secret="client_secret"
        )
        token = await manager.get_valid_token()
        ```
    """
    return GitLabTokenManager(
        session_id=session_id,
        redis_client=redis_client,
        gitlab_base_url=gitlab_base_url,
        client_id=client_id,
        client_secret=client_secret,
        verify_ssl=verify_ssl
    )
