"""FastAPI dependencies for GitLab Client Service."""

import structlog
import gitlab
import redis.asyncio as redis
from fastapi import Request, HTTPException, status, Depends, Header

from services.gitlab_token_manager import get_token, clear_token, is_token_expired, exchange_token_for_user
from utils.redis_client import get_redis_client
from config import GitLabClientSettings, get_gitlab_client_settings
from utils.local_auth import get_gateway_service_verified, LocalServiceCaller

logger = structlog.get_logger(__name__)

require_gateway_verified = get_gateway_service_verified()


def get_x_user_id(x_user_id: str = Header(..., alias="x-user-id")) -> str:
    """Extract user identity propagated by the gateway."""
    if not x_user_id or not str(x_user_id).strip():
        raise HTTPException(status_code=status.HTTP_400_BAD_REQUEST, detail="Missing x-user-id header")
    return str(x_user_id)


def get_gitlab_client(
    gitlab_token: str,
    settings: GitLabClientSettings
) -> gitlab.Gitlab:
    """
    Create and configure a GitLab client instance.
    
    Args:
        gitlab_token: GitLab OAuth access token for authentication
        settings: GitLab client settings
        
    Returns:
        Configured python-gitlab Gitlab client
    """
    # Configure SSL verification
    ssl_verify: bool | str = True  # Default: use system CA bundle
    if settings.GITLAB_CA_CERT_PATH:
        # Use custom CA certificate for internal GitLab instances
        ssl_verify = settings.GITLAB_CA_CERT_PATH
    elif not settings.GITLAB_VERIFY_SSL:
        # Disable SSL verification (development only)
        ssl_verify = False
    
    client = gitlab.Gitlab(
        url=settings.GITLAB_BASE_URL,
        oauth_token=gitlab_token,
        timeout=settings.HTTP_CONNECTION_TIMEOUT,
        retry_transient_errors=True,
        ssl_verify=ssl_verify,
    )
    
    logger.debug(
        "GitLab client created with OAuth authentication",
        gitlab_url=settings.GITLAB_BASE_URL,
        timeout=settings.HTTP_CONNECTION_TIMEOUT
    )
    
    return client


def get_redis_client_dep() -> redis.Redis:
    """
    Dependency for injecting Redis client.
    
    Reuses shared library implementation for consistency.
    
    Returns:
        Redis client instance (thread-local cached)
    """
    return get_redis_client()


async def get_gitlab_client_dep(
    request: Request,
    _caller: LocalServiceCaller = Depends(require_gateway_verified),
    user_id: str = Depends(get_x_user_id),
    settings: GitLabClientSettings = Depends(get_gitlab_client_settings),
    redis_client: redis.Redis = Depends(get_redis_client_dep)
) -> gitlab.Gitlab:
    """
    FastAPI dependency for injecting configured GitLab client.
    
    Uses user-based authentication: looks up GitLab token from Redis
    using user_id (oid) extracted from S2S JWT.
    
    Validates token expiry and attempts to refresh if expired.
    
    Args:
        request: FastAPI request object (for accessing app state)
        user_id: Azure AD user object ID (oid) extracted from JWT Authorization header
        settings: GitLab client settings
        redis_client: Redis client for token lookup
        
    Returns:
        Configured GitLab client authenticated with user's token
        
    Raises:
        HTTPException: If GitLab token not found, expired without refresh token, or refresh fails
    """
    
    
    # Load token from Redis
    token_data = await get_token(user_id, redis_client)
    
    if not token_data or not token_data.get("access_token"):
        logger.warning("GitLab token not found for user", user_id=user_id)
        raise HTTPException(
            status_code=status.HTTP_401_UNAUTHORIZED,
            detail="GitLab not connected. Please authenticate with GitLab."
        )
    
    # Check if token is expired or about to expire
    if is_token_expired(token_data):
        logger.warning(
            "GitLab token expired or expiring soon",
            user_id=user_id,
            has_refresh_token=bool(token_data.get('refresh_token'))
        )
        
        # Attempt to refresh token if refresh_token is available
        refresh_token = token_data.get('refresh_token')
        if refresh_token:
            try:
                # Get OAuth client from app state
                oauth = getattr(request.app.state, "oauth", None)
                if not oauth or not hasattr(oauth, 'gitlab'):
                    logger.error("OAuth client not available for token refresh", user_id=user_id)
                    raise HTTPException(
                        status_code=status.HTTP_401_UNAUTHORIZED,
                        detail="GitLab token expired. Please re-authenticate."
                    )
                
                logger.info("Attempting to refresh GitLab token", user_id=user_id)
                
                # Refresh token using shared function (DRY principle)
                token_data = await exchange_token_for_user(
                    user_id=user_id,
                    redis_client=redis_client,
                    oauth_client=oauth.gitlab,
                    grant_type='refresh_token',
                    refresh_token=refresh_token
                )
                
                logger.info("GitLab token refreshed successfully", user_id=user_id)
                    
            except HTTPException:
                raise
            except Exception as e:
                logger.error(
                    "Token refresh failed",
                    user_id=user_id,
                    error=str(e),
                    error_type=type(e).__name__
                )
                # Clear expired token from Redis
                await clear_token(user_id, redis_client)
                raise HTTPException(
                    status_code=status.HTTP_401_UNAUTHORIZED,
                    detail="GitLab token expired and refresh failed. Please re-authenticate."
                )
        else:
            # No refresh token available - user must re-authenticate
            logger.warning("GitLab token expired and no refresh token available", user_id=user_id)
            await clear_token(user_id, redis_client)
            raise HTTPException(
                status_code=status.HTTP_401_UNAUTHORIZED,
                detail="GitLab token expired. Please re-authenticate with GitLab."
            )
    
    gitlab_token = token_data["access_token"]
    
    logger.debug(
        "GitLab client created for user",
        user_id=user_id,
        token_expires_at=token_data.get('created_at', 0) + token_data.get('expires_in', 0),
        has_refresh_token=bool(token_data.get('refresh_token'))
    )
    
    return get_gitlab_client(gitlab_token, settings)


