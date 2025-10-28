"""FastAPI dependencies for GitLab Client Service."""

import structlog
import gitlab
import redis.asyncio as redis
from fastapi import Request, HTTPException, status, Depends

from utils.redis_client import get_redis_client
from config import GitLabClientSettings, get_gitlab_client_settings

logger = structlog.get_logger(__name__)



def get_session_id_from_jwt(request: Request) -> str:
    """
    Extract session_id from S2S JWT token.
    
    Expects JWT in Authorization header with claim 'oid', 'session_id', or 'sid'.
    This enables session-based authentication where GitLab tokens are stored
    in Redis and looked up by session ID.
    
    Args:
        request: FastAPI request object
        
    Returns:
        Session ID extracted from JWT claims
        
    Raises:
        HTTPException: If JWT is invalid or missing session_id claim
    """
    auth_header = request.headers.get("Authorization", "")
    if not auth_header.startswith("Bearer "):
        logger.warning("Missing or invalid Authorization header")
        raise HTTPException(
            status_code=status.HTTP_401_UNAUTHORIZED,
            detail="Missing or invalid Authorization header"
        )
    
    token = auth_header.replace("Bearer ", "")
    
    # Import from shared library
    from utils.jwt_utils import verify_jwt
    
    try:
        claims = verify_jwt(token, verify_exp=True)
        session_id = claims.get("oid") or claims.get("session_id") or claims.get("sid")
        
        if not session_id:
            logger.warning("JWT missing session_id claim", claims=list(claims.keys()))
            raise HTTPException(
                status_code=status.HTTP_401_UNAUTHORIZED,
                detail="JWT missing session_id claim"
            )
        
        logger.debug("Extracted session_id from JWT", session_id=session_id)
        return session_id
    except HTTPException:
        raise
    except Exception as e:
        logger.error("JWT verification failed", error=str(e), exc_info=True)
        raise HTTPException(
            status_code=status.HTTP_401_UNAUTHORIZED,
            detail="Invalid JWT token"
        )


def get_gitlab_client(
    gitlab_token: str,
    settings: GitLabClientSettings
) -> gitlab.Gitlab:
    """
    Create and configure a GitLab client instance.
    
    Args:
        gitlab_token: GitLab access token for authentication
        settings: GitLab client settings
        
    Returns:
        Configured python-gitlab Gitlab client
    """
    client = gitlab.Gitlab(
        url=settings.GITLAB_BASE_URL,
        private_token=gitlab_token,
        timeout=settings.HTTP_CONNECTION_TIMEOUT,
        retry_transient_errors=True,
        ssl_verify=settings.GITLAB_VERIFY_SSL,
    )
    
    logger.debug(
        "GitLab client created",
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
    session_id: str = Depends(get_session_id_from_jwt),
    settings: GitLabClientSettings = Depends(get_gitlab_client_settings),
    redis_client: redis.Redis = Depends(get_redis_client_dep)
) -> gitlab.Gitlab:
    """
    FastAPI dependency for injecting configured GitLab client.
    
    Uses session-based authentication: looks up GitLab token from Redis
    using session_id extracted from S2S JWT.
    
    Args:
        request: FastAPI request object (for accessing app state)
        session_id: Extracted from JWT Authorization header
        settings: GitLab client settings
        redis_client: Redis client for token lookup
        
    Returns:
        Configured GitLab client authenticated with user's token
        
    Raises:
        HTTPException: If GitLab token not found in Redis or expired
    
    Note:
        Token refresh is handled automatically by Authlib when making API calls.
        The update_token callback will save refreshed tokens to Redis.
    """
    from services.gitlab_token_manager import get_token, set_session_context
    
    # Load token from Redis
    token_data = await get_token(session_id, redis_client)
    
    if not token_data or not token_data.get("access_token"):
        logger.warning("GitLab token not found for session", session_id=session_id)
        raise HTTPException(
            status_code=status.HTTP_401_UNAUTHORIZED,
            detail="GitLab not connected. Please authenticate with GitLab."
        )
    
    # Set session context for automatic token refresh callback
    set_session_context(session_id)
    
    gitlab_token = token_data["access_token"]
    
    logger.debug(
        "GitLab client created for session",
        session_id=session_id,
        token_expires_at=token_data.get('expires_at') or token_data.get('created_at', 0) + token_data.get('expires_in', 0)
    )
    
    return get_gitlab_client(gitlab_token, settings)


