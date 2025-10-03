"""FastAPI dependencies for GitLab Client Service."""

import structlog
import gitlab
import redis.asyncio as redis
from fastapi import Request, HTTPException, status, Depends

from utils.redis_client import get_redis_client
from config import GitLabClientSettings, get_gitlab_client_settings

logger = structlog.get_logger(__name__)


def get_gitlab_access_token(request: Request) -> str:
    """
    Extract GitLab access token from request header.
    
    Args:
        request: FastAPI request object
        
    Returns:
        GitLab access token
        
    Raises:
        HTTPException: If header is missing or empty
    """
    token = request.headers.get("GitLab-Access-Token", "").strip()
    
    if not token:
        logger.warning("GitLab access token missing in request")
        raise HTTPException(
            status_code=status.HTTP_401_UNAUTHORIZED,
            detail="GitLab-Access-Token header required"
        )
    
    return token


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
        timeout=settings.HTTP_TIMEOUT_SEC,
        retry_transient_errors=True,
        ssl_verify=settings.GITLAB_VERIFY_SSL,
    )
    
    logger.debug(
        "GitLab client created",
        gitlab_url=settings.GITLAB_BASE_URL,
        timeout=settings.HTTP_TIMEOUT_SEC
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


def get_gitlab_client_dep(
    gitlab_token: str = Depends(get_gitlab_access_token),
    settings: GitLabClientSettings = Depends(get_gitlab_client_settings)
) -> gitlab.Gitlab:
    """
    FastAPI dependency for injecting configured GitLab client.
    
    This combines token extraction and client creation for use in endpoints.
    
    Args:
        gitlab_token: Extracted from GitLab-Access-Token header
        settings: GitLab client settings
        
    Returns:
        Configured GitLab client
    """
    return get_gitlab_client(gitlab_token, settings)


