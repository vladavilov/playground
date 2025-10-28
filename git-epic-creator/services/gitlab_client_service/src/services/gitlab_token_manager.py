"""
GitLab Token Manager - Redis-based Token Storage for GitLab OAuth

This module manages GitLab OAuth tokens in Redis, providing storage,
retrieval, and clearing capabilities for session-based authentication.

Integrates with Authlib's automatic token refresh via update_token callback.

Redis key pattern: gitlab:oauth:{session_id}
"""

from __future__ import annotations

import json
import structlog
from typing import Optional, Dict, Any, Callable

logger = structlog.get_logger(__name__)

TOKEN_KEY_PREFIX = "gitlab:oauth:"
DEFAULT_TTL = 7200  # 2 hours

# Global storage for session_id context during token updates
_current_session_id: Optional[str] = None


async def get_token(
    session_id: str,
    redis_client
) -> Optional[Dict[str, Any]]:
    """
    Load GitLab OAuth token from Redis.
    
    Args:
        session_id: User session identifier
        redis_client: Async Redis client
        
    Returns:
        Token dictionary or None if not found/expired
        
    Token format:
        {
            'access_token': str,
            'token_type': str,
            'expires_in': int,
            'refresh_token': str (optional),
            'scope': str,
            'created_at': int (timestamp)
        }
    """
    try:
        key = f"{TOKEN_KEY_PREFIX}{session_id}"
        data = await redis_client.get(key)
        
        if data:
            token = json.loads(data)
            logger.debug("Loaded GitLab OAuth token from Redis", session_id=session_id)
            return token
        else:
            logger.debug("No GitLab OAuth token found in Redis", session_id=session_id)
            return None
            
    except json.JSONDecodeError:
        logger.warning("Corrupted token data in Redis", session_id=session_id)
        return None
    except Exception as e:
        logger.error("Failed to load GitLab OAuth token", session_id=session_id, error=str(e))
        return None


async def save_token(
    session_id: str,
    redis_client,
    token: Dict[str, Any]
) -> None:
    """
    Save OAuth token to Redis.
    
    Args:
        session_id: User session identifier
        redis_client: Async Redis client
        token: Token dictionary from Authlib OAuth flow
        
    Raises:
        Exception: If Redis operation fails
    """
    try:
        key = f"{TOKEN_KEY_PREFIX}{session_id}"
        ttl = token.get('expires_in', DEFAULT_TTL)
        
        # Store token with TTL plus buffer for refresh window
        await redis_client.set(
            key,
            json.dumps(token),
            ex=int(ttl) + 600  # 10 minute buffer
        )
        
        logger.info(
            "Saved GitLab OAuth token",
            session_id=session_id,
            expires_in=ttl,
            has_refresh_token=bool(token.get('refresh_token'))
        )
        
    except Exception as e:
        logger.error("Failed to save GitLab OAuth token", session_id=session_id, error=str(e))
        raise


async def clear_token(
    session_id: str,
    redis_client
) -> None:
    """
    Clear OAuth token from Redis.
    
    Called during logout or explicit GitLab account disconnection.
    
    Args:
        session_id: User session identifier
        redis_client: Async Redis client
    """
    try:
        key = f"{TOKEN_KEY_PREFIX}{session_id}"
        await redis_client.delete(key)
        logger.info("Cleared GitLab OAuth token", session_id=session_id)
        
    except Exception as e:
        logger.error("Failed to clear GitLab OAuth token", session_id=session_id, error=str(e))


def create_update_token_callback(redis_client) -> Callable:
    """
    Create an Authlib-compatible update_token callback for automatic token refresh.
    
    This callback is invoked by Authlib when tokens are refreshed automatically.
    It saves the new token to Redis under the current session context.
    
    Args:
        redis_client: Async Redis client for token storage
        
    Returns:
        Async callback function compatible with Authlib's update_token parameter
        
    Usage:
        oauth = OAuth(update_token=create_update_token_callback(redis_client))
    """
    async def update_token_callback(token: Dict[str, Any], refresh_token: Optional[str] = None, access_token: Optional[str] = None):
        """
        Authlib callback invoked when token is refreshed.
        
        Args:
            token: New token dictionary from OAuth provider
            refresh_token: Previous refresh_token (for identifying which token to update)
            access_token: Previous access_token (alternative identifier)
        """
        global _current_session_id
        
        session_id = _current_session_id
        if not session_id:
            logger.warning(
                "Token update callback invoked but no session_id in context",
                has_refresh_token=bool(refresh_token),
                has_access_token=bool(access_token)
            )
            return
        
        try:
            # Ensure created_at is set for expiry tracking
            if 'created_at' not in token:
                import time
                token['created_at'] = int(time.time())
            
            # Save updated token to Redis
            await save_token(session_id, redis_client, token)
            
            logger.info(
                "Token automatically updated by Authlib",
                session_id=session_id,
                has_refresh_token=bool(token.get('refresh_token'))
            )
            
        except Exception as e:
            logger.error(
                "Failed to save automatically refreshed token",
                session_id=session_id,
                error=str(e),
                error_type=type(e).__name__
            )
    
    return update_token_callback


def set_session_context(session_id: str) -> None:
    """
    Set the current session_id context for token updates.
    
    Must be called before making API calls that may trigger token refresh.
    
    Args:
        session_id: Current user session identifier
    """
    global _current_session_id
    _current_session_id = session_id


def clear_session_context() -> None:
    """Clear the session_id context after API calls complete."""
    global _current_session_id
    _current_session_id = None

