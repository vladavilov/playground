"""
GitLab Token Manager - Redis-based Token Storage for GitLab OAuth

This module manages GitLab OAuth tokens in Redis, providing storage,
retrieval, and clearing capabilities for session-based authentication.

Integrates with Authlib's automatic token refresh via update_token callback.
Uses token-to-session reverse mapping for automatic session identification.

Redis key patterns:
- gitlab:oauth:{session_id} - Token storage
- gitlab:token_to_session:{access_token} - Reverse lookup for token refresh
"""

from __future__ import annotations

import json
import structlog
from typing import Optional, Dict, Any, Callable

logger = structlog.get_logger(__name__)

TOKEN_KEY_PREFIX = "gitlab:oauth:"
TOKEN_TO_SESSION_PREFIX = "gitlab:token_to_session:"
DEFAULT_TTL = 7200  # 2 hours


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
    Save OAuth token to Redis with reverse mapping.
    
    Stores both:
    1. Token data under session_id
    2. Reverse mapping from token hash to session_id (for automatic refresh)
    
    Args:
        session_id: User session identifier
        redis_client: Async Redis client
        token: Token dictionary from Authlib OAuth flow
        
    Raises:
        Exception: If Redis operation fails
    """
    try:
        ttl = token.get('expires_in', DEFAULT_TTL)
        ttl_with_buffer = int(ttl) + 600  # 10 minute buffer
        
        # Store token under session_id
        token_key = f"{TOKEN_KEY_PREFIX}{session_id}"
        await redis_client.set(
            token_key,
            json.dumps(token),
            ex=ttl_with_buffer
        )
        
        # Store reverse mapping: token -> session_id
        # This enables automatic session lookup during token refresh
        access_token = token.get('access_token')
        if access_token:
            reverse_key = f"{TOKEN_TO_SESSION_PREFIX}{access_token}"
            await redis_client.set(
                reverse_key,
                session_id,
                ex=ttl_with_buffer  # Same TTL as token
            )
        
        logger.info(
            "Saved GitLab OAuth token with reverse mapping",
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
    Clear OAuth token and reverse mapping from Redis.
    
    Called during logout or explicit GitLab account disconnection.
    
    Args:
        session_id: User session identifier
        redis_client: Async Redis client
    """
    try:
        token_key = f"{TOKEN_KEY_PREFIX}{session_id}"
        
        # Get token to find reverse mapping
        token_data = await get_token(session_id, redis_client)
        
        # Delete token
        await redis_client.delete(token_key)
        
        # Delete reverse mapping if token exists
        if token_data and token_data.get('access_token'):
            reverse_key = f"{TOKEN_TO_SESSION_PREFIX}{token_data['access_token']}"
            await redis_client.delete(reverse_key)
        
        logger.info("Cleared GitLab OAuth token and reverse mapping", session_id=session_id)
        
    except Exception as e:
        logger.error("Failed to clear GitLab OAuth token", session_id=session_id, error=str(e))


def create_update_token_callback(redis_client) -> Callable:
    """
    Create an Authlib-compatible update_token callback for automatic token refresh.
    
    This callback is invoked by Authlib when tokens are refreshed automatically.
    Uses the old access_token to look up which session owns it via reverse mapping.
    
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
        
        Uses old access_token to identify session via reverse mapping in Redis.
        No manual context management needed!
        
        Args:
            token: New token dictionary from OAuth provider
            refresh_token: Previous refresh_token (unused)
            access_token: Previous access_token (used for session lookup)
        """
        if not access_token:
            logger.warning(
                "Token update callback invoked without access_token",
                has_refresh_token=bool(refresh_token)
            )
            return
        
        try:
            # Look up session_id using old access_token
            reverse_key = f"{TOKEN_TO_SESSION_PREFIX}{access_token}"
            session_id = await redis_client.get(reverse_key)
            
            if not session_id:
                logger.warning(
                    "Cannot find session for refreshed token",
                    token_prefix=access_token[:16] + "..."
                )
                return
            
            # Decode if bytes
            if isinstance(session_id, bytes):
                session_id = session_id.decode('utf-8')
            
            # Ensure created_at is set for expiry tracking
            if 'created_at' not in token:
                import time
                token['created_at'] = int(time.time())
            
            # Delete old token's reverse mapping
            await redis_client.delete(reverse_key)
            
            # Save updated token with new reverse mapping
            await save_token(session_id, redis_client, token)
            
            logger.info(
                "Token automatically refreshed by Authlib",
                session_id=session_id,
                has_refresh_token=bool(token.get('refresh_token'))
            )
            
        except Exception as e:
            logger.error(
                "Failed to save automatically refreshed token",
                error=str(e),
                error_type=type(e).__name__
            )
    
    return update_token_callback

