"""
GitLab Token Manager - Redis-based Token Storage for GitLab OAuth

This module manages GitLab OAuth tokens in Redis, providing storage,
retrieval, and clearing capabilities for user-based authentication.

Tokens are stored per user (Azure AD oid), not per session, enabling:
- Persistent GitLab connection across browser sessions/tabs
- Simpler architecture without reverse mapping
- Better UX - connect once, works everywhere

Redis key patterns:
- gitlab:oauth:{user_id} - Token storage keyed by Azure AD object ID (oid)
- gitlab:token_to_user:{access_token} - Reverse lookup for automatic token refresh

The reverse mapping enables Authlib to automatically refresh expired tokens
by looking up which user owns the old token.
"""

from __future__ import annotations

import json
import time
import structlog
from typing import Optional, Dict, Any

logger = structlog.get_logger(__name__)

TOKEN_KEY_PREFIX = "gitlab:oauth:"
TOKEN_TO_USER_PREFIX = "gitlab:token_to_user:"
DEFAULT_TTL = 7200  # 2 hours
TOKEN_EXPIRY_SAFETY_MARGIN = 300  # 5 minutes - refresh tokens proactively


async def get_token(
    user_id: str,
    redis_client
) -> Optional[Dict[str, Any]]:
    """
    Load GitLab OAuth token from Redis.
    
    Args:
        user_id: Azure AD user object ID (oid)
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
        key = f"{TOKEN_KEY_PREFIX}{user_id}"
        data = await redis_client.get(key)
        
        if data:
            token = json.loads(data)
            logger.debug("Loaded GitLab OAuth token from Redis", user_id=user_id)
            return token
        else:
            logger.debug("No GitLab OAuth token found in Redis", user_id=user_id)
            return None
            
    except json.JSONDecodeError:
        logger.warning("Corrupted token data in Redis", user_id=user_id)
        return None
    except Exception as e:
        logger.error("Failed to load GitLab OAuth token", user_id=user_id, error=str(e))
        return None


async def save_token(
    user_id: str,
    redis_client,
    token: Dict[str, Any]
) -> None:
    """
    Save OAuth token to Redis with reverse mapping.
    
    Stores both:
    1. Token data keyed by user's Azure AD object ID (oid)
    2. Reverse mapping from access_token to user_id (for automatic refresh)
    
    This enables persistent GitLab connection across sessions/devices
    while maintaining automatic token refresh capability.
    
    Args:
        user_id: Azure AD user object ID (oid)
        redis_client: Async Redis client
        token: Token dictionary from Authlib OAuth flow
        
    Raises:
        Exception: If Redis operation fails
    """
    try:
        ttl = token.get('expires_in', DEFAULT_TTL)
        ttl_with_buffer = int(ttl) + 600  # 10 minute buffer
        
        # Store token under user_id
        token_key = f"{TOKEN_KEY_PREFIX}{user_id}"
        await redis_client.set(
            token_key,
            json.dumps(token),
            ex=ttl_with_buffer
        )
        
        # Store reverse mapping: access_token -> user_id
        # This enables automatic token refresh by Authlib
        access_token = token.get('access_token')
        if access_token:
            reverse_key = f"{TOKEN_TO_USER_PREFIX}{access_token}"
            await redis_client.set(
                reverse_key,
                user_id,
                ex=ttl_with_buffer  # Same TTL as token
            )
        
        logger.info(
            "Saved GitLab OAuth token with reverse mapping",
            user_id=user_id,
            expires_in=ttl,
            has_refresh_token=bool(token.get('refresh_token'))
        )
        
    except Exception as e:
        logger.error("Failed to save GitLab OAuth token", user_id=user_id, error=str(e))
        raise


async def clear_token(
    user_id: str,
    redis_client
) -> None:
    """
    Clear OAuth token and reverse mapping from Redis.
    
    Called during logout or explicit GitLab account disconnection.
    
    Args:
        user_id: Azure AD user object ID (oid)
        redis_client: Async Redis client
    """
    try:
        token_key = f"{TOKEN_KEY_PREFIX}{user_id}"
        
        # Get token to find reverse mapping
        token_data = await get_token(user_id, redis_client)
        
        # Delete token
        await redis_client.delete(token_key)
        
        # Delete reverse mapping if token exists
        if token_data and token_data.get('access_token'):
            reverse_key = f"{TOKEN_TO_USER_PREFIX}{token_data['access_token']}"
            await redis_client.delete(reverse_key)
        
        logger.info("Cleared GitLab OAuth token and reverse mapping", user_id=user_id)
        
    except Exception as e:
        logger.error("Failed to clear GitLab OAuth token", user_id=user_id, error=str(e))


def is_token_expired(token: Dict[str, Any], safety_margin: int = TOKEN_EXPIRY_SAFETY_MARGIN) -> bool:
    """
    Check if OAuth token is expired or about to expire.
    
    Uses safety margin to proactively refresh tokens before they expire,
    avoiding 401 errors during API calls.
    
    Args:
        token: Token dictionary with created_at and expires_in fields
        safety_margin: Seconds before expiry to consider token expired (default: 5 minutes)
        
    Returns:
        True if token is expired or will expire within safety margin
    """
    if not token:
        return True
    
    created_at = token.get('created_at')
    expires_in = token.get('expires_in')
    
    # If missing required fields, consider expired
    if not created_at or not expires_in:
        logger.warning(
            "Token missing expiry information",
            has_created_at=bool(created_at),
            has_expires_in=bool(expires_in)
        )
        return True
    
    try:
        # Calculate expiry time with safety margin
        expiry_time = int(created_at) + int(expires_in)
        current_time = int(time.time())
        time_until_expiry = expiry_time - current_time
        
        is_expired = time_until_expiry <= safety_margin
        
        if is_expired:
            logger.info(
                "Token expired or expiring soon",
                time_until_expiry=time_until_expiry,
                safety_margin=safety_margin,
                expires_at=expiry_time
            )
        else:
            logger.debug(
                "Token is valid",
                time_until_expiry=time_until_expiry,
                expires_at=expiry_time
            )
        
        return is_expired
        
    except (ValueError, TypeError) as e:
        logger.warning("Error checking token expiry", error=str(e))
        return True


async def exchange_token_for_user(
    user_id: str,
    redis_client,
    oauth_client,
    **fetch_params
) -> Dict[str, Any]:
    """
    Exchange authorization code or refresh token for access token.
    
    Handles both initial OAuth token exchange and token refresh.
    Automatically adds created_at timestamp and saves to Redis.
    
    Args:
        user_id: Azure AD user object ID (oid)
        redis_client: Async Redis client
        oauth_client: Authlib OAuth client (e.g., oauth.gitlab)
        **fetch_params: Parameters for fetch_access_token (code, redirect_uri, grant_type, refresh_token, etc.)
        
    Returns:
        Token dictionary with access_token, refresh_token, expires_in, created_at
        
    Raises:
        Exception: If token exchange fails or response is invalid
        
    Example usage:
        # Initial OAuth callback
        token = await exchange_token_for_user(
            user_id=user_id,
            redis_client=redis_client,
            oauth_client=oauth.gitlab,
            code=code,
            redirect_uri=callback_uri
        )
        
        # Token refresh
        token = await exchange_token_for_user(
            user_id=user_id,
            redis_client=redis_client,
            oauth_client=oauth.gitlab,
            grant_type='refresh_token',
            refresh_token=refresh_token
        )
    """
    try:
        # Exchange code/refresh_token for access token
        token = await oauth_client.fetch_access_token(**fetch_params)
        
        if not token or not token.get('access_token'):
            logger.error(
                "Token exchange returned invalid response",
                user_id=user_id,
                has_token=bool(token),
                has_access_token=bool(token.get('access_token') if token else False)
            )
            raise ValueError("Token exchange failed - no access_token in response")
        
        # Add created_at timestamp for expiry tracking
        token['created_at'] = int(time.time())
        
        # Save token to Redis
        await save_token(user_id, redis_client, token)
        
        logger.info(
            "Token exchange successful",
            user_id=user_id,
            has_refresh_token=bool(token.get('refresh_token')),
            expires_in=token.get('expires_in')
        )
        
        return token
        
    except Exception as e:
        logger.error(
            "Token exchange failed",
            user_id=user_id,
            error=str(e),
            error_type=type(e).__name__
        )
        raise


def create_update_token_callback(redis_client):
    """
    Create an Authlib-compatible update_token callback for automatic token refresh.
    
    This callback is invoked by Authlib when tokens are refreshed automatically.
    Uses the old access_token to look up which user owns it via reverse mapping.
    
    With oid-based storage, the reverse mapping is simpler:
    gitlab:token_to_user:{old_access_token} -> user_id
    
    Args:
        redis_client: Async Redis client for token storage
        
    Returns:
        Async callback function compatible with Authlib's update_token parameter
        
    Usage:
        oauth = OAuth(update_token=create_update_token_callback(redis_client))
    """
    async def update_token_callback(
        token: Dict[str, Any],
        refresh_token: Optional[str] = None,
        access_token: Optional[str] = None
    ):
        """
        Authlib callback invoked when token is refreshed.
        
        Uses old access_token to identify user via reverse mapping in Redis.
        
        Args:
            token: New token dictionary from OAuth provider
            refresh_token: Previous refresh_token (unused)
            access_token: Previous access_token (used for user lookup)
        """
        if not access_token:
            logger.warning(
                "Token update callback invoked without access_token",
                has_refresh_token=bool(refresh_token)
            )
            return
        
        try:
            # Look up user_id using old access_token
            reverse_key = f"{TOKEN_TO_USER_PREFIX}{access_token}"
            user_id = await redis_client.get(reverse_key)
            
            if not user_id:
                logger.warning(
                    "Cannot find user for refreshed token",
                    token_prefix=access_token[:16] + "..."
                )
                return
            
            # Decode if bytes
            if isinstance(user_id, bytes):
                user_id = user_id.decode('utf-8')
            
            # Ensure created_at is set for expiry tracking
            if 'created_at' not in token:
                token['created_at'] = int(time.time())
            
            # Delete old token's reverse mapping
            await redis_client.delete(reverse_key)
            
            # Save updated token with new reverse mapping
            await save_token(user_id, redis_client, token)
            
            logger.info(
                "Token automatically refreshed by Authlib",
                user_id=user_id,
                has_refresh_token=bool(token.get('refresh_token'))
            )
            
        except Exception as e:
            logger.error(
                "Failed to save automatically refreshed token",
                error=str(e),
                error_type=type(e).__name__
            )
    
    return update_token_callback
