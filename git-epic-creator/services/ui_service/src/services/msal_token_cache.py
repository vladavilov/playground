"""
MSAL Token Cache with Redis Backend

This module implements a custom SerializableTokenCache for MSAL Python
that persists tokens to Redis for scalable, distributed caching.

The cache implements MSAL's standard serialization interface and automatically
handles token expiration and refresh.
"""

from __future__ import annotations

import json
import time
import structlog

from msal import SerializableTokenCache

logger = structlog.get_logger(__name__)

CACHE_KEY_PREFIX = "ui:msal:"
DEFAULT_TTL_SECONDS = 3600  # 1 hour default


class MSALRedisTokenCache(SerializableTokenCache):
    """
    MSAL token cache backed by Redis.
    
    This class extends MSAL's SerializableTokenCache to persist tokens to Redis,
    enabling distributed sessions and horizontal scaling of the UI service.
    
    The cache automatically loads from Redis before MSAL accesses it and
    saves back to Redis after MSAL modifies it.
    """
    
    def __init__(self, session_id: str, redis_client):
        """
        Initialize MSAL token cache with Redis backend.
        
        Args:
            session_id: Unique session identifier (used as cache partition key)
            redis_client: Async Redis client instance
        """
        super().__init__()
        self.session_id = session_id
        self.redis_client = redis_client
        self._cache_key = f"{CACHE_KEY_PREFIX}{session_id}"
        logger.debug("MSAL token cache initialized", session_id=session_id)
    
    async def load_from_redis(self) -> None:
        """
        Load cached tokens from Redis into MSAL cache.
        
        This method is called before MSAL accesses the cache to ensure
        it has the latest token state from Redis.
        """
        try:
            raw_data = await self.redis_client.get(self._cache_key)
            if raw_data:
                try:
                    # Deserialize and load into MSAL cache
                    self.deserialize(raw_data)
                    logger.debug("Loaded MSAL cache from Redis", session_id=self.session_id)
                except json.JSONDecodeError:
                    logger.warning(
                        "Corrupted cache data in Redis, ignoring",
                        session_id=self.session_id
                    )
            else:
                logger.debug("No cached tokens in Redis", session_id=self.session_id)
        except Exception as e:
            logger.error(
                "Failed to load MSAL cache from Redis",
                session_id=self.session_id,
                error=str(e)
            )
    
    async def save_to_redis(self) -> None:
        """
        Save MSAL cache to Redis.
        
        This method is called after MSAL modifies the cache to persist
        changes to Redis.
        
        The TTL is calculated based on token expiration to avoid storing
        expired tokens indefinitely.
        """
        if not self.has_state_changed:
            logger.debug("MSAL cache unchanged, skipping save", session_id=self.session_id)
            return
        
        try:
            # Serialize cache to JSON
            serialized = self.serialize()
            
            # Calculate TTL based on token expiration
            ttl = self._calculate_ttl(serialized)
            
            # Save to Redis with TTL
            await self.redis_client.set(
                self._cache_key,
                serialized,
                ex=ttl
            )
            
            logger.debug(
                "Saved MSAL cache to Redis",
                session_id=self.session_id,
                ttl=ttl
            )
            
            # Reset state change flag
            self.has_state_changed = False
            
        except Exception as e:
            logger.error(
                "Failed to save MSAL cache to Redis",
                session_id=self.session_id,
                error=str(e)
            )
    
    async def clear_from_redis(self) -> None:
        """
        Clear cached tokens from Redis.
        
        This method is called during logout to remove all cached tokens
        for the session.
        """
        try:
            await self.redis_client.delete(self._cache_key)
            logger.debug("Cleared MSAL cache from Redis", session_id=self.session_id)
        except Exception as e:
            logger.error(
                "Failed to clear MSAL cache from Redis",
                session_id=self.session_id,
                error=str(e)
            )
    
    def _calculate_ttl(self, serialized_cache: str) -> int:
        """
        Calculate TTL based on token expiration.
        
        Examines all tokens in the cache and uses the maximum expiration
        time to set Redis TTL.
        
        Args:
            serialized_cache: Serialized cache JSON string
            
        Returns:
            TTL in seconds (minimum 60, maximum configured default)
        """
        try:
            cache_data = json.loads(serialized_cache)
            max_exp = 0
            
            # Check AccessToken expiration
            for token_key, token_data in cache_data.get("AccessToken", {}).items():
                exp_str = token_data.get("expires_on")
                if exp_str:
                    try:
                        exp = int(exp_str)
                        max_exp = max(max_exp, exp)
                    except (ValueError, TypeError):
                        pass
            
            # Check RefreshToken expiration (if present)
            for token_key, token_data in cache_data.get("RefreshToken", {}).items():
                exp_str = token_data.get("expires_on")
                if exp_str:
                    try:
                        exp = int(exp_str)
                        max_exp = max(max_exp, exp)
                    except (ValueError, TypeError):
                        pass
            
            if max_exp > 0:
                ttl = max(60, int(max_exp - time.time()))
                return min(ttl, DEFAULT_TTL_SECONDS)
            
        except Exception as e:
            logger.warning(
                "Failed to calculate TTL from token expiration",
                error=str(e)
            )
        
        # Default TTL
        return DEFAULT_TTL_SECONDS

