"""In-memory idempotency store with TTL for deduplication."""

import time
from typing import Dict, Any, Optional, Tuple
import structlog

from config import GitLabClientSettings

logger = structlog.get_logger(__name__)


class IdempotencyStore:
    """
    In-memory store for idempotency keys with TTL.
    
    Stores (project_id, idempotency_key) -> (result, expiry_time).
    """
    
    def __init__(self, settings: GitLabClientSettings):
        """
        Initialize idempotency store.
        
        Args:
            settings: GitLab client settings with TTL config
        """
        self.settings = settings
        self._store: Dict[Tuple[str, str], Tuple[Any, float]] = {}
    
    def _cleanup_expired(self) -> None:
        """Remove expired entries from the store."""
        now = time.time()
        expired_keys = [
            key for key, (_, expiry) in self._store.items()
            if expiry < now
        ]
        
        for key in expired_keys:
            del self._store[key]
        
        if expired_keys:
            logger.debug(
                "Cleaned up expired idempotency keys",
                count=len(expired_keys)
            )
    
    def get(
        self,
        project_id: str,
        idempotency_key: str
    ) -> Optional[Any]:
        """
        Retrieve stored result for an idempotency key.
        
        Args:
            project_id: Project ID
            idempotency_key: Idempotency key
            
        Returns:
            Stored result or None if not found or expired
        """
        self._cleanup_expired()
        
        key = (project_id, idempotency_key)
        entry = self._store.get(key)
        
        if entry is None:
            return None
        
        result, expiry = entry
        now = time.time()
        
        if expiry < now:
            # Expired
            del self._store[key]
            return None
        
        logger.info(
            "Idempotency key hit",
            project_id=project_id,
            idempotency_key=idempotency_key
        )
        
        return result
    
    def set(
        self,
        project_id: str,
        idempotency_key: str,
        result: Any
    ) -> None:
        """
        Store a result with an idempotency key.
        
        Args:
            project_id: Project ID
            idempotency_key: Idempotency key
            result: Result to store
        """
        self._cleanup_expired()
        
        key = (project_id, idempotency_key)
        expiry = time.time() + self.settings.IDEMPOTENCY_TTL_SECONDS
        
        self._store[key] = (result, expiry)
        
        logger.debug(
            "Idempotency key stored",
            project_id=project_id,
            idempotency_key=idempotency_key,
            ttl_seconds=self.settings.IDEMPOTENCY_TTL_SECONDS
        )
    
    def clear(self) -> None:
        """Clear all stored idempotency keys (for testing)."""
        self._store.clear()
        logger.debug("Idempotency store cleared")


