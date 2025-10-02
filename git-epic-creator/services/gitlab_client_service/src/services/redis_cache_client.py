"""Redis cache client for storing and retrieving embeddings."""

from typing import List, Optional, Dict
import json
import structlog
import redis.asyncio as redis

logger = structlog.get_logger(__name__)


class RedisCacheClient:
    """Client for caching embeddings in Redis."""
    
    def __init__(self, redis_client: redis.Redis):
        """
        Initialize Redis cache client.
        
        Args:
            redis_client: Configured Redis client
        """
        self.redis = redis_client
    
    def _make_key(self, project_id: str, work_item_id: str) -> str:
        """
        Create Redis key for a work item embedding.
        
        Args:
            project_id: Project ID
            work_item_id: Work item ID (epic or issue)
            
        Returns:
            Redis key string
        """
        return f"embeddings:{project_id}:{work_item_id}"
    
    async def set_embedding(
        self,
        project_id: str,
        work_item_id: str,
        embedding: List[float]
    ) -> None:
        """
        Store an embedding vector in Redis.
        
        Args:
            project_id: Project ID
            work_item_id: Work item ID
            embedding: Embedding vector
        """
        key = self._make_key(project_id, work_item_id)
        value = json.dumps(embedding)
        
        await self.redis.set(key, value)
        
        logger.debug(
            "Embedding cached",
            project_id=project_id,
            work_item_id=work_item_id,
            embedding_dim=len(embedding)
        )
    
    async def get_embedding(
        self,
        project_id: str,
        work_item_id: str
    ) -> Optional[List[float]]:
        """
        Retrieve an embedding vector from Redis.
        
        Args:
            project_id: Project ID
            work_item_id: Work item ID
            
        Returns:
            Embedding vector or None if not found
        """
        key = self._make_key(project_id, work_item_id)
        value = await self.redis.get(key)
        
        if value is None:
            return None
        
        embedding = json.loads(value)
        
        logger.debug(
            "Embedding retrieved from cache",
            project_id=project_id,
            work_item_id=work_item_id,
            embedding_dim=len(embedding)
        )
        
        return embedding
    
    async def set_embeddings_bulk(
        self,
        project_id: str,
        embeddings: Dict[str, List[float]]
    ) -> None:
        """
        Store multiple embeddings in a single pipeline.
        
        Args:
            project_id: Project ID
            embeddings: Dict mapping work_item_id to embedding vector
        """
        if not embeddings:
            return
        
        pipe = self.redis.pipeline()
        
        for work_item_id, embedding in embeddings.items():
            key = self._make_key(project_id, work_item_id)
            value = json.dumps(embedding)
            pipe.set(key, value)
        
        await pipe.execute()
        
        logger.info(
            "Bulk embeddings cached",
            project_id=project_id,
            count=len(embeddings)
        )
    
    async def get_embeddings_bulk(
        self,
        project_id: str,
        work_item_ids: List[str]
    ) -> Dict[str, List[float]]:
        """
        Retrieve multiple embeddings in a single pipeline.
        
        Args:
            project_id: Project ID
            work_item_ids: List of work item IDs
            
        Returns:
            Dict mapping work_item_id to embedding vector (only found items)
        """
        if not work_item_ids:
            return {}
        
        pipe = self.redis.pipeline()
        keys = [self._make_key(project_id, wid) for wid in work_item_ids]
        
        for key in keys:
            pipe.get(key)
        
        values = await pipe.execute()
        
        result = {}
        for work_item_id, value in zip(work_item_ids, values):
            if value is not None:
                result[work_item_id] = json.loads(value)
        
        logger.debug(
            "Bulk embeddings retrieved",
            project_id=project_id,
            requested=len(work_item_ids),
            found=len(result)
        )
        
        return result
    
    async def clear_project_embeddings(self, project_id: str) -> int:
        """
        Clear all embeddings for a project.
        
        Args:
            project_id: Project ID
            
        Returns:
            Number of keys deleted
        """
        pattern = f"embeddings:{project_id}:*"
        
        # Scan and delete keys matching pattern
        cursor = 0
        deleted = 0
        
        while True:
            cursor, keys = await self.redis.scan(
                cursor=cursor,
                match=pattern,
                count=100
            )
            
            if keys:
                deleted += await self.redis.delete(*keys)
            
            if cursor == 0:
                break
        
        logger.info(
            "Project embeddings cleared",
            project_id=project_id,
            deleted_count=deleted
        )
        
        return deleted


