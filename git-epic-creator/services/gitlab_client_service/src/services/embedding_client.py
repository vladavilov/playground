"""Embedding client for GitLab service using shared utilities.

This module provides a thin wrapper around shared EmbeddingService
configured for GitLab-specific batch sizes and concurrency.
"""

import structlog
from utils.llm_client_factory import create_embedder
from utils.embedding_service import EmbeddingService

from config import GitLabClientSettings

logger = structlog.get_logger(__name__)


class EmbeddingClient:
    """Client for generating embeddings via shared embedding service.
    
    Wraps shared EmbeddingService with GitLab-specific configuration
    for batch size and concurrency control.
    """
    
    def __init__(self, settings: GitLabClientSettings):
        """Initialize embedding client using shared factory.
        
        Args:
            settings: GitLab client settings with embedding config
        """
        self.settings = settings
        
        # Use shared embedder factory (gets config from get_llm_config())
        embedder = create_embedder()
        
        # Wrap in shared EmbeddingService
        self.service = EmbeddingService(embedder)
        
        logger.info(
            "embedding_client_initialized",
            batch_size=settings.OAI_EMBED_BATCH,
            concurrency=settings.OAI_EMBED_CONCURRENCY,
        )
    
    async def embed_texts_batched(self, texts: list[str]) -> list[list[float]]:
        """Generate embeddings in batches with concurrency control.
        
        Delegates to shared EmbeddingService with GitLab-specific
        batch size and concurrency settings.
        
        Args:
            texts: List of text strings to embed
            
        Returns:
            List of embedding vectors in original order
        """
        if not texts:
            return []
        
        return await self.service.embed_batch_concurrent(
            texts=texts,
            batch_size=self.settings.OAI_EMBED_BATCH,
            concurrency=self.settings.OAI_EMBED_CONCURRENCY,
        )
