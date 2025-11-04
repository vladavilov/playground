"""Shared embedding service with request-level caching and validation.

This module provides an EmbeddingService class that encapsulates:
- Embedding generation with Azure OpenAI
- Request-level caching to avoid duplicate embeddings
- Dimension validation to catch configuration errors early
- Batched embedding with concurrency control
- Comprehensive error handling and logging
"""

from typing import Optional
import asyncio
import structlog
from langchain_openai import AzureOpenAIEmbeddings
from fastapi import HTTPException

logger = structlog.get_logger(__name__)


class EmbeddingService:
    """Service for generating embeddings with caching and validation.
    
    Provides:
    - Request-level caching (avoids duplicate embeddings in same request)
    - Dimension validation (catches config errors early)
    - Clean error handling with HTTPException
    - Logging for observability
    
    Examples:
        # Initialize service
        embedder = create_embedder()  # From llm_client_factory
        service = EmbeddingService(embedder, expected_dimensions=1536)
        
        # Generate embedding with caching
        vector = await service.embed_with_cache("query text", cache={})
        
        # Validate dimensions
        service.validate_dimension(vector, "query_embedding")
    """
    
    def __init__(
        self,
        embedder: AzureOpenAIEmbeddings,
        expected_dimensions: Optional[int] = None,
    ):
        """Initialize embedding service.
        
        Args:
            embedder: Configured AzureOpenAIEmbeddings instance
            expected_dimensions: Expected embedding vector dimensions (optional)
        """
        self.embedder = embedder
        self.expected_dimensions = expected_dimensions
    
    async def embed_text(self, text: str) -> list[float]:
        """Generate embedding for single text.
        
        Args:
            text: Text to embed
            
        Returns:
            Embedding vector as list of floats
            
        Raises:
            HTTPException(502): If embedding generation fails
        """
        try:
            vectors = await self.embedder.aembed_documents([text])
            if not vectors:
                raise ValueError("Embedder returned empty result")
            
            vector = [float(x) for x in vectors[0]]
            
            logger.debug(
                "embedding_generated",
                text_preview=text[:50],
                vector_length=len(vector),
            )
            
            return vector
            
        except Exception as exc:
            logger.error(
                "embedding_generation_failed",
                error=str(exc),
                error_type=type(exc).__name__,
                text_preview=text[:100],
            )
            raise HTTPException(
                status_code=502,
                detail=f"Embedding generation failed: {exc}"
            )
    
    async def embed_with_cache(
        self,
        text: str,
        cache: dict[str, list[float]],
    ) -> list[float]:
        """Embed text with request-level caching.
        
        Uses cache dictionary to avoid duplicate embeddings within same request.
        Cache is typically stored in request state/context.
        
        Args:
            text: Text to embed
            cache: Dictionary mapping text -> embedding vector
            
        Returns:
            Embedding vector (from cache or newly generated)
        """
        # Check cache first
        if text in cache:
            logger.debug("embedding_cache_hit", text_preview=text[:50])
            return cache[text]
        
        # Cache miss - generate embedding
        vector = await self.embed_text(text)
        cache[text] = vector
        
        logger.debug(
            "embedding_cache_miss",
            text_preview=text[:50],
            vec_len=len(vector),
        )
        
        return vector
    
    def validate_dimension(
        self,
        vector: list[float],
        label: str,
    ) -> None:
        """Validate embedding vector dimension matches expected.
        
        Args:
            vector: Embedding vector to validate
            label: Label for logging context
            
        Raises:
            HTTPException(502): If dimension mismatch detected
        """
        if self.expected_dimensions is None:
            # No validation configured
            return
        
        actual_dim = len(vector)
        
        if actual_dim != self.expected_dimensions:
            logger.error(
                "embedding_dimension_mismatch",
                label=label,
                expected_dim=self.expected_dimensions,
                actual_dim=actual_dim,
                mismatch_size=abs(self.expected_dimensions - actual_dim),
                message=f"Embedding dimension mismatch may cause low recall or query failures"
            )
            raise HTTPException(
                status_code=502,
                detail=(
                    f"Embedding dimension mismatch: expected {self.expected_dimensions}, "
                    f"got {actual_dim}. Check OAI_EMBED_MODEL_NAME configuration."
                )
            )
        
        logger.debug(
            "embedding_dimension_validated",
            label=label,
            dimension=actual_dim
        )
    
    async def embed_and_validate(
        self,
        text: str,
        label: str,
    ) -> list[float]:
        """Generate embedding and validate dimensions in one call.
        
        Args:
            text: Text to embed
            label: Label for logging and validation
            
        Returns:
            Validated embedding vector
            
        Raises:
            HTTPException(502): If generation fails or dimension mismatch
        """
        vector = await self.embed_text(text)
        self.validate_dimension(vector, label)
        return vector
    
    async def embed_batch(self, texts: list[str]) -> list[list[float]]:
        """Generate embeddings for multiple texts efficiently.
        
        Args:
            texts: List of texts to embed
            
        Returns:
            List of embedding vectors
            
        Raises:
            HTTPException(502): If embedding generation fails
        """
        if not texts:
            return []
        
        try:
            vectors = await self.embedder.aembed_documents(texts)
            result = [[float(x) for x in vec] for vec in vectors]
            
            logger.debug(
                "embeddings_batch_generated",
                count=len(texts),
                vector_length=len(result[0]) if result else 0,
            )
            
            return result
            
        except Exception as exc:
            logger.error(
                "embedding_batch_failed",
                error=str(exc),
                error_type=type(exc).__name__,
                text_count=len(texts),
            )
            raise HTTPException(
                status_code=502,
                detail=f"Batch embedding generation failed: {exc}"
            )
    
    async def embed_query(self, query: str) -> list[float]:
        """Generate embedding for query text (alias for single text embedding).
        
        Some embedding models have separate query vs document embeddings.
        This method uses aembed_query for potential optimization.
        
        Args:
            query: Query text to embed
            
        Returns:
            Query embedding vector
        """
        try:
            vector = await self.embedder.aembed_query(query)
            result = [float(x) for x in vector]
            
            logger.debug(
                "query_embedding_generated",
                query_preview=query[:50],
                vector_length=len(result),
            )
            
            return result
            
        except Exception as exc:
            logger.error(
                "query_embedding_failed",
                error=str(exc),
                error_type=type(exc).__name__,
                query_preview=query[:100],
            )
            raise HTTPException(
                status_code=502,
                detail=f"Query embedding generation failed: {exc}"
            )
    
    async def embed_batch_concurrent(
        self,
        texts: list[str],
        batch_size: int = 16,
        concurrency: int = 2,
    ) -> list[list[float]]:
        """Generate embeddings in batches with concurrency control.
        
        Optimized for large-scale embedding generation with:
        - Batch processing to reduce API overhead
        - Concurrency control to respect rate limits
        - Automatic batching and result flattening
        
        Args:
            texts: List of texts to embed
            batch_size: Number of texts per batch (default: 16)
            concurrency: Maximum concurrent batches (default: 2)
            
        Returns:
            List of embedding vectors in original order
            
        Raises:
            HTTPException(502): If embedding generation fails
            
        Examples:
            # Embed 1000 titles with concurrency
            embeddings = await service.embed_batch_concurrent(
                titles, batch_size=16, concurrency=3
            )
        """
        if not texts:
            return []
        
        # Split into batches
        batches = [
            texts[i:i + batch_size]
            for i in range(0, len(texts), batch_size)
        ]
        
        logger.info(
            "starting_concurrent_batch_embedding",
            total_texts=len(texts),
            batch_count=len(batches),
            batch_size=batch_size,
            concurrency=concurrency,
        )
        
        # Process batches with concurrency control
        semaphore = asyncio.Semaphore(concurrency)
        
        async def process_batch(batch: list[str]) -> list[list[float]]:
            async with semaphore:
                return await self.embed_batch(batch)
        
        try:
            batch_results = await asyncio.gather(
                *[process_batch(batch) for batch in batches],
                return_exceptions=False
            )
            
            # Flatten results
            results = []
            for batch_result in batch_results:
                results.extend(batch_result)
            
            logger.info(
                "concurrent_batch_embedding_complete",
                total_embeddings=len(results),
                batches_processed=len(batches),
            )
            
            return results
            
        except Exception as exc:
            logger.error(
                "concurrent_batch_embedding_failed",
                error=str(exc),
                error_type=type(exc).__name__,
                total_texts=len(texts),
                batches_attempted=len(batches),
            )
            raise HTTPException(
                status_code=502,
                detail=f"Concurrent batch embedding failed: {exc}"
            )

