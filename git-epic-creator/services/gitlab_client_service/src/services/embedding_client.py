"""OpenAI/Azure OpenAI embedding client for generating title embeddings."""

from typing import List
import asyncio
import structlog
from openai import AzureOpenAI, AsyncAzureOpenAI

from config import GitLabClientSettings

logger = structlog.get_logger(__name__)


class EmbeddingClient:
    """Client for generating embeddings via OpenAI or Azure OpenAI API."""
    
    def __init__(self, settings: GitLabClientSettings):
        """
        Initialize embedding client.
        
        Automatically detects Azure OpenAI vs standard OpenAI based on api_version.
        
        Args:
            settings: GitLab client settings with embedding config
        """
        self.settings = settings
        llm_cfg = settings.llm
        
        # Determine if using Azure OpenAI (has api_version) or standard OpenAI
        is_azure = llm_cfg.OAI_API_VERSION is not None
        
        if is_azure:
            logger.info(
                "Initializing Azure OpenAI embedding client",
                base_url=llm_cfg.OAI_BASE_URL,
                api_version=llm_cfg.OAI_API_VERSION,
                model_name=llm_cfg.OAI_EMBED_MODEL_NAME,
                deployment_name=llm_cfg.embedding_deployment_name
            )
            self.client = AzureOpenAI(
                api_key=llm_cfg.OAI_KEY or "dummy-key",
                azure_endpoint=llm_cfg.OAI_BASE_URL or "",
                api_version=llm_cfg.OAI_API_VERSION
            )
            self.async_client = AsyncAzureOpenAI(
                api_key=llm_cfg.OAI_KEY or "dummy-key",
                azure_endpoint=llm_cfg.OAI_BASE_URL or "",
                api_version=llm_cfg.OAI_API_VERSION
            )
        else:
            logger.info(
                "Initializing OpenAI embedding client",
                base_url=llm_cfg.OAI_BASE_URL,
                model=llm_cfg.OAI_EMBED_MODEL_NAME
            )
            self.client = OpenAI(
                api_key=llm_cfg.OAI_KEY or "dummy-key",
                base_url=llm_cfg.OAI_BASE_URL
            )
            self.async_client = AsyncOpenAI(
                api_key=llm_cfg.OAI_KEY or "dummy-key",
                base_url=llm_cfg.OAI_BASE_URL
            )
    
    def embed_texts(self, texts: List[str]) -> List[List[float]]:
        """
        Generate embeddings for a list of texts (synchronous).
        
        Args:
            texts: List of text strings to embed
            
        Returns:
            List of embedding vectors
        """
        if not texts:
            return []
        
        try:
            # Use deployment name for Azure API calls
            response = self.client.embeddings.create(
                model=self.settings.llm.embedding_deployment_name,
                input=texts
            )
            
            embeddings = [item.embedding for item in response.data]
            
            logger.debug(
                "Embeddings generated",
                count=len(embeddings),
                model=self.settings.llm.OAI_EMBED_MODEL_NAME,
                deployment=self.settings.llm.embedding_deployment_name
            )
            
            return embeddings
            
        except Exception as e:
            logger.error(
                "Embedding generation failed",
                error=str(e),
                error_type=type(e).__name__,
                text_count=len(texts)
            )
            raise
    
    async def embed_texts_async(self, texts: List[str]) -> List[List[float]]:
        """
        Generate embeddings for a list of texts (asynchronous).
        
        Args:
            texts: List of text strings to embed
            
        Returns:
            List of embedding vectors
        """
        if not texts:
            return []
        
        try:
            # Use deployment name for Azure API calls
            response = await self.async_client.embeddings.create(
                model=self.settings.llm.embedding_deployment_name,
                input=texts
            )
            
            embeddings = [item.embedding for item in response.data]
            
            logger.debug(
                "Embeddings generated (async)",
                count=len(embeddings),
                model=self.settings.llm.OAI_EMBED_MODEL_NAME,
                deployment=self.settings.llm.embedding_deployment_name
            )
            
            return embeddings
            
        except Exception as e:
            logger.error(
                "Async embedding generation failed",
                error=str(e),
                error_type=type(e).__name__,
                text_count=len(texts)
            )
            raise
    
    async def embed_texts_batched(
        self,
        texts: List[str]
    ) -> List[List[float]]:
        """
        Generate embeddings in batches with concurrency control.
        
        Args:
            texts: List of text strings to embed
            
        Returns:
            List of embedding vectors in original order
        """
        if not texts:
            return []
        
        batch_size = self.settings.OAI_EMBED_BATCH
        concurrency = self.settings.OAI_EMBED_CONCURRENCY
        
        # Split into batches
        batches = [
            texts[i:i + batch_size]
            for i in range(0, len(texts), batch_size)
        ]
        
        logger.info(
            "Starting batched embedding generation",
            total_texts=len(texts),
            batch_count=len(batches),
            batch_size=batch_size,
            concurrency=concurrency
        )
        
        # Process batches with concurrency control
        results = []
        semaphore = asyncio.Semaphore(concurrency)
        
        async def process_batch(batch: List[str]) -> List[List[float]]:
            async with semaphore:
                return await self.embed_texts_async(batch)
        
        batch_results = await asyncio.gather(
            *[process_batch(batch) for batch in batches],
            return_exceptions=False
        )
        
        # Flatten results
        for batch_result in batch_results:
            results.extend(batch_result)
        
        logger.info(
            "Batched embedding generation complete",
            total_embeddings=len(results)
        )
        
        return results


