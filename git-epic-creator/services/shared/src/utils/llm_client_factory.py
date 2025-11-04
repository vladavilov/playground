"""Shared LLM and embedding client factory for Azure OpenAI.

This module provides standardized client creation for:
- AzureChatOpenAI (chat completions)
- AzureOpenAIEmbeddings (text embeddings)

Uses @lru_cache to avoid repeated client construction. All configuration
is loaded from get_llm_config() for consistency across services.
"""

from functools import lru_cache
import structlog
from langchain_openai import AzureChatOpenAI, AzureOpenAIEmbeddings

from configuration.llm_config import get_llm_config

logger = structlog.get_logger(__name__)


@lru_cache(maxsize=2)
def create_llm(use_fast_model: bool = False) -> AzureChatOpenAI:
    """Create cached AzureChatOpenAI instance configured with settings.

    Uses LRU cache for two model variants (standard and fast) to avoid
    repeated client construction. All configuration loaded from get_llm_config().

    Args:
        use_fast_model: If True, uses OAI_MODEL_FAST for simpler tasks.
                        If False, uses OAI_MODEL for complex reasoning.

    Returns:
        Configured AzureChatOpenAI instance

    Examples:
        # Standard model for complex tasks
        llm = create_llm(use_fast_model=False)
        
        # Fast model for simple extraction
        llm_fast = create_llm(use_fast_model=True)
    """
    cfg = get_llm_config()
    model = cfg.OAI_MODEL_FAST if use_fast_model else cfg.OAI_MODEL
    
    if not cfg.OAI_KEY:
        raise ValueError(
            "OAI_KEY not configured. Set OAI_KEY environment variable for Azure OpenAI access."
        )
    
    if not cfg.OAI_BASE_URL:
        raise ValueError(
            "OAI_BASE_URL not configured. Set OAI_BASE_URL environment variable "
            "for Azure OpenAI endpoint (e.g., https://<resource>.openai.azure.com)."
        )
    
    client = AzureChatOpenAI(
        azure_endpoint=cfg.OAI_BASE_URL,
        deployment_name=model,
        api_key=cfg.OAI_KEY,
        api_version=cfg.OAI_API_VERSION or "2024-02-15-preview",
        temperature=cfg.LLM_TEMPERATURE,
    )
    
    logger.debug(
        "llm_client_created",
        model=model,
        use_fast_model=use_fast_model,
        endpoint=cfg.OAI_BASE_URL,
        temperature=client.temperature,
        timeout=client.timeout,
    )
    
    return client


@lru_cache(maxsize=1)
def create_embedder() -> AzureOpenAIEmbeddings:
    """Create cached AzureOpenAIEmbeddings instance configured with settings.

    Uses LRU cache to avoid repeated client construction.
    All configuration loaded from get_llm_config().

    Returns:
        Configured AzureOpenAIEmbeddings instance

    Examples:
        embedder = create_embedder()
        vectors = await embedder.aembed_documents(["text1", "text2"])
    """
    cfg = get_llm_config()
    
    if not cfg.OAI_KEY:
        raise ValueError(
            "OAI_KEY not configured. Set OAI_KEY environment variable for Azure OpenAI access."
        )
    
    if not cfg.OAI_BASE_URL:
        raise ValueError(
            "OAI_BASE_URL not configured. Set OAI_BASE_URL environment variable."
        )
    
    # Use deployment name (falls back to model name if not set)
    deployment = cfg.embedding_deployment_name
    
    embedder = AzureOpenAIEmbeddings(
        azure_endpoint=cfg.OAI_BASE_URL,
        azure_deployment=deployment,
        api_key=cfg.OAI_KEY,
        api_version=cfg.OAI_API_VERSION or "2024-02-15-preview",
    )
    
    logger.debug(
        "embedder_created",
        model=cfg.OAI_EMBED_MODEL_NAME,
        deployment=deployment,
        endpoint=cfg.OAI_BASE_URL,
    )
    
    return embedder


def clear_llm_cache():
    """Clear LRU cache for LLM clients.
    
    Useful for testing or when configuration changes at runtime.
    """
    create_llm.cache_clear()
    create_embedder.cache_clear()
    logger.info("llm_cache_cleared", message="Cleared LLM and embedder caches")

