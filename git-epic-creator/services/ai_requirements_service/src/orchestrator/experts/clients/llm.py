from functools import lru_cache
from langchain_openai import AzureChatOpenAI

from config import get_ai_requirements_settings


@lru_cache(maxsize=2)
def get_llm(use_fast_model: bool = False) -> AzureChatOpenAI:
    """Return a shared AzureChatOpenAI instance configured with settings from config.

    Using a small LRU cache for the two model variants (standard and fast)
    to avoid repeated client construction.

    Args:
        use_fast_model: If True, uses OAI_MODEL_FAST instead of OAI_MODEL for faster, simpler tasks.

    Returns:
        Configured AzureChatOpenAI instance.
    """
    settings = get_ai_requirements_settings()
    model = settings.llm.OAI_MODEL_FAST if use_fast_model else settings.llm.OAI_MODEL
    
    return AzureChatOpenAI(
        azure_endpoint=settings.llm.OAI_BASE_URL,
        deployment_name=model,
        api_key=settings.llm.OAI_KEY,
        api_version=settings.llm.OAI_API_VERSION or "2024-02-15-preview",
        timeout=settings.llm.LLM_TIMEOUT_SEC,
        temperature=settings.llm.LLM_TEMPERATURE,
    )


