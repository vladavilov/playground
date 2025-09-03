from functools import lru_cache
from langchain_openai import ChatOpenAI

from config import get_ai_workflow_settings


@lru_cache(maxsize=16)
def get_llm(temperature: float = None) -> ChatOpenAI:
    """Return a shared ChatOpenAI instance configured with settings from config.

    Using a small LRU cache to allow a few variants (e.g., different temperatures)
    while avoiding repeated client construction.

    Args:
        temperature: Optional temperature override. If None, uses default from config.

    Returns:
        Configured ChatOpenAI instance.
    """
    settings = get_ai_workflow_settings()
    
    # Build constructor arguments
    kwargs = {
        "model": settings.llm.OAI_MODEL,
        "timeout": settings.LLM_TIMEOUT_SEC,
        # Azure OpenAI requires api-version query parameter. Prefer configured version when present.
        "default_query": {"api-version": settings.llm.OAI_API_VERSION or "2024-02-15-preview"},
    }
    
    # Add base_url if provided
    if settings.llm.OAI_BASE_URL:
        kwargs["base_url"] = settings.llm.OAI_BASE_URL
    
    # Add API key if provided
    if settings.llm.OAI_KEY:
        kwargs["api_key"] = settings.llm.OAI_KEY
    
    # Use provided temperature or default from config
    kwargs["temperature"] = temperature if temperature is not None else settings.LLM_TEMPERATURE
    
    return ChatOpenAI(**kwargs)


