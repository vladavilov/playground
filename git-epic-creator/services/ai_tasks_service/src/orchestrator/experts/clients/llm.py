"""LLM factory for AI Tasks Service."""

from functools import lru_cache
from langchain_openai import AzureChatOpenAI

from config import get_ai_tasks_settings


@lru_cache(maxsize=16)
def get_llm(temperature: float = None) -> AzureChatOpenAI:
    """Return a shared AzureChatOpenAI instance configured with settings from config.

    Args:
        temperature: Optional temperature override. If None, uses default from config.

    Returns:
        Configured AzureChatOpenAI instance.
    """
    settings = get_ai_tasks_settings()
    
    return AzureChatOpenAI(
        azure_endpoint=settings.llm.OAI_BASE_URL,
        deployment_name=settings.llm.OAI_MODEL,
        api_key=settings.llm.OAI_KEY,
        api_version=settings.llm.OAI_API_VERSION,
        timeout=settings.LLM_TIMEOUT_SEC,
        temperature=temperature if temperature is not None else settings.LLM_TEMPERATURE,
    )



