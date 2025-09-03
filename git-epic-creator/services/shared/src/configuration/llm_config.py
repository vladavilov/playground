"""
Shared LLM configuration used across services.
"""

from functools import lru_cache
from pydantic import Field

from .base_config import BaseConfig


class LlmConfig(BaseConfig):
	"""
	LLM connection settings for OpenAI-compatible endpoints (Azure/OpenAI gateways).

	Environment variables:
	- OAI_KEY: API key for OpenAI-compatible endpoint
	- OAI_MODEL: Chat model id (default: gpt-4o-mini)
	- OAI_EMBED_MODEL: Embedding model id (default: text-embedding-3-small)
	- OAI_BASE_URL: Optional base URL for OpenAI-compatible endpoint (e.g., https://<resource>.openai.azure.com)
	- OAI_API_VERSION: Optional Azure OpenAI API version (e.g., 2024-06-01)
	"""

	OAI_KEY: str | None = Field(default=None, description="OpenAI API key")
	OAI_MODEL: str = Field(default="gpt-4o-mini", description="Chat model id")
	OAI_EMBED_MODEL: str = Field(default="text-embedding-3-small", description="Embedding model id")
	OAI_BASE_URL: str | None = Field(default=None, description="OpenAI-compatible API base URL")
	OAI_API_VERSION: str | None = Field(default=None, description="Azure OpenAI API version")


@lru_cache()
def get_llm_config() -> LlmConfig:
	"""Return cached LLM config."""
	return LlmConfig()


