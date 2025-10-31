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
	- OAI_MODEL: Chat model id (default: gpt-4o-mini) - used for complex tasks
	- OAI_MODEL_FAST: Fast chat model id (default: gpt-4o-mini) - used for simple/review tasks
	- OAI_EMBED_MODEL_NAME: Embedding model name for tiktoken (default: text-embedding-3-small)
	- OAI_EMBED_DEPLOYMENT_NAME: Azure deployment name for embeddings (optional, defaults to model name)
	- OAI_BASE_URL: Optional base URL for OpenAI-compatible endpoint (e.g., https://<resource>.openai.azure.com)
	- OAI_API_VERSION: Optional Azure OpenAI API version (e.g., 2024-06-01)
	"""

	OAI_KEY: str | None = Field(default=None, description="OpenAI API key")
	OAI_MODEL: str = Field(default="gpt-4o-mini", description="Chat model id for complex tasks")
	OAI_MODEL_FAST: str = Field(default="gpt-4o-mini", description="Fast chat model id for simple/review tasks")
	
	# Separate embedding model name and deployment name for proper tiktoken usage
	OAI_EMBED_MODEL_NAME: str = Field(
		default="text-embedding-3-small",
		description="Embedding model name (for tiktoken and model identification)"
	)
	OAI_EMBED_DEPLOYMENT_NAME: str | None = Field(
		default=None,
		description="Embedding deployment name (Azure OpenAI deployment; defaults to model name if not set)"
	)
	
	OAI_BASE_URL: str | None = Field(default=None, description="OpenAI-compatible API base URL")
	OAI_API_VERSION: str | None = Field(default=None, description="Azure OpenAI API version")
	
	@property
	def embedding_deployment_name(self) -> str:
		"""Get effective embedding deployment name (fallback to model name if not explicitly set)."""
		return self.OAI_EMBED_DEPLOYMENT_NAME or self.OAI_EMBED_MODEL_NAME


@lru_cache()
def get_llm_config() -> LlmConfig:
	"""Return cached LLM config."""
	return LlmConfig()


