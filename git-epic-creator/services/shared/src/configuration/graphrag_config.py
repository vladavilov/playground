"""
GraphRAG/LLM configuration settings used by the ingestion pipeline.
"""

from functools import lru_cache
from pydantic import Field

from .base_config import BaseConfig


class GraphRAGSettings(BaseConfig):
	"""
	Minimal settings required for the library-based GraphRAG pipeline.

	Environment variables:
	- OAI_KEY: API key for OpenAI-compatible endpoint
	- OAI_MODEL: Chat model id (default: gpt-4o-mini)
	- OAI_EMBED_MODEL: Embedding model id (default: text-embedding-3-small)
	- OAI_BASE_URL: Optional base URL for OpenAI-compatible endpoint (e.g., http://localhost:8010/v1)
	- OAI_API_VERSION: Optional Azure OpenAI API version (e.g., 2024-06-01)
	"""

	OAI_KEY: str | None = Field(default=None, description="OpenAI API key")
	OAI_MODEL: str = Field(default="gpt-4o-mini", description="Chat model id")
	OAI_EMBED_MODEL: str = Field(default="text-embedding-3-small", description="Embedding model id")
	OAI_BASE_URL: str | None = Field(default=None, description="OpenAI-compatible API base URL")
	OAI_API_VERSION: str | None = Field(default=None, description="Azure OpenAI API version")
	RAG_WORKSPACE_ROOT: str = Field(default="./graphrag", description="Workspace root for per-project runs")


@lru_cache()
def get_graphrag_settings() -> GraphRAGSettings:
	"""Return cached GraphRAG settings."""
	return GraphRAGSettings()
