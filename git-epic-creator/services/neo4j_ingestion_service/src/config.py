"""
GraphRAG/LLM configuration for the Neo4j ingestion service.
"""

from functools import lru_cache
from pydantic import Field

from configuration.base_config import BaseConfig
from configuration.llm_config import LlmConfig


class GraphRAGSettings(BaseConfig):
	"""
	Minimal settings required for the library-based GraphRAG pipeline.

	Environment variables:
	- OAI_KEY, OAI_MODEL, OAI_EMBED_MODEL_NAME, OAI_EMBED_DEPLOYMENT_NAME, OAI_BASE_URL, OAI_API_VERSION (via LlmConfig)
	- RAG_WORKSPACE_ROOT: Workspace root path
	"""

	llm: LlmConfig = Field(default_factory=LlmConfig, description="Shared LLM configuration")
	RAG_WORKSPACE_ROOT: str = Field(default="./graphrag", description="Workspace root for per-project runs")


@lru_cache()
def get_graphrag_settings() -> GraphRAGSettings:
	"""Return cached GraphRAG settings."""
	return GraphRAGSettings()


