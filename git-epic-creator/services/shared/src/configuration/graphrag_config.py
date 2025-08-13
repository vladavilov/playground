"""
GraphRAG/LLM configuration settings.
"""

from pydantic import Field, field_validator
from functools import lru_cache
from .base_config import BaseConfig


class GraphRAGSettings(BaseConfig):
    """
    Configuration for GraphRAG workspace and concurrency.
    """

    RAG_WORKSPACE_ROOT: str = Field(default="./graphrag", description="Workspace root for per-project runs")
    GRAPHRAG_API_KEY: str | None = Field(default=None, description="LLM API key for GraphRAG (optional)")

    GRAPHRAG_LLM_THREAD_COUNT: int = Field(default=8, description="Max threads for LLM operations")
    GRAPHRAG_EMBEDDING_THREAD_COUNT: int = Field(default=8, description="Max threads for embedding operations")

    @field_validator("GRAPHRAG_LLM_THREAD_COUNT", "GRAPHRAG_EMBEDDING_THREAD_COUNT")
    @classmethod
    def _validate_positive_int(cls, v: int) -> int:
        if v <= 0:
            raise ValueError("Thread counts must be positive integers")
        return v


@lru_cache()
def get_graphrag_settings() -> GraphRAGSettings:
    """Return cached GraphRAG settings."""
    return GraphRAGSettings()


