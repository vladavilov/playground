"""
GraphRAG configuration settings.
"""

from functools import lru_cache
from pydantic import Field, field_validator
from .base_config import BaseConfig


class GraphragSettings(BaseConfig):
    """
    Defines GraphRAG and Azure OpenAI configuration with sensible, performance-oriented defaults.
    """

    # Note: Deployment names are taken from LlmConfig directly in settings builder.

    # Async mode used by GraphRAG model clients
    GRAPHRAG_ASYNC_MODE: str = Field(default="threaded", description="Async mode: 'asyncio' or 'threaded'")

    # Concurrency and throttling (chat)
    GRAPHRAG_LLM_THREAD_COUNT: int = Field(default=16, description="Threads used by chat model parallelization")
    GRAPHRAG_LLM_THREAD_STAGGER: float = Field(default=0.2, description="Seconds to stagger chat threads start")
    GRAPHRAG_LLM_CONCURRENT_REQUESTS: int = Field(default=12, description="Max concurrent chat requests")
    GRAPHRAG_LLM_TOKENS_PER_MINUTE: int | str = Field(default=0, description="Leaky-bucket TPM limit (>0), 'auto', or 0 to omit")
    GRAPHRAG_LLM_REQUESTS_PER_MINUTE: int | str = Field(default=0, description="Leaky-bucket RPM limit (>0), 'auto', or 0 to omit")

    # Concurrency and throttling (embeddings)
    GRAPHRAG_EMBEDDING_THREAD_COUNT: int = Field(default=16, description="Threads used by embedding parallelization")
    GRAPHRAG_EMBEDDING_THREAD_STAGGER: float = Field(default=0.2, description="Seconds to stagger embedding threads start")
    GRAPHRAG_EMBEDDING_CONCURRENT_REQUESTS: int = Field(default=12, description="Max concurrent embedding requests")
    GRAPHRAG_EMBEDDING_TOKENS_PER_MINUTE: int | str = Field(default=60000, description="Leaky-bucket TPM limit (>0), 'auto', or 0 to omit")
    GRAPHRAG_EMBEDDING_REQUESTS_PER_MINUTE: int | str = Field(default=2000, description="Leaky-bucket RPM limit (>0), 'auto', or 0 to omit")

    # Extraction workflow controls
    GRAPHRAG_EXTRACT_MAX_GLEANINGS: int = Field(default=2, description="Max gleaning cycles in extract_graph")
    
    # LLM sampling parameters for deterministic extraction
    GRAPHRAG_LLM_TEMPERATURE: float = Field(default=0.0, description="Temperature for entity/relationship extraction (0 = deterministic)")
    GRAPHRAG_LLM_TOP_P: float = Field(default=1.0, description="Top-p nucleus sampling (1.0 = consider all tokens)")
    GRAPHRAG_LLM_N: int = Field(default=1, description="Number of completions to generate")

    @field_validator("GRAPHRAG_LLM_THREAD_COUNT", "GRAPHRAG_LLM_CONCURRENT_REQUESTS",
                     "GRAPHRAG_EMBEDDING_THREAD_COUNT", "GRAPHRAG_EMBEDDING_CONCURRENT_REQUESTS",
                     "GRAPHRAG_EXTRACT_MAX_GLEANINGS", "GRAPHRAG_LLM_N")
    @classmethod
    def _validate_non_negative(cls, v: int) -> int:
        if v < 0:
            raise ValueError("Value must be non-negative")
        return v
    
    @field_validator("GRAPHRAG_LLM_TEMPERATURE", "GRAPHRAG_LLM_TOP_P")
    @classmethod
    def _validate_sampling_params(cls, v: float, info) -> float:
        field_name = info.field_name
        if field_name == "GRAPHRAG_LLM_TEMPERATURE":
            if v < 0.0 or v > 2.0:
                raise ValueError("Temperature must be between 0.0 and 2.0")
        elif field_name == "GRAPHRAG_LLM_TOP_P":
            if v < 0.0 or v > 1.0:
                raise ValueError("Top-p must be between 0.0 and 1.0")
        return v

    @field_validator("GRAPHRAG_LLM_TOKENS_PER_MINUTE", "GRAPHRAG_LLM_REQUESTS_PER_MINUTE",
                     "GRAPHRAG_EMBEDDING_TOKENS_PER_MINUTE", "GRAPHRAG_EMBEDDING_REQUESTS_PER_MINUTE")
    @classmethod
    def _validate_rate_limits(cls, v: int | str) -> int | str:
        if isinstance(v, str):
            if v.strip().lower() == "auto":
                return "auto"
            # allow numeric strings
            if v.strip().isdigit():
                v = int(v.strip())
            else:
                raise ValueError("Tokens/Requests per minute must be a positive int, 'auto', or 0")
        if isinstance(v, int):
            if v < 0:
                raise ValueError("Tokens/Requests per minute must be >= 0 or 'auto'")
        return v

@lru_cache()
def get_graphrag_settings() -> GraphragSettings:
    """
    Returns a cached instance of the GraphragSettings.
    """
    return GraphragSettings()


