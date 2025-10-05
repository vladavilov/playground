"""Service-specific configuration for AI Tasks Service."""

from functools import lru_cache
from typing import Dict
from pydantic import Field

from configuration.base_config import BaseConfig
from configuration.llm_config import LlmConfig
from configuration.redis_config import RedisSettings


class AITasksSettings(BaseConfig):
    """Settings specific to the AI tasks/backlog orchestration."""

    GRAPH_RAG_BASE_URL: str = Field(
        default="http://neo4j-retrieval-service:8000",
        description="Base URL for GraphRAG/retrieval service",
    )

    HTTP_TIMEOUT_SEC: float = Field(
        default=30.0,
        description="Timeout in seconds for HTTP calls",
    )

    GITLAB_INGESTION_BASE_URL: str = Field(
        default="http://gitlab-client-service:8000",
        description="Base URL for GitLab client service",
    )

    CLARIFICATION_SCORE_TARGET: float = Field(
        default=0.75,
        description="Target score threshold for clarification loop (backlog)",
    )

    EVAL_WEIGHTS: Dict[str, float] = Field(
        default_factory=lambda: {
            "coverage": 0.3,
            "specificity": 0.3,
            "feasibility": 0.2,
            "duplication": 0.2,
        },
        description="Weights for backlog evaluation metrics",
    )

    MAX_AGENT_ITERS: int = Field(
        default=3,
        description="Maximum number of agentic refinement iterations",
    )

    RETRIEVAL_TOP_K: int = Field(
        default=2,
        description="Top-K for retrieval provider",
        ge=1,
    )

    RETRY_MAX_ATTEMPTS: int = Field(
        default=3,
        description="Max retry attempts for retrieval client",
    )

    RETRIEVAL_BACKOFF_BASE_SEC: float = Field(
        default=0.2,
        description="Base seconds for exponential backoff",
    )

    SIMILARITY_THRESHOLD: float = Field(
        default=0.83,
        description="Cosine similarity threshold for duplicate detection",
        ge=0.0,
        le=1.0,
    )

    # Shared LLM config (Azure OpenAI)
    llm: LlmConfig = Field(default_factory=LlmConfig)
    LLM_TIMEOUT_SEC: float = Field(
        default=20.0,
        description="Timeout for LLM requests",
    )
    LLM_TEMPERATURE: float = Field(
        default=0.2,
        ge=0.0,
        le=2.0,
        description="Temperature for LLM requests",
    )

    # Shared Redis config
    redis: RedisSettings = Field(default_factory=RedisSettings)


@lru_cache()
def get_ai_tasks_settings() -> AITasksSettings:
    """Get cached service settings."""
    return AITasksSettings()


