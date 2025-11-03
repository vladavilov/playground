"""Service-specific configuration for AI Tasks Service."""

from functools import lru_cache
from typing import Dict
from pydantic import Field

from configuration.base_config import BaseConfig
from configuration.llm_config import LlmConfig
from configuration.redis_config import RedisSettings
from configuration.http_client_config import HTTPClientSettings


class AITasksSettings(BaseConfig):
    """Settings specific to the AI tasks/backlog orchestration."""

    WORKFLOW_TIMEOUT_SEC: int = Field(
        default=150,
        description="Maximum workflow execution time in seconds (aborts gracefully before client timeout)",
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

    # Evaluation optimization (token reduction for metrics)
    MAX_EPICS_FOR_EVAL: int = Field(
        default=3,
        description="Maximum epics to include in evaluation context (token optimization)",
        ge=1,
    )
    MAX_TASKS_PER_EPIC_EVAL: int = Field(
        default=5,
        description="Maximum tasks per epic for evaluation context",
        ge=1,
    )
    MAX_AC_PER_TASK_EVAL: int = Field(
        default=3,
        description="Maximum acceptance criteria per task for evaluation",
        ge=1,
    )

    # Shared configurations
    http: HTTPClientSettings = Field(default_factory=HTTPClientSettings)
    llm: LlmConfig = Field(default_factory=LlmConfig)
    redis: RedisSettings = Field(default_factory=RedisSettings)


@lru_cache()
def get_ai_tasks_settings() -> AITasksSettings:
    """Get cached service settings."""
    return AITasksSettings()


