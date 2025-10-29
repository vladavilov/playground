"""Service-specific configuration for AI Workflow Service."""

from functools import lru_cache
from typing import Dict
from pydantic import Field

from configuration.base_config import BaseConfig
from configuration.llm_config import LlmConfig
from configuration.http_client_config import HTTPClientSettings


class AIRequirementsSettings(BaseConfig):
    """Settings specific to the AI requirements generation."""

    WORKFLOW_TIMEOUT_SEC: int = Field(
        default=150,
        description="Maximum workflow execution time in seconds (aborts gracefully before client timeout)",
    )

    CLARIFICATION_SCORE_TARGET: float = Field(
        default=0.7,
        description="Target score threshold for clarification loop",
    )

    EVAL_WEIGHTS: Dict[str, float] = Field(
        default_factory=lambda: {"faithfulness": 0.3, "groundedness": 0.3, "response_relevancy": 0.2, "completeness": 0.2},
        description="Weights for evaluation metrics across axes",
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

    # Shared configurations
    http: HTTPClientSettings = Field(default_factory=HTTPClientSettings)
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

@lru_cache()
def get_ai_requirements_settings() -> AIRequirementsSettings:
    """Get cached service settings."""
    return AIRequirementsSettings()


