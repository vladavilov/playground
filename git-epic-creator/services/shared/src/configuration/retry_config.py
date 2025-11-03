"""
Common retry/backoff settings used across services for retryable messages and channels.
"""

from functools import lru_cache
from pydantic_settings import BaseSettings


class RetrySettings(BaseSettings):
    """Environment-driven settings for retry/backoff policy."""

    RETRY_MAX_ATTEMPTS: int = 3
    RETRY_BACKOFF_BASE_SEC: int = 1  # Reduced from 2s for faster error recovery
    RETRY_BACKOFF_FACTOR: int = 2
    RETRY_BACKOFF_MAX_SEC: int = 60

    model_config = {
        "env_file": ".env",
        "case_sensitive": True,
    }


@lru_cache()
def get_retry_settings() -> RetrySettings:
    """Return cached retry settings loaded from environment."""
    return RetrySettings()


