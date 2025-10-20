from functools import lru_cache
from pydantic_settings import BaseSettings, SettingsConfigDict
from pydantic import Field

from configuration.redis_config import RedisSettings
from configuration.llm_config import LlmConfig


class GitLabClientSettings(BaseSettings):
    """
    GitLab Client Service configuration settings.
    
    Reuses RedisSettings and LlmConfig from shared library.
    Only GitLab-specific settings are defined here.
    """
    
    # GitLab Configuration
    GITLAB_BASE_URL: str = Field(default="", description="Base URL of GitLab instance")
    GITLAB_VERIFY_SSL: bool = Field(default=True, description="Verify SSL certificates for GitLab")
    HTTP_CONNECTION_TIMEOUT: float = Field(default=30.0, description="HTTP connection timeout in seconds")
    RETRY_MAX_ATTEMPTS: int = Field(default=3, description="Maximum number of HTTP retries")
    RETRY_BACKOFF_FACTOR: float = Field(default=2.0, description="Exponential backoff factor for retries")
    DEFAULT_PAGE_SIZE: int = Field(default=100, description="Default page size for GitLab API pagination")
    
    # Embedding Batch Configuration (extends LlmConfig)
    OAI_EMBED_BATCH: int = Field(default=16, description="Batch size for embedding requests")
    OAI_EMBED_CONCURRENCY: int = Field(default=2, description="Concurrent embedding requests")
    
    # Redis Pub/Sub Configuration (extends RedisSettings)
    EMBEDDINGS_PUBSUB_PREFIX: str = Field(
        default="embeddings:projects:",
        description="Redis pub/sub channel prefix for embedding progress"
    )
    
    # Idempotency Configuration
    IDEMPOTENCY_TTL_SECONDS: int = Field(
        default=86400,
        description="TTL for idempotency keys in seconds (default 24 hours)"
    )
    
    # Composed settings from shared library
    redis: RedisSettings = Field(default_factory=RedisSettings)
    llm: LlmConfig = Field(default_factory=LlmConfig)

    model_config = SettingsConfigDict(
        env_file=".env",
        env_file_encoding='utf-8',
        case_sensitive=True,
        extra='ignore'
    )


@lru_cache()
def get_gitlab_client_settings() -> GitLabClientSettings:
    """Return cached GitLab client settings instance."""
    return GitLabClientSettings()


