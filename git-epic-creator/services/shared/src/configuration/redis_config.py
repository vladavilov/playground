"""
Redis configuration settings.
"""

from pydantic import Field
from .base_config import BaseConfig
from functools import lru_cache

class RedisSettings(BaseConfig):
    """
    Defines the Redis configuration settings.
    """
    REDIS_URL: str = Field(default="redis://localhost:6379", description="Redis connection URL")
    REDIS_PASSWORD: str | None = Field(default=None, description="Redis password")
    REDIS_DB: int = Field(default=0, description="Redis database number")
    REDIS_MAX_CONNECTIONS: int = Field(default=10, description="Maximum number of Redis connections")
    REDIS_RETRY_ON_TIMEOUT: bool = Field(default=True, description="Whether to retry on timeout")
    REDIS_SOCKET_CONNECT_TIMEOUT: float = Field(default=5.0, description="Socket connection timeout in seconds")
    REDIS_SOCKET_TIMEOUT: float = Field(default=5.0, description="Socket timeout in seconds")
    
    # Redis Pub/Sub specific settings
    REDIS_PUBSUB_CHANNEL_PREFIX: str = Field(default="project_progress", description="Channel prefix for pub/sub messages")
    REDIS_PUBSUB_MAX_CONNECTIONS: int = Field(default=5, description="Maximum pub/sub connections")
    REDIS_PUBSUB_CONNECTION_TIMEOUT: float = Field(default=10.0, description="Pub/sub connection timeout in seconds")
    REDIS_PUBSUB_RETRY_ATTEMPTS: int = Field(default=3, description="Number of retry attempts for pub/sub operations")
    REDIS_PUBSUB_RETRY_DELAY: float = Field(default=2.0, description="Delay between pub/sub retry attempts in seconds")

@lru_cache()
def get_redis_settings() -> RedisSettings:
    """
    Creates a cached instance of RedisSettings.
    This ensures that the settings are loaded only once and reused across the application.
    """
    return RedisSettings() 