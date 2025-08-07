"""
Redis client utilities following SOLID principles.
Separates client creation from configuration.
"""

from functools import lru_cache
import redis.asyncio as redis
import structlog
from configuration.redis_config import RedisSettings
from configuration.common_config import get_app_settings

logger = structlog.get_logger(__name__)

class RedisClientFactory:
    """A factory for creating Redis clients, ensuring a clean separation of concerns."""
    
    @staticmethod
    def create_client(settings: RedisSettings) -> redis.Redis:
        """Create a Redis client from settings."""
        return redis.from_url(
            settings.REDIS_URL,
            password=settings.REDIS_PASSWORD,
            db=settings.REDIS_DB,
            max_connections=settings.REDIS_MAX_CONNECTIONS,
            socket_connect_timeout=settings.REDIS_SOCKET_CONNECT_TIMEOUT,
            socket_timeout=settings.REDIS_SOCKET_TIMEOUT,
            decode_responses=True
        )

class RedisClient:
    """A client for interacting with a Redis server."""

    def __init__(self, settings: RedisSettings):
        self.settings = settings
        self.client = RedisClientFactory.create_client(settings)

        logger.info("Redis client created", redis_url=settings.REDIS_URL, redis_db=settings.REDIS_DB)

    def get_client(self) -> redis.Redis:
        """Get the Redis client instance."""
        return self.client

    async def close(self):
        """Close the Redis client connection."""
        await self.client.close()
        logger.info("Redis client closed")

    async def __aenter__(self):
        return self.client

    async def __aexit__(self, exc_type, exc_val, exc_tb):
        await self.close()

@lru_cache()
def get_redis_client() -> redis.Redis:
    """Get a Redis client, creating it if necessary."""
    settings = get_app_settings()
    redis_client = RedisClient(settings.redis)
    return redis_client.get_client()
