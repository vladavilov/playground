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
            retry_on_timeout=settings.REDIS_RETRY_ON_TIMEOUT,
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


class RedisHealthChecker:
    """A class to check the health of the Redis server."""

    @staticmethod
    async def check_health(client: redis.Redis) -> dict:
        """
        Check Redis connection health.
        
        Args:
            client: Redis client instance
            
        Returns:
            dict: Health check results
        """
        try:
            await client.ping()
            logger.info("Redis health check passed")
            return {"healthy": True}
        except Exception as e:
            logger.error("Redis health check failed", error=str(e))
            return {"healthy": False, "error": str(e)}
    
    @staticmethod
    async def check_health_with_details(client: redis.Redis) -> dict[str, any]:
        """
        Check Redis connection health with detailed information.
        
        Args:
            client: Redis client instance
            
        Returns:
            dict: Health check results with details
        """
        try:
            # Test basic connectivity
            ping_result = await client.ping()
            
            # Get server info
            info = await client.info()
            
            result = {
                "healthy": True,
                "ping": ping_result,
                "version": info.get("redis_version", "unknown"),
                "connected_clients": info.get("connected_clients", 0),
                "used_memory": info.get("used_memory", 0),
                "uptime_in_seconds": info.get("uptime_in_seconds", 0)
            }
            
            logger.info("Redis detailed health check passed", **result)
            return result
            
        except Exception as e:
            result = {
                "healthy": False,
                "error": str(e),
                "ping": False
            }
            logger.error("Redis detailed health check failed", **result)
            return result

# Convenience function for backward compatibility
async def check_redis_health(client: redis.Redis) -> bool:
    """
    Check Redis connection health.
    Convenience wrapper for RedisHealthChecker.
    
    Args:
        client: Redis client instance
        
    Returns:
        bool: True if Redis is healthy, False otherwise
    """
    result = await RedisHealthChecker.check_health(client)
    return result.get("healthy", False)