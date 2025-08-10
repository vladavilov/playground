"""Minimal Redis client creation utilities."""

from functools import lru_cache
import redis.asyncio as redis
import structlog
from configuration.redis_config import RedisSettings
from configuration.common_config import get_app_settings

logger = structlog.get_logger(__name__)

@lru_cache()
def get_redis_client() -> redis.Redis:
    """Return a cached Redis client from settings."""
    settings = get_app_settings()
    cfg: RedisSettings = settings.redis
    client = redis.from_url(
        cfg.REDIS_URL,
        password=cfg.REDIS_PASSWORD,
        db=cfg.REDIS_DB,
        max_connections=cfg.REDIS_MAX_CONNECTIONS,
        socket_connect_timeout=cfg.REDIS_SOCKET_CONNECT_TIMEOUT,
        socket_timeout=cfg.REDIS_SOCKET_TIMEOUT,
        decode_responses=True,
    )
    logger.info("Redis client created", redis_url=cfg.REDIS_URL, redis_db=cfg.REDIS_DB)
    return client
