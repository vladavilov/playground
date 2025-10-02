"""Minimal Redis client creation utilities."""

import threading
import asyncio
import redis.asyncio as redis
import redis as redis_sync
import structlog
from configuration.redis_config import RedisSettings
from configuration.common_config import get_app_settings

logger = structlog.get_logger(__name__)

_thread_local_clients: dict[int, redis.Redis] = {}


def create_redis_client(is_pubsub_client: bool = False) -> redis.Redis:
    """Create a fresh asyncio Redis client from settings.

    Args:
        is_pubsub_client: When True, configures the client for long-lived pub/sub
            consumption (infinite socket timeout, binary payloads).
    """
    settings = get_app_settings()
    cfg: RedisSettings = settings.redis
    client = redis.from_url(
        cfg.REDIS_URL,
        password=cfg.REDIS_PASSWORD,
        db=cfg.REDIS_DB,
        max_connections=cfg.REDIS_MAX_CONNECTIONS,
        socket_connect_timeout=cfg.REDIS_SOCKET_CONNECT_TIMEOUT,
        socket_timeout=(None if is_pubsub_client else cfg.REDIS_SOCKET_TIMEOUT),
        decode_responses=(False if is_pubsub_client else True),
    )
    if is_pubsub_client:
        logger.info("Redis pub/sub client created", redis_url=cfg.REDIS_URL, redis_db=cfg.REDIS_DB)
    else:
        logger.info("Redis client created", redis_url=cfg.REDIS_URL, redis_db=cfg.REDIS_DB)
    return client


def get_redis_client(is_pubsub_client=False) -> redis.Redis:
    """Return a per-thread cached Redis client from settings.

    This avoids cross-thread/loop sharing issues by keeping one client per thread.
    """
    thread_id = threading.get_ident()
    client = _thread_local_clients.get(thread_id)
    if client is None:
        client = create_redis_client(is_pubsub_client=is_pubsub_client)
        _thread_local_clients[thread_id] = client
    return client


def get_sync_redis_client() -> "redis_sync.Redis":
    """Create a synchronous Redis client using the same settings as the asyncio client.

    This is intended for use in synchronous contexts (e.g., Celery tasks) where
    background asyncio tasks are undesirable.
    """
    settings = get_app_settings()
    cfg: RedisSettings = settings.redis
    client = redis_sync.from_url(
        cfg.REDIS_URL,
        password=cfg.REDIS_PASSWORD,
        db=cfg.REDIS_DB,
        max_connections=cfg.REDIS_MAX_CONNECTIONS,
        socket_connect_timeout=cfg.REDIS_SOCKET_CONNECT_TIMEOUT,
        socket_timeout=cfg.REDIS_SOCKET_TIMEOUT,
        decode_responses=True,
    )
    logger.info("Redis sync client created", redis_url=cfg.REDIS_URL, redis_db=cfg.REDIS_DB)
    return client


class RedisDistributedLock:
    """Simple distributed lock using SET NX EX with periodic TTL refresh and safe release.

    Usage:
        lock = RedisDistributedLock(client, key, ttl_seconds, token)
        acquired = await lock.acquire()
        ...
        await lock.release()
    """

    def __init__(self, client: redis.Redis, key: str, ttl_seconds: int, token: str) -> None:
        self._client = client
        self._key = key
        self._ttl = ttl_seconds
        self._token = token
        self._stop = asyncio.Event()
        self._refresher_task: asyncio.Task | None = None

    async def acquire(self) -> bool:
        try:
            ok = await self._client.set(self._key, self._token, nx=True, ex=self._ttl)
            if ok:
                # Start background refresher
                self._refresher_task = asyncio.create_task(self._refresh_loop())
            return bool(ok)
        except Exception as e:
            logger.error("Redis lock acquire failed", key=self._key, error=str(e))
            return False

    async def release(self) -> None:
        try:
            self._stop.set()
            # Do not await the refresher task to avoid bubbling CancelledError
            # in environments where the event loop is being torn down.
            if self._refresher_task is not None:
                try:
                    self._refresher_task.cancel()
                except Exception:
                    pass
            script = (
                "if redis.call('get', KEYS[1]) == ARGV[1] then "
                "return redis.call('del', KEYS[1]) else return 0 end"
            )
            try:
                await self._client.eval(script, 1, self._key, self._token)
            except Exception as e:
                logger.debug("Redis lock release failed (ignored)", key=self._key, error=str(e))
        except Exception:
            pass

    async def _refresh_loop(self) -> None:
        try:
            while not self._stop.is_set():
                try:
                    await self._client.expire(self._key, self._ttl)
                except Exception:
                    pass
                try:
                    await asyncio.wait_for(self._stop.wait(), timeout=max(1, self._ttl // 2))
                except asyncio.TimeoutError:
                    continue
                except asyncio.CancelledError:
                    break
        except Exception:
            pass


async def coalesce_retry(client: redis.Redis, project_id: str, ttl_seconds: int = 5) -> bool:
    """Return True if this call should proceed (key set), False if coalesced.

    Creates a transient NX key to suppress duplicate retries for a short window.
    """
    try:
        key = f"graphrag:coalesce:{project_id}"
        ok = await client.set(key, "1", nx=True, ex=ttl_seconds)
        return bool(ok)
    except Exception as e:
        logger.debug("Coalesce check failed; allowing retry", error=str(e))
        return True


def create_namespaced_lock(
    client: redis.Redis,
    *,
    namespace: str,
    identifier: str,
    ttl_seconds: int,
    token: str,
) -> RedisDistributedLock:
    """Factory to create a namespaced lock with a consistent key scheme.

    Key format: "{namespace}:lock:{identifier}"
    """
    key = f"{namespace}:lock:{identifier}"
    return RedisDistributedLock(client, key, ttl_seconds, token)
