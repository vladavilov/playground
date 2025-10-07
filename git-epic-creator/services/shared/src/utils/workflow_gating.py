"""Unified gating utilities: debounce, lock-aware enqueue, and locked execution."""

from __future__ import annotations

from typing import Any, Awaitable, Callable, Dict, Optional

import structlog
import redis as redis_sync
from redis.asyncio import Redis

from utils.redis_client import get_sync_redis_client
from utils.retry_policy import compute_retry_decision


logger = structlog.get_logger(__name__)


def _keys(namespace: str, identifier: str) -> tuple[str, str, str]:
    coalesce_key = f"{namespace}:trigger:coalesce:{identifier}"
    lock_key = f"{namespace}:lock:{identifier}"
    pending_key = f"{namespace}:pending:{identifier}"
    return coalesce_key, lock_key, pending_key


async def gate_and_enqueue_async(
    namespace: str,
    identifier: str,
    *,
    client: "Redis",
    enqueue: Callable[[], None],
    coalesce_ttl_seconds: int = 5,
    pending_ttl_seconds: int = 60,
) -> bool:
    """Debounce and lock-aware enqueue in async contexts.

    - Debounce bursts using a short coalesce key
    - If lock is free, enqueue immediately
    - If locked, set pending marker for a single follow-up and skip enqueue
    """
    try:
        coalesce_key, lock_key, pending_key = _keys(namespace, identifier)
        ok = await client.set(coalesce_key, "1", nx=True, ex=coalesce_ttl_seconds)
        if not ok:
            return False
        locked = bool(await client.exists(lock_key))
        if not locked:
            enqueue()
            return True
        try:
            await client.set(pending_key, "1", nx=True, ex=pending_ttl_seconds)
        except Exception:
            pass
        return True
    except Exception:
        # On Redis errors, best-effort enqueue
        try:
            enqueue()
            return True
        except Exception:
            return False


def gate_and_enqueue_sync(
    namespace: str,
    identifier: str,
    *,
    client: "redis_sync.Redis",
    enqueue: Callable[[], None],
    coalesce_ttl_seconds: int = 5,
    pending_ttl_seconds: int = 60,
) -> bool:
    """Debounce and lock-aware enqueue in sync contexts."""
    try:
        coalesce_key, lock_key, pending_key = _keys(namespace, identifier)
        if not client.set(coalesce_key, "1", nx=True, ex=coalesce_ttl_seconds):
            return False
        locked = bool(client.exists(lock_key))
        if not locked:
            enqueue()
            return True
        try:
            client.set(pending_key, "1", nx=True, ex=pending_ttl_seconds)
        except Exception:
            pass
        return True
    except Exception:
        try:
            enqueue()
            return True
        except Exception:
            return False


def cleanup_after_run_sync(
    namespace: str,
    identifier: str,
    *,
    client: "redis_sync.Redis",
    enqueue_follow_up: Optional[Callable[[], None]] = None,
) -> bool:
    """After a locked run, check and clear pending once, optionally enqueue follow-up."""
    try:
        _, _, pending_key = _keys(namespace, identifier)
        script = (
            "if redis.call('get', KEYS[1]) then "
            "redis.call('del', KEYS[1]); return 1 else return 0 end"
        )
        had_pending = False
        try:
            had_pending = bool(client.eval(script, 1, pending_key))
        except Exception:
            had_pending = False
        if had_pending and enqueue_follow_up is not None:
            try:
                enqueue_follow_up()
            except Exception:
                pass
            return True
        return False
    except Exception:
        return False


async def run_with_lock(
    *,
    namespace: str,
    identifier: str,
    job_id: str,
    attempts: int,
    execute: Callable[[], Awaitable[Dict[str, Any]]],
    schedule_retry: Callable,
    enqueue_follow_up: Optional[Callable[[str, str, int], None]] = None,
    running_ttl_seconds: int = 300,
    pending_ttl_seconds: int = 60,
    authorization_header: Optional[str] = None,
) -> Dict[str, Any]:
    """Execute under a per-identifier lock; set pending + backoff on contention."""
    sync_client = get_sync_redis_client()
    _, lock_key, pending_key = _keys(namespace, identifier)
    sync_lock = sync_client.lock(lock_key, timeout=running_ttl_seconds, blocking=False)
    if not sync_lock.acquire(blocking=False):
        logger.warning("Lock busy; skipping execution", namespace=namespace, identifier=identifier)
        try:
            try:
                sync_client.set(pending_key, "1", nx=True, ex=pending_ttl_seconds)
            except Exception:
                pass
        except Exception:
            pass
        try:
            _, countdown, next_attempts = compute_retry_decision(attempts or 0)
        except Exception:
            countdown, next_attempts = 5, (attempts or 0) + 1
        await schedule_retry(job_id, identifier, int(next_attempts), False, countdown=countdown, authorization_header=authorization_header)
        return {"job_id": job_id, "project_id": identifier, "attempts": attempts, "skipped": True}

    try:
        result = await execute()
        if isinstance(result, dict):
            result.setdefault("job_id", job_id)
            result.setdefault("project_id", identifier)
            result.setdefault("attempts", attempts)
        return result
    finally:
        try:
            sync_lock.release()
        except Exception:
            pass
        # Check pending and enqueue follow-up if present
        try:
            script = (
                "if redis.call('get', KEYS[1]) then "
                "redis.call('del', KEYS[1]); return 1 else return 0 end"
            )
            had_pending = False
            try:
                had_pending = bool(sync_client.eval(script, 1, pending_key))
            except Exception:
                had_pending = False
            if had_pending and enqueue_follow_up is not None:
                try:
                    enqueue_follow_up(job_id, identifier, int(attempts))
                except Exception:
                    pass
        except Exception:
            pass


