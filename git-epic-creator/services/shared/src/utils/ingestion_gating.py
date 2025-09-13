"""Shared ingestion enqueue gating and pending follow-up utilities.

Centralizes the single-flight, pending marker, and debounce logic so that
multiple services can reuse consistent behavior.
"""

from __future__ import annotations

import asyncio
from typing import Callable, Optional

import redis as redis_sync

from redis.asyncio import Redis


def _build_keys(namespace: str, project_id: str) -> tuple[str, str, str]:
    coalesce_key = f"{namespace}:trigger:coalesce:{project_id}"
    running_key = f"{namespace}:running:{project_id}"
    pending_key = f"{namespace}:pending:{project_id}"
    return coalesce_key, running_key, pending_key


async def should_enqueue_async(
    namespace: str,
    project_id: str,
    *,
    client: "Redis",
    running_ttl_seconds: int = 300,
    pending_ttl_seconds: int = 60,
    coalesce_ttl_seconds: int = 5,
) -> bool:
    """
    Async variant of enqueue gating for use in async contexts (e.g., FastAPI handlers).
    """
    try:
        coalesce_key, running_key, pending_key = _build_keys(namespace, project_id)

        # Debounce: suppress bursts for a brief window
        ok = await client.set(coalesce_key, "1", nx=True, ex=coalesce_ttl_seconds)
        if not ok:
            return False

        # Single-flight: set running if not present
        ok_running = await client.set(running_key, "1", nx=True, ex=running_ttl_seconds)
        if ok_running:
            return True

        # Already running: set a short pending marker and skip enqueue
        try:
            await client.set(pending_key, "1", nx=True, ex=pending_ttl_seconds)
        except Exception:
            pass
        return False
    except Exception:
        # On Redis errors, default to allowing enqueue to avoid blocking progress
        return True

def should_enqueue_sync(
    namespace: str,
    project_id: str,
    *,
    client: "redis_sync.Redis",
    running_ttl_seconds: int = 300,
    pending_ttl_seconds: int = 60,
    coalesce_ttl_seconds: int = 5,
) -> bool:
    """
    Sync variant of enqueue gating for use in synchronous contexts (e.g., Celery tasks).
    """
    try:
        coalesce_key, running_key, pending_key = _build_keys(namespace, project_id)

        # Debounce: suppress bursts for a brief window
        if not client.set(coalesce_key, "1", nx=True, ex=coalesce_ttl_seconds):
            return False

        # Single-flight: set running if not present
        if client.set(running_key, "1", nx=True, ex=running_ttl_seconds):
            return True

        # Already running: set a short pending marker and skip enqueue
        try:
            client.set(pending_key, "1", nx=True, ex=pending_ttl_seconds)
        except Exception:
            pass
        return False
    except Exception:
        # On Redis errors, default to allowing enqueue to avoid blocking progress
        return True

def post_run_cleanup(
    namespace: str,
    job_id: str,
    project_id: str,
    attempts: int,
    *,
    client: Redis,
    enqueue_callable: Optional[Callable[[str, str, int], None]] = None,
) -> bool:
    """
    Generic post-run cleanup for a namespaced workflow.
    Clears running marker and, if a pending marker exists, enqueues one follow-up.
    """
    try:
        redis_client = client
        _, running_key, pending_key = _build_keys(namespace, project_id)

        # Best-effort: clear running marker
        try:
            asyncio.run(redis_client.delete(running_key))
        except Exception:
            pass

        # Atomic check+delete for pending
        script = (
            "if redis.call('get', KEYS[1]) then "
            "redis.call('del', KEYS[1]); return 1 else return 0 end"
        )
        had_pending = False
        try:
            had_pending = bool(asyncio.run(redis_client.eval(script, 1, pending_key)))
        except Exception:
            had_pending = False

        if had_pending and enqueue_callable is not None:
            enqueue_callable(job_id, project_id, int(attempts))
            return True
        return False
    except Exception:
        return False
