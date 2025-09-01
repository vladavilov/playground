"""
Celery tasks for Neo4j ingestion.

Thin wrappers that delegate orchestration to the service layer.
"""

from __future__ import annotations

import os
import time
from typing import Any, Dict
from pathlib import Path
import shutil

import structlog
logger = structlog.get_logger(__name__)

from worker.celery_app import celery_app
from tasks.retry import schedule_ingestion_retry
from services.ingestion_service import Neo4jIngestionService
from utils.retry_policy import compute_retry_decision
from utils.neo4j_client import get_neo4j_client
from clients.project_management_client import ProjectManagementClient
from config import get_graphrag_settings
import asyncio
from utils.redis_client import get_redis_client, get_sync_redis_client
from utils.ingestion_gating import post_run_cleanup
from constants import (
    TASK_RUN_GRAPHRAG_JOB,
    QUEUE_NEO4J_INGESTION,
    GATE_NS_INGESTION,
)

@celery_app.task(
    bind=True,
    name=TASK_RUN_GRAPHRAG_JOB,
)
def run_graphrag_job(
    self,
    job_id: str,
    project_id: str,
    attempts: int = 0,
) -> Dict[str, Any]:
    logger.info(
        "TASK START - run_graphrag_job",
        job_id=job_id,
        project_id=project_id,
        attempts=attempts,
        task_id=self.request.id,
        worker_pid=os.getpid(),
    )
    start = time.time()

    if not isinstance(job_id, str) or not job_id.strip():
        raise ValueError("job_id must be a non-empty string")
    if not isinstance(project_id, str) or not project_id.strip():
        raise ValueError("project_id must be a non-empty string")

    # Use a single event loop to handle all async work
    redis_client = get_sync_redis_client()
    ran_successfully = False
    try:
        result = asyncio.run(_run_graphrag_job_async(job_id, project_id, attempts, start))
        ran_successfully = True
        return result
    finally:
        try:
            def _enqueue(job_id_: str, project_id_: str, attempts_: int) -> None:
                celery_app.send_task(
                    TASK_RUN_GRAPHRAG_JOB,
                    args=[job_id_, project_id_, int(attempts_)],
                    queue=QUEUE_NEO4J_INGESTION,
                )

            # Synchronous cleanup equivalent of post_run_cleanup
            running_key = f"{GATE_NS_INGESTION}:running:{project_id}"
            pending_key = f"{GATE_NS_INGESTION}:pending:{project_id}"
            try:
                redis_client.delete(running_key)
            except Exception:
                pass
            script = (
                "if redis.call('get', KEYS[1]) then "
                "redis.call('del', KEYS[1]); return 1 else return 0 end"
            )
            had_pending = False
            try:
                had_pending = bool(redis_client.eval(script, 1, pending_key))
            except Exception:
                had_pending = False
            if had_pending:
                _enqueue(job_id, project_id, int(attempts or 0))
        except Exception:
            pass
        if ran_successfully:
            try:
                root = Path(get_graphrag_settings().RAG_WORKSPACE_ROOT)
                workdir: Path = root / project_id
                shutil.rmtree(workdir, ignore_errors=True)
            except Exception:
                pass


async def _run_graphrag_job_async(
    job_id: str,
    project_id: str,
    attempts: int,
    start_time: float,
) -> Dict[str, Any]:
    """Single-event-loop orchestration for the GraphRAG job."""
    # Distributed lock per project using synchronous Redis lock
    sync_client = get_sync_redis_client()
    lock_key = f"{GATE_NS_INGESTION}:lock:{project_id}"
    sync_lock = sync_client.lock(lock_key, timeout=300, blocking=False)
    if not sync_lock.acquire(blocking=False):
        logger.warning("Project lock busy; skipping execution", project_id=project_id)
        await schedule_ingestion_retry(job_id, project_id, attempts or 0, False, countdown=3)
        return {"job_id": job_id, "project_id": project_id, "attempts": attempts, "skipped": True}

    try:
        # Mark project as rag_processing (best-effort)
        try:
            async with ProjectManagementClient() as pm:
                await pm.update_project_status(
                    project_id=project_id,
                    status="rag_processing",
                )
        except Exception:
            logger.error("Failed to mark project as rag_processing", project_id=project_id)

        # Run pipeline
        service = Neo4jIngestionService(client=get_neo4j_client())
        service_result = await service.run_graphrag_pipeline(project_id=project_id)

        result: Dict[str, Any] = {
            "job_id": job_id,
            "project_id": project_id,
            "attempts": attempts,
            "counts": service_result.get("counts", {}),
            "duration_ms": service_result.get(
                "duration_ms", round((time.time() - start_time) * 1000, 2)
            ),
        }

        # On success, publish status with counts (best-effort)
        try:
            counts = result.get("counts", {})
            processed_count = int(counts.get("documents", 0))
            total_count = max(processed_count, 1)
            async with ProjectManagementClient() as pm:
                await pm.update_project_status(
                    project_id=project_id,
                    processed_count=processed_count,
                    total_count=total_count,
                    status="rag_ready",
                )
        except Exception:
            pass

        return result
    except Exception as exc:  # noqa: BLE001
        # On failure, publish error status (best-effort)
        try:
            async with ProjectManagementClient() as pm:
                await pm.update_project_status(
                    project_id=project_id,
                    status="rag_failed",
                    error_message=str(exc),
                )
        except Exception:
            pass

        # Compute next attempts and schedule retry or DLQ based on configured policy (shared)
        to_dlq, countdown, next_attempts = compute_retry_decision(attempts or 0)
        if to_dlq:
            await schedule_ingestion_retry(job_id, project_id, next_attempts, True)
        else:
            await schedule_ingestion_retry(job_id, project_id, next_attempts, False, countdown=countdown)
        # Re-raise to surface failure to Celery for visibility
        raise
    finally:
        try:
            sync_lock.release()
        except Exception:
            pass
