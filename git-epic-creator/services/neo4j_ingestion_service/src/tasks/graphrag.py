"""
Celery tasks for Neo4j ingestion.

Thin wrappers that delegate orchestration to the service layer.
"""

from __future__ import annotations

import os
import time
from typing import Any, Dict

import structlog
logger = structlog.get_logger(__name__)

from worker.celery_app import celery_app
from tasks.retry import schedule_ingestion_retry_task
from services.ingestion_service import Neo4jIngestionService
from utils.neo4j_client import get_neo4j_client
from clients.project_management_client import ProjectManagementClient
import asyncio

@celery_app.task(
    bind=True,
    name="tasks.neo4j_ingestion.run_graphrag_job",
    acks_late=True,
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

    try:
        # Mark project as processing (best-effort)
        try:
            asyncio.run(
                ProjectManagementClient().update_project_status(
                    project_id=project_id,
                    status="processing",
                )
            )
        except Exception:
            logger.error("Failed to mark project as processing", project_id=project_id)
            pass

        service = Neo4jIngestionService(client=get_neo4j_client())
        service_result = service.run_graphrag_pipeline(project_id=project_id)

        result = {
            "job_id": job_id,
            "project_id": project_id,
            "attempts": attempts,
            "counts": service_result.get("counts", {}),
            "duration_ms": service_result.get(
                "duration_ms", round((time.time() - start) * 1000, 2)
            ),
        }
        # On success, publish status with counts (best-effort)
        try:
            counts = result.get("counts", {})
            processed_count = int(counts.get("documents", 0))
            total_count = max(processed_count, 1)
            asyncio.run(
                ProjectManagementClient().update_project_status(
                    project_id=project_id,
                    processed_count=processed_count,
                    total_count=total_count,
                    status="rag_ready",
                )
            )
        except Exception:
            pass
        return result
    except Exception as exc:  # noqa: BLE001
        # On failure, publish error status (best-effort)
        try:
            asyncio.run(
                ProjectManagementClient().update_project_status(
                    project_id=project_id,
                    status="rag_failed",
                    error_message=str(exc),
                )
            )
        except Exception:
            pass
        # Compute next attempts and schedule retry or DLQ based on configured policy (shared)
        from utils.retry_policy import compute_retry_decision  # type: ignore

        to_dlq, countdown, next_attempts = compute_retry_decision(attempts or 0)

        if to_dlq:
            schedule_ingestion_retry_task.apply_async(
                args=(job_id, project_id, next_attempts, True)
            )
        else:
            schedule_ingestion_retry_task.apply_async(
                args=(job_id, project_id, next_attempts, False),
                countdown=countdown,
            )
        # Re-raise to surface failure to caller/monitoring
        raise
