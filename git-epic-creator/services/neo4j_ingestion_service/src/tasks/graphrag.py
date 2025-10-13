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
from tasks.retry import schedule_ingestion_retry
from services.ingestion_service import Neo4jIngestionService
from utils.retry_policy import compute_retry_decision
from utils.neo4j_client import get_neo4j_client
from clients.project_management_client import ProjectManagementClient
import asyncio
from utils.workflow_gating import run_with_lock
from module_utils.asyncio_runner import run_async
from constants import (
    TASK_RUN_GRAPHRAG_JOB,
    QUEUE_NEO4J_INGESTION,
    GATE_NS_INGESTION,
)

@celery_app.task(
    bind=True,
    name=TASK_RUN_GRAPHRAG_JOB,
    soft_time_limit=3600,
    time_limit=0,
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

    # Extract and validate authentication token
    logger.info("Checking task headers", 
                has_headers=self.request.headers is not None,
                headers_type=type(self.request.headers).__name__ if self.request.headers else "None",
                task_id=self.request.id)
    
    if not self.request.headers:
        error_msg = (
            "Task headers are None - Authentication token required. "
            "Ensure task_protocol=2 is set in Celery config and worker is restarted."
        )
        logger.error(error_msg, task_id=self.request.id, project_id=project_id)
        raise RuntimeError(error_msg)
    
    auth_token = self.request.headers.get('Authentication')
    if not auth_token:
        logger.error("Missing Authentication header", 
                    headers=dict(self.request.headers) if self.request.headers else {},
                    task_id=self.request.id,
                    project_id=project_id)
        raise RuntimeError("Missing Authentication header in task")
    
    logger.info("Authentication token found", token_length=len(auth_token), task_id=self.request.id)

    def _enqueue_follow_up(job_id_: str, project_id_: str, attempts_: int) -> None:
        celery_app.send_task(
            TASK_RUN_GRAPHRAG_JOB,
            args=[job_id_, project_id_, int(attempts_)],
            queue=QUEUE_NEO4J_INGESTION,
            headers={"Authentication": auth_token},
        )

    async def _execute() -> Dict[str, Any]:
        return await _run_graphrag_job_async(job_id, project_id, attempts, start, authorization_header=auth_token)

    # Centralized lock/pending/retry handling using persistent event loop
    return run_async(
        run_with_lock(
            namespace=GATE_NS_INGESTION,
            identifier=project_id,
            job_id=job_id,
            attempts=int(attempts or 0),
            execute=_execute,
            schedule_retry=schedule_ingestion_retry,
            enqueue_follow_up=_enqueue_follow_up,
        )
    )


async def _run_graphrag_job_async(
    job_id: str,
    project_id: str,
    attempts: int,
    start_time: float,
    authorization_header: str | None = None,
) -> Dict[str, Any]:
    """Single-event-loop orchestration for the GraphRAG job."""
    # Debug: log the active event loop id to verify persistence across tasks
    try:
        loop = asyncio.get_running_loop()
        logger.info("Persistent event loop active", loop_id=id(loop))
    except Exception:
        pass

    try:
        # Mark project as rag_processing (best-effort)
        try:
            async with ProjectManagementClient() as pm:
                await pm.update_project_status(
                    project_id=project_id,
                    status="rag_processing",
                    authorization_header=authorization_header,
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
            logger.info(
                "Updating project status to rag_ready",
                project_id=project_id,
                processed_count=processed_count,
                total_count=total_count,
                has_auth=bool(authorization_header),
            )
            async with ProjectManagementClient() as pm:
                update_result = await pm.update_project_status(
                    project_id=project_id,
                    processed_count=processed_count,
                    total_count=total_count,
                    status="rag_ready",
                    authorization_header=authorization_header,
                )
                if update_result.success:
                    logger.info(
                        "Successfully updated project status to rag_ready",
                        project_id=project_id,
                        status_code=update_result.status_code,
                    )
                else:
                    logger.error(
                        "Failed to update project status to rag_ready",
                        project_id=project_id,
                        status_code=update_result.status_code,
                        error_message=update_result.error_message,
                    )
        except Exception as e:
            logger.error(
                "Exception while updating project status to rag_ready",
                project_id=project_id,
                error=str(e),
                error_type=type(e).__name__,
                exc_info=True,
            )

        return result
    except Exception as exc:  # noqa: BLE001
        # On failure, publish error status (best-effort)
        try:
            logger.error(
                "GraphRAG job failed, updating project status to rag_failed",
                project_id=project_id,
                error=str(exc),
                error_type=type(exc).__name__,
                has_auth=bool(authorization_header),
            )
            async with ProjectManagementClient() as pm:
                update_result = await pm.update_project_status(
                    project_id=project_id,
                    status="rag_failed",
                    error_message=str(exc),
                    authorization_header=authorization_header,
                )
                if update_result.success:
                    logger.info(
                        "Successfully updated project status to rag_failed",
                        project_id=project_id,
                        status_code=update_result.status_code,
                    )
                else:
                    logger.error(
                        "Failed to update project status to rag_failed",
                        project_id=project_id,
                        status_code=update_result.status_code,
                        error_message=update_result.error_message,
                    )
        except Exception as status_exc:
            logger.error(
                "Exception while updating project status to rag_failed",
                project_id=project_id,
                original_error=str(exc),
                status_update_error=str(status_exc),
                status_update_error_type=type(status_exc).__name__,
                exc_info=True,
            )

        # Compute next attempts and schedule retry or DLQ based on configured policy (shared)
        to_dlq, countdown, next_attempts = compute_retry_decision(attempts or 0)
        if to_dlq:
            await schedule_ingestion_retry(job_id, project_id, next_attempts, True, authorization_header=authorization_header)
        else:
            await schedule_ingestion_retry(job_id, project_id, next_attempts, False, countdown=countdown, authorization_header=authorization_header)
        # Re-raise to surface failure to Celery for visibility
        raise
