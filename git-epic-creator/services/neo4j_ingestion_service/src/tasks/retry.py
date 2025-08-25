"""
Retry/DLQ scheduling helpers and Celery task for Neo4j ingestion triggers via Celery broker.
"""

from __future__ import annotations

from utils.redis_client import get_redis_client, coalesce_retry
from typing import Optional

import structlog

from worker.celery_app import celery_app
from constants import (
    TASK_RUN_GRAPHRAG_JOB,
    QUEUE_NEO4J_INGESTION,
    QUEUE_NEO4J_INGESTION_DLQ,
)


logger = structlog.get_logger(__name__)


async def schedule_ingestion_retry(
    job_id: str,
    project_id: str,
    attempts: int,
    to_dlq: bool = False,
    *,
    countdown: Optional[int] = None,
) -> str:
    """
    Schedule a retry or DLQ of the ingestion job by publishing directly to the Celery broker.

    Args:
        job_id: Ingestion job id
        project_id: Project id
        attempts: Attempts count to publish with
        to_dlq: If True, route to a DLQ queue; otherwise main queue
        countdown: Optional delay before retry execution (seconds)

    Returns:
        Celery task id for the scheduled retry
    """
    queue_name = QUEUE_NEO4J_INGESTION_DLQ if to_dlq else QUEUE_NEO4J_INGESTION
    # Duplicate suppression: coalesce retries for the same project briefly
    # We rely on job_id+project_id uniqueness but coalesce per project to reduce thundering herds
    try:
        redis_client = get_redis_client()
        proceed = await coalesce_retry(redis_client, project_id, ttl_seconds=5)
        if not proceed:
            logger.info("Coalesced retry - skipping enqueue", project_id=project_id)
            return "coalesced"
    except Exception:
        # If coalescing fails, fall through and enqueue normally
        pass
    logger.info(
        "Publishing ingestion retry via Celery",
        queue=queue_name,
        job_id=job_id,
        project_id=project_id,
        attempts=int(attempts) if attempts is not None else 0,
        to_dlq=to_dlq,
        countdown=countdown,
    )
    async_result = celery_app.send_task(
        TASK_RUN_GRAPHRAG_JOB,
        args=[job_id, project_id, int(attempts) if attempts is not None else 0],
        queue=queue_name,
        countdown=countdown,
    )
    return async_result.id
