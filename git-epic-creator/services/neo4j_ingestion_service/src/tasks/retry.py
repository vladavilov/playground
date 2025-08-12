"""
Retry/DLQ scheduling helpers and Celery task for Neo4j ingestion triggers.
"""

from __future__ import annotations

import asyncio
from typing import Optional

import structlog

from worker.celery_app import celery_app
from utils.redis_client import get_redis_client
from constants.streams import (
    INGESTION_TRIGGER_STREAM,
    INGESTION_DLQ_STREAM,
)
from models.ingestion_messages import to_stream_fields_raw


logger = structlog.get_logger(__name__)


async def schedule_ingestion_retry(
    job_id: str,
    project_id: str,
    attempts: int,
    to_dlq: bool = False,
    *,
    stream_key: Optional[str] = None,
) -> str:
    """
    Publish a retry or DLQ entry to Redis Streams.

    Args:
        job_id: Ingestion job id
        project_id: Project id
        attempts: Attempts count to publish with
        to_dlq: If True, publish to DLQ stream; otherwise main stream
        stream_key: Optional override of target stream (main only)

    Returns:
        Redis stream id for the published entry
    """
    redis_client = get_redis_client()
    target_stream = (
        INGESTION_DLQ_STREAM if to_dlq else (stream_key or INGESTION_TRIGGER_STREAM)
    )
    fields = to_stream_fields_raw(
        job_id,
        project_id,
        int(attempts) if attempts is not None else 0,
    )

    logger.info(
        "Publishing ingestion retry",
        stream_key=target_stream,
        job_id=fields["job_id"],
        project_id=fields["project_id"],
        attempts=fields["attempts"],
        to_dlq=to_dlq,
    )
    stream_id = await redis_client.xadd(target_stream, fields)
    return stream_id


@celery_app.task(
    bind=True,
    name="tasks.neo4j_ingestion.schedule_ingestion_retry",
    acks_late=True,
)
def schedule_ingestion_retry_task(
    self,
    job_id: str,
    project_id: str,
    attempts: int,
    to_dlq: bool = False,
) -> str:
    """
    Celery task wrapper that publishes retry/DLQ entries.
    """
    return asyncio.run(schedule_ingestion_retry(job_id, project_id, attempts, to_dlq))
