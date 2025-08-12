"""
DLQ replay utility for ingestion triggers.
"""

from __future__ import annotations

from constants.streams import (
    INGESTION_TRIGGER_STREAM,
    INGESTION_DLQ_STREAM,
)
from models.ingestion_messages import to_stream_fields_raw

async def replay_deadletter(
    redis_client,
    *,
    deadletter_stream: str = INGESTION_DLQ_STREAM,
    target_stream: str = INGESTION_TRIGGER_STREAM,
    count: int = 100,
    reset_attempts: bool = True,
) -> int:
    """
    Read a number of entries from DLQ and re-publish to the main trigger stream.

    Args:
        redis_client: Async redis client
        deadletter_stream: DLQ stream key
        target_stream: Destination stream key
        count: Maximum entries to replay
        reset_attempts: If True, set attempts to "0" when replaying

    Returns:
        Number of entries re-published
    """
    entries = await redis_client.xrange(deadletter_stream, count=count)
    republished = 0
    for _, fields in entries:
        job_id = fields.get("job_id")
        project_id = fields.get("project_id")
        attempts_value = 0 if reset_attempts else int(fields.get("attempts", "0"))
        await redis_client.xadd(
            target_stream,
            to_stream_fields_raw(str(job_id or ""), str(project_id or ""), attempts_value),
        )
        republished += 1
    return republished
