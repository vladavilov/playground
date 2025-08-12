"""
Trigger publisher for GraphRAG ingestion: publishes minimal fields to Redis Streams.
"""

from __future__ import annotations

import structlog

logger = structlog.get_logger(__name__)


class IngestionJobPublisher:
    """
    Publishes trigger entries to a Redis Stream for GraphRAG ingestion.
    """

    def __init__(self, redis_client, stream_key: str = "ingestion.trigger") -> None:
        self.redis_client = redis_client
        self.stream_key = stream_key

    async def publish(self, job_id: str, project_id: str, attempts: int = 0) -> str:
        """Publish a trigger entry and return the stream entry ID."""
        fields = {
            "job_id": job_id,
            "project_id": project_id,
            "attempts": str(int(attempts) if attempts is not None else 0),
        }

        logger.info(
            "Publishing ingestion trigger",
            stream_key=self.stream_key,
            job_id=fields["job_id"],
            project_id=fields["project_id"],
            attempts=fields["attempts"],
        )

        stream_id = await self.redis_client.xadd(self.stream_key, fields)
        return stream_id


