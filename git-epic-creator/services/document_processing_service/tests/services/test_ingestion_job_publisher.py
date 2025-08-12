import asyncio
from unittest.mock import AsyncMock
from uuid import uuid4

from services.ingestion_job_publisher import IngestionJobPublisher


def test_publisher_builds_stream_entry():
    redis_client = AsyncMock()
    redis_client.xadd = AsyncMock(return_value="1738359478123-0")

    publisher = IngestionJobPublisher(redis_client)

    stream_id = asyncio.get_event_loop().run_until_complete(
        publisher.publish(job_id="job-1", project_id=str(uuid4()), attempts=0)
    )

    assert stream_id == "1738359478123-0"
    redis_client.xadd.assert_awaited_once()
    args, kwargs = redis_client.xadd.call_args
    assert args[0] == "ingestion.trigger"
    fields = args[1]
    assert fields["job_id"] == "job-1"
    assert "project_id" in fields
    assert fields["attempts"] == "0"


