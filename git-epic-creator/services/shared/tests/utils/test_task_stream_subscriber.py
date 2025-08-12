from unittest.mock import AsyncMock, MagicMock

import pytest
from constants import INGESTION_TRIGGER_STREAM

@pytest.mark.asyncio
async def test_process_messages_enqueues_and_xacks():
    from utils.task_stream_subscriber import TaskStreamSubscriber

    redis_client = AsyncMock()
    redis_client.xack = AsyncMock(return_value=1)

    task = MagicMock()
    task.apply_async = MagicMock(return_value=MagicMock(id="task-1"))

    def build_apply_kwargs(fields: dict) -> dict:
        return {
            "args": [fields.get("project_id")],
            "kwargs": {"extra": fields.get("job_id")},
            "queue": "neo4j_ingestion",
            "correlation_id": fields.get("correlation_id"),
        }

    
    subscriber = TaskStreamSubscriber(
        redis_client=redis_client,
        stream_key=INGESTION_TRIGGER_STREAM,
        consumer_group="neo4j_ingestor",
        consumer_name="ingestor-1",
        task=task,
        queue="neo4j_ingestion",
        build_apply_kwargs=build_apply_kwargs,
    )

    messages = [
        (
            INGESTION_TRIGGER_STREAM,
            [
                (
                    "1738359478123-0",
                    {
                        b"job_id": b"job-123",
                        b"project_id": b"proj-1",
                        b"documents": b"[]",
                        b"attempts": b"0",
                        b"correlation_id": b"corr-1",
                    },
                )
            ],
        )
    ]

    result = await subscriber.process_messages(messages)

    assert result.processed == 1
    task.apply_async.assert_called_once()
    kwargs = task.apply_async.call_args.kwargs
    assert kwargs["queue"] == "neo4j_ingestion"
    redis_client.xack.assert_awaited_once()


@pytest.mark.asyncio
async def test_idle_claim_calls_xautoclaim():
    from utils.task_stream_subscriber import TaskStreamSubscriber

    redis_client = AsyncMock()
    redis_client.xautoclaim = AsyncMock(return_value=("0-0", []))

    task = MagicMock()
    task.apply_async = MagicMock()

    from constants import INGESTION_TRIGGER_STREAM
    subscriber = TaskStreamSubscriber(
        redis_client=redis_client,
        stream_key=INGESTION_TRIGGER_STREAM,
        consumer_group="neo4j_ingestor",
        consumer_name="ingestor-1",
        task=task,
        queue="neo4j_ingestion",
    )

    await subscriber.claim_idle_messages()
    redis_client.xautoclaim.assert_awaited_once()


