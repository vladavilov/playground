import pytest
from unittest.mock import AsyncMock, MagicMock


@pytest.mark.asyncio
async def test_ingestion_subscriber_enqueues_and_xacks():
    from subscribers.task_subscriber import create_task_subscriber

    # Arrange mocks
    redis_client = AsyncMock()
    redis_client.xack = AsyncMock(return_value=1)

    task = MagicMock()
    task.apply_async = MagicMock(return_value=MagicMock(id="t-1"))

    subscriber = create_task_subscriber(redis_client, task)

    messages = [
        (
            "ingestion.trigger",
            [
                (
                    "1738359478123-0",
                    {
                        b"job_id": b"job-1",
                        b"project_id": b"proj-1",
                        b"attempts": b"0",
                    },
                )
            ],
        )
    ]

    # Act
    result = await subscriber.process_messages(messages)

    # Assert
    assert result.processed == 1
    task.apply_async.assert_called_once()
    kwargs = task.apply_async.call_args.kwargs
    assert kwargs["queue"] == "neo4j_ingestion"
    assert kwargs["args"] == ["job-1", "proj-1", 0]
    redis_client.xack.assert_awaited_once()


