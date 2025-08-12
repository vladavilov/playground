from unittest.mock import AsyncMock, patch

import pytest
from constants import INGESTION_TRIGGER_STREAM, INGESTION_DLQ_STREAM

@pytest.mark.asyncio
async def test_run_graphrag_job_schedules_retry_with_backoff(monkeypatch):
    # Configure deterministic backoff env
    monkeypatch.setenv("RETRY_MAX_ATTEMPTS", "3")
    monkeypatch.setenv("RETRY_BACKOFF_BASE_SEC", "2")
    monkeypatch.setenv("RETRY_BACKOFF_FACTOR", "2")
    monkeypatch.setenv("RETRY_BACKOFF_MAX_SEC", "60")

    # Import task after env is set
    from tasks import graphrag as graphrag_tasks

    # Make the service raise to trigger retry
    monkeypatch.setattr(
        graphrag_tasks.Neo4jIngestionService,
        "run_graphrag_pipeline",
        lambda self, project_id: (_ for _ in ()).throw(RuntimeError("fail")),
    )

    # Mock the retry task scheduling
    with patch("tasks.retry.schedule_ingestion_retry_task.apply_async") as apply_async_mock:
        # Act
        with pytest.raises(RuntimeError):
            graphrag_tasks.run_graphrag_job("job-1", "proj-1", 0)

        # Assert retry enqueued with attempts+1 and exponential backoff countdown
        assert apply_async_mock.called
        kwargs = apply_async_mock.call_args.kwargs
        assert kwargs["args"] == ("job-1", "proj-1", 1, False)
        # backoff: base * factor^(attempts) = 2 * 2^0 = 2
        assert kwargs.get("countdown") == 2


@pytest.mark.asyncio
async def test_run_graphrag_job_sends_to_dlq_on_max_attempts(monkeypatch):
    monkeypatch.setenv("RETRY_MAX_ATTEMPTS", "3")
    monkeypatch.setenv("RETRY_BACKOFF_BASE_SEC", "1")
    monkeypatch.setenv("RETRY_BACKOFF_FACTOR", "2")
    monkeypatch.setenv("RETRY_BACKOFF_MAX_SEC", "60")

    from tasks import graphrag as graphrag_tasks

    monkeypatch.setattr(
        graphrag_tasks.Neo4jIngestionService,
        "run_graphrag_pipeline",
        lambda self, project_id: (_ for _ in ()).throw(RuntimeError("fail")),
    )

    with patch("tasks.retry.schedule_ingestion_retry_task.apply_async") as apply_async_mock:
        with pytest.raises(RuntimeError):
            # attempts=2, max=3 -> next_attempts=3 => DLQ
            graphrag_tasks.run_graphrag_job("job-2", "proj-2", 2)

        kwargs = apply_async_mock.call_args.kwargs
        assert kwargs["args"] == ("job-2", "proj-2", 3, True)
        # DLQ: should not include countdown for immediate publish
        assert "countdown" not in kwargs


@pytest.mark.asyncio
async def test_schedule_ingestion_retry_publishes_to_correct_stream(monkeypatch):
    # Lazy import to avoid Celery wiring during collection
    from tasks.retry import schedule_ingestion_retry

    mock_redis = AsyncMock()
    # Patch the symbol used inside tasks.retry module
    monkeypatch.setattr("tasks.retry.get_redis_client", lambda: mock_redis)
    mock_redis.xadd = AsyncMock(return_value="1738359478123-0")

    # Publish to main stream
    result_id = await schedule_ingestion_retry("job-3", "proj-3", 2, False)
    assert result_id == "1738359478123-0"
    mock_redis.xadd.assert_awaited()
    
    args, kwargs = mock_redis.xadd.call_args
    assert args[0] == INGESTION_TRIGGER_STREAM
    fields = args[1]
    assert fields["job_id"] == "job-3"
    assert fields["project_id"] == "proj-3"
    assert fields["attempts"] == "2"

    # Publish to DLQ
    mock_redis.xadd.reset_mock(return_value=True, side_effect=True)
    mock_redis.xadd = AsyncMock(return_value="1738359478124-0")
    result_id2 = await schedule_ingestion_retry("job-4", "proj-4", 3, True)
    assert result_id2 == "1738359478124-0"
    args2, _ = mock_redis.xadd.call_args
    assert args2[0] == INGESTION_DLQ_STREAM


@pytest.mark.asyncio
async def test_replay_deadletter_republishes_with_reset_attempts(monkeypatch):
    from tools.replay_deadletter import replay_deadletter

    mock_redis = AsyncMock()
    # Simulate two DLQ entries
    mock_redis.xrange = AsyncMock(
        return_value=[
            ("1738-0", {"job_id": "job-a", "project_id": "proj-a", "attempts": "5"}),
            ("1738-1", {"job_id": "job-b", "project_id": "proj-b", "attempts": "7"}),
        ]
    )
    mock_redis.xadd = AsyncMock(return_value="ok")

    count = await replay_deadletter(mock_redis, count=2, reset_attempts=True)
    assert count == 2
    # Both re-published to main stream with attempts reset to 0
    assert mock_redis.xadd.await_count == 2
    for call in mock_redis.xadd.await_args_list:
        assert call.args[0] == INGESTION_TRIGGER_STREAM
        assert call.args[1]["attempts"] == "0"
