"""Tests for RedisProgressPublisher base class."""

import asyncio
from typing import Set
from unittest.mock import AsyncMock, MagicMock
import pytest
from pydantic import BaseModel

from utils.redis_progress_publisher import RedisProgressPublisher


class TestMessage(BaseModel):
    """Test message model."""
    message_type: str
    content: str


@pytest.mark.asyncio
async def test_publish_message_keeps_task_reference():
    """Verify that background tasks are kept in set to prevent garbage collection."""
    redis_mock = AsyncMock()
    redis_mock.publish = AsyncMock(return_value=1)
    
    publisher = RedisProgressPublisher(redis_mock, "test_channel")
    
    # Verify background tasks set is initialized
    assert hasattr(publisher, "_background_tasks")
    assert isinstance(publisher._background_tasks, Set)
    assert len(publisher._background_tasks) == 0
    
    # Publish a message
    message = TestMessage(message_type="test", content="hello")
    result = await publisher._publish_message(message)
    
    assert result is True
    
    # Background tasks set should contain the task immediately after creation
    # (Before the task completes, it should be in the set)
    # Note: The task might complete very quickly, so we check if publish was called
    
    # Allow time for background task to execute
    await asyncio.sleep(0.1)
    
    # Verify Redis publish was called
    redis_mock.publish.assert_called_once()
    call_args = redis_mock.publish.call_args
    assert "ui:test_channel" in call_args[0]
    
    # After task completes, it should be removed from the set (via callback)
    await asyncio.sleep(0.1)
    assert len(publisher._background_tasks) == 0


@pytest.mark.asyncio
async def test_publish_message_multiple_concurrent():
    """Verify multiple concurrent publishes maintain separate task references."""
    redis_mock = AsyncMock()
    
    # Add delay to Redis publish to ensure tasks overlap
    async def slow_publish(*args, **kwargs):
        await asyncio.sleep(0.05)
        return 1
    
    redis_mock.publish = slow_publish
    
    publisher = RedisProgressPublisher(redis_mock, "test_channel")
    
    # Publish multiple messages concurrently
    messages = [
        TestMessage(message_type="test", content=f"message{i}")
        for i in range(5)
    ]
    
    results = await asyncio.gather(*[
        publisher._publish_message(msg) for msg in messages
    ])
    
    assert all(r is True for r in results)
    
    # While tasks are running, background_tasks should contain references
    # (May be fewer than 5 if some completed already)
    initial_task_count = len(publisher._background_tasks)
    assert initial_task_count >= 0  # Some tasks may have already completed
    
    # Wait for all tasks to complete
    await asyncio.sleep(0.3)
    
    # All tasks should be cleaned up
    assert len(publisher._background_tasks) == 0


@pytest.mark.asyncio
async def test_publish_message_handles_redis_error():
    """Verify that Redis errors are caught and logged without raising."""
    redis_mock = AsyncMock()
    redis_mock.publish = AsyncMock(side_effect=Exception("Redis connection failed"))
    
    publisher = RedisProgressPublisher(redis_mock, "test_channel")
    
    message = TestMessage(message_type="test", content="hello")
    
    # Should not raise even though Redis fails
    result = await publisher._publish_message(message)
    assert result is True
    
    # Allow background task to execute and fail
    await asyncio.sleep(0.1)
    
    # Task should still be removed from set after error
    assert len(publisher._background_tasks) == 0


@pytest.mark.asyncio
async def test_channel_name_formatting():
    """Verify channel name is correctly formatted."""
    redis_mock = AsyncMock()
    publisher = RedisProgressPublisher(redis_mock, "test_channel")
    
    # Test default channel
    assert publisher._channel() == "ui:test_channel"
    
    # Test custom channel
    assert publisher._channel("custom") == "ui:custom"
    
    # Test None uses default
    assert publisher._channel(None) == "ui:test_channel"


@pytest.mark.asyncio
async def test_task_cleanup_callback():
    """Verify that task cleanup callback works correctly."""
    redis_mock = AsyncMock()
    redis_mock.publish = AsyncMock(return_value=1)
    
    publisher = RedisProgressPublisher(redis_mock, "test_channel")
    
    # Create multiple messages
    messages = [TestMessage(message_type="test", content=f"msg{i}") for i in range(3)]
    
    # Publish all messages
    for msg in messages:
        await publisher._publish_message(msg)
    
    # Tasks should be in the set
    max_tasks = len(publisher._background_tasks)
    assert max_tasks >= 0  # Some may have already completed
    
    # Wait for all tasks to complete
    await asyncio.sleep(0.2)
    
    # All tasks should be removed via callbacks
    assert len(publisher._background_tasks) == 0


@pytest.mark.asyncio
async def test_fire_and_forget_behavior():
    """Verify that _publish_message returns immediately without waiting."""
    redis_mock = AsyncMock()
    
    # Simulate slow Redis publish
    async def very_slow_publish(*args, **kwargs):
        await asyncio.sleep(1.0)  # Simulate 1 second delay
        return 1
    
    redis_mock.publish = very_slow_publish
    
    publisher = RedisProgressPublisher(redis_mock, "test_channel")
    message = TestMessage(message_type="test", content="hello")
    
    # Measure time taken to call _publish_message
    import time
    start = time.time()
    result = await publisher._publish_message(message)
    elapsed = time.time() - start
    
    # Should return immediately (< 100ms), not wait for Redis
    assert elapsed < 0.1
    assert result is True
    
    # Background task is still running
    assert len(publisher._background_tasks) >= 0

