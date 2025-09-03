"""
Tests for AiWorkflowStatusPublisher service.
Mirrors the Redis-based project status publisher tests to ensure parity.
"""

from uuid import uuid4
from unittest.mock import AsyncMock

import json
import pytest


class TestAiWorkflowStatusPublisher:
    """Test AiWorkflowStatusPublisher implementation."""

    def test_publisher_creation_and_introspection(self):
        """Publisher exposes prefix/default_name/_channel consistent with pattern."""
        mock_redis_client = AsyncMock()
        # Import within test to avoid import errors before implementation exists
        from services.ai_workflow_status_publisher import AiWorkflowStatusPublisher  # type: ignore

        publisher = AiWorkflowStatusPublisher(mock_redis_client)

        assert publisher is not None
        assert hasattr(publisher, "prefix")
        assert hasattr(publisher, "default_name")
        assert hasattr(publisher, "_channel")

    def test_default_channel_name(self):
        """Default channel resolves to ui:ai_workflow_progress."""
        mock_redis_client = AsyncMock()
        from services.ai_workflow_status_publisher import AiWorkflowStatusPublisher  # type: ignore

        publisher = AiWorkflowStatusPublisher(mock_redis_client)
        channel = publisher._channel(publisher.default_name)
        assert channel == "ui:ai_workflow_progress"

    @pytest.mark.asyncio
    async def test_publish_workflow_update_success(self):
        """Successful publish returns True and calls redis.publish once."""
        mock_redis_client = AsyncMock()
        mock_redis_client.publish.return_value = 1
        from services.ai_workflow_status_publisher import AiWorkflowStatusPublisher  # type: ignore

        publisher = AiWorkflowStatusPublisher(mock_redis_client)
        project_id = uuid4()
        prompt_id = uuid4()

        ok = await publisher.publish_workflow_update(
            project_id=project_id,
            prompt_id=prompt_id,
            status="evaluating",
            thought_summary="Scoring draft against retrieved context; gaps in completeness.",
            score=0.62,
        )

        assert ok is True
        mock_redis_client.publish.assert_called_once()
        args, kwargs = mock_redis_client.publish.call_args
        assert args[0] == "ui:ai_workflow_progress"
        # Ensure payload is JSON serializable and has required fields
        payload = json.loads(args[1])
        assert payload["message_type"] == "ai_workflow_progress"
        assert payload["status"] == "evaluating"
        assert "project_id" in payload and isinstance(payload["project_id"], str)
        assert "message_id" in payload and isinstance(payload["message_id"], str)
        assert "prompt_id" in payload and isinstance(payload["prompt_id"], str)
        assert "timestamp" in payload and isinstance(payload["timestamp"], str)

    @pytest.mark.asyncio
    async def test_publish_workflow_update_failure(self):
        """Failure to publish returns False."""
        mock_redis_client = AsyncMock()
        mock_redis_client.publish.side_effect = Exception("Redis error")
        from services.ai_workflow_status_publisher import AiWorkflowStatusPublisher  # type: ignore

        publisher = AiWorkflowStatusPublisher(mock_redis_client)
        project_id = uuid4()
        prompt_id = uuid4()

        ok = await publisher.publish_workflow_update(
            project_id=project_id,
            prompt_id=prompt_id,
            status="evaluating",
            thought_summary="Testing failure path",
        )

        assert ok is False
        mock_redis_client.publish.assert_called_once()


