from typing import Optional
from uuid import UUID

import json

from constants import UI_CHANNEL_PREFIX
from constants.streams import UI_AI_REQUIREMENTS_PROGRESS_NAME
from models.progress_messages import WorkflowProgressMessage


class AiWorkflowStatusPublisher:
    """Minimal Redis pub/sub publisher for UI AI workflow progress."""

    def __init__(self, redis_client):
        self.redis_client = redis_client
        self.prefix = UI_CHANNEL_PREFIX
        # Use shared constant for the default progress stream name
        self.default_name = UI_AI_REQUIREMENTS_PROGRESS_NAME

    def _channel(self, name: str) -> str:
        return f"{self.prefix}:{name}"

    async def _publish_message(self, message: WorkflowProgressMessage, channel: str | None = None) -> bool:
        try:
            target = self._channel(channel or self.default_name)
            await self.redis_client.publish(target, json.dumps(message.model_dump(exclude_none=True)))
            return True
        except Exception:
            return False

    async def publish_workflow_update(
        self,
        project_id: UUID,
        prompt_id: UUID,
        status: str,
        thought_summary: str,
        details_md: Optional[str] = None,
        iteration: Optional[int] = None,
        score: Optional[float] = None,
    ) -> bool:
        message = WorkflowProgressMessage(
            project_id=project_id,
            prompt_id=prompt_id,
            status=status,
            thought_summary=thought_summary,
            details_md=details_md,
            iteration=iteration,
            score=score,
        )
        return await self._publish_message(message)


