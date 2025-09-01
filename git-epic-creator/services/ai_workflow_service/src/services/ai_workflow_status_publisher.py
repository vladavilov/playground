from typing import Optional, List
from uuid import UUID

import json

from constants import UI_CHANNEL_PREFIX
from workflow_models.progress_messages import WorkflowProgressMessage


class AiWorkflowStatusPublisher:
    """Minimal Redis pub/sub publisher for UI AI workflow progress."""

    def __init__(self, redis_client):
        self.redis_client = redis_client
        self.prefix = UI_CHANNEL_PREFIX
        # Use local literal to avoid depending on shared package update at runtime
        self.default_name = "ai_workflow_progress"

    def _channel(self, name: str) -> str:
        return f"{self.prefix}:{name}"

    async def _publish_message(self, message: WorkflowProgressMessage, channel: str | None = None) -> bool:
        try:
            target = self._channel(channel or self.default_name)
            await self.redis_client.publish(target, json.dumps(message.model_dump()))
            return True
        except Exception:
            return False

    async def publish_workflow_update(
        self,
        project_id: UUID,
        prompt_id: UUID,
        stage: str,
        status: str,
        thought_summary: str,
        iteration: Optional[int] = None,
        score: Optional[float] = None,
        citations: Optional[List[str]] = None,
        visibility: str = "user",
    ) -> bool:
        message = WorkflowProgressMessage(
            project_id=project_id,
            prompt_id=prompt_id,
            stage=stage,
            status=status,
            thought_summary=thought_summary,
            iteration=iteration,
            score=score,
            citations=citations,
            visibility=visibility,
        )
        return await self._publish_message(message)


