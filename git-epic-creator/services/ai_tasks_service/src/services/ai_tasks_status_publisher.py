"""Minimal Redis pub/sub publisher for UI AI tasks/backlog progress."""

from typing import Optional
from uuid import UUID
import json

from models.progress_messages import BacklogProgressMessage
from constants.streams import UI_CHANNEL_PREFIX, UI_AI_TASKS_PROGRESS_NAME


class AiTasksStatusPublisher:
    """Minimal Redis pub/sub publisher for backlog generation progress.
    
    Publishes to channel: ui:ai_tasks_progress
    """

    def __init__(self, redis_client):
        self.redis_client = redis_client
        self.prefix = UI_CHANNEL_PREFIX
        self.default_name = UI_AI_TASKS_PROGRESS_NAME

    def _channel(self) -> str:
        return f"{self.prefix}:{self.default_name}"

    async def _publish_message(self, message: BacklogProgressMessage) -> bool:
        try:
            target = self._channel()
            await self.redis_client.publish(target, json.dumps(message.model_dump(exclude_none=True)))
            return True
        except Exception:
            return False

    async def publish_backlog_update(
        self,
        project_id: UUID,
        prompt_id: UUID,
        status: str,
        thought_summary: str,
        details_md: Optional[str] = None,
        iteration: Optional[int] = None,
        score: Optional[float] = None,
    ) -> bool:
        """Publish a backlog generation progress update.
        
        Args:
            project_id: Project UUID
            prompt_id: Conversation/prompt UUID
            status: One of BacklogStatus enum values
            thought_summary: User-visible progress summary
            details_md: Optional markdown details
            iteration: Optional iteration number (>=1)
            score: Optional quality score (0..1)
            
        Returns:
            True if published successfully, False otherwise
        """
        message = BacklogProgressMessage(
            project_id=project_id,
            prompt_id=prompt_id,
            status=status,
            thought_summary=thought_summary,
            details_md=details_md,
            iteration=iteration,
            score=score,
        )
        return await self._publish_message(message)


