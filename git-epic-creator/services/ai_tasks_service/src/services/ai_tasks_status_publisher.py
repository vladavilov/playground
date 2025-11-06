"""AI Tasks/Backlog status publisher (Redis pub/sub)."""

from typing import Optional
from uuid import UUID

from constants.streams import UI_AI_TASKS_PROGRESS_NAME
from models.progress_messages import BacklogProgressMessage
from utils.redis_progress_publisher import RedisProgressPublisher


class AiTasksStatusPublisher(RedisProgressPublisher):
    """Redis pub/sub publisher for backlog generation progress.
    
    Publishes to channel: ui:ai_tasks_progress
    Inherits common publish infrastructure from RedisProgressPublisher.
    """

    def __init__(self, redis_client):
        super().__init__(
            redis_client=redis_client,
            default_channel_name=UI_AI_TASKS_PROGRESS_NAME
        )

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

    async def publish_enhancement_progress(
        self,
        project_id: UUID,
        item_id: str,
        status: str,
        thought_summary: str,
        details_md: Optional[str] = None,
    ) -> bool:
        """Publish a single-item enhancement progress update.
        
        Args:
            project_id: Project UUID
            item_id: Epic or task identifier
            status: One of: "analyzing_item", "retrieving_context", "enhancing_item", "completed", "error"
            thought_summary: User-visible progress summary
            details_md: Optional markdown details
            
        Returns:
            True if published successfully, False otherwise
        """
        message = BacklogProgressMessage(
            project_id=project_id,
            prompt_id=None,  # No prompt_id for single-item enhancement
            status=status,
            thought_summary=thought_summary,
            details_md=details_md,
            item_id=item_id,
            item_type="task",  # Generic - can be epic or task
            enhancement_mode=True,
        )
        return await self._publish_message(message)