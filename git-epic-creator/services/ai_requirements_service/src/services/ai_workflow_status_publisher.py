"""AI Requirements workflow status publisher (Redis pub/sub)."""

from typing import Optional
from uuid import UUID

from constants.streams import UI_AI_REQUIREMENTS_PROGRESS_NAME
from models.progress_messages import WorkflowProgressMessage
from utils.redis_progress_publisher import RedisProgressPublisher


class AiWorkflowStatusPublisher(RedisProgressPublisher):
    """Redis pub/sub publisher for AI requirements workflow progress.
    
    Publishes to channel: ui:ai_requirements_progress
    Inherits common publish infrastructure from RedisProgressPublisher.
    """

    def __init__(self, redis_client):
        super().__init__(
            redis_client=redis_client,
            default_channel_name=UI_AI_REQUIREMENTS_PROGRESS_NAME
        )

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
        """Publish a workflow progress update.
        
        Args:
            project_id: Project UUID
            prompt_id: Conversation/prompt UUID
            status: One of WorkflowStatus enum values
            thought_summary: User-visible progress summary
            details_md: Optional markdown details
            iteration: Optional iteration number (>=1)
            score: Optional quality score (0..1)
            
        Returns:
            True if published successfully, False otherwise
        """
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
