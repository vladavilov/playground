"""Retrieval service status publisher (Redis pub/sub)."""

from typing import Optional
from uuid import UUID

from constants.streams import UI_RETRIEVAL_PROGRESS_NAME
from models.progress_messages import RetrievalProgressMessage, RetrievalStatus
from utils.redis_progress_publisher import RedisProgressPublisher


class RetrievalStatusPublisher(RedisProgressPublisher):
    """Redis pub/sub publisher for retrieval (DRIFT search) progress.
    
    Publishes to channel: ui:retrieval_progress
    Inherits common publish infrastructure from RedisProgressPublisher.
    """

    def __init__(self, redis_client):
        super().__init__(
            redis_client=redis_client,
            default_channel_name=UI_RETRIEVAL_PROGRESS_NAME
        )

    async def publish_retrieval_update(
        self,
        project_id: UUID,
        retrieval_id: UUID,
        phase: RetrievalStatus,
        thought_summary: str,
        details_md: Optional[str] = None,
        progress_pct: Optional[float] = None,
    ) -> bool:
        """Publish a retrieval progress update.
        
        Args:
            project_id: Project UUID
            retrieval_id: Unique retrieval session UUID
            phase: Current retrieval phase (RetrievalStatus enum)
            thought_summary: User-visible progress summary
            details_md: Optional markdown details
            progress_pct: Optional progress percentage (0-100)
            
        Returns:
            True if published successfully, False otherwise
        """
        message = RetrievalProgressMessage(
            project_id=project_id,
            retrieval_id=retrieval_id,
            phase=phase,
            thought_summary=thought_summary,
            details_md=details_md,
            progress_pct=progress_pct,
        )
        return await self._publish_message(message)

