"""
Project status publisher for project management service.
Handles publishing project status updates to Redis for real-time UI updates.
"""

import structlog
from uuid import UUID
from typing import Optional

from utils.redis_abstractions import BaseRedisPublisher, RedisChannelConfig, RedisMode
from utils.unified_redis_messages import ProjectProgressMessage

logger = structlog.get_logger(__name__)


class ProjectStatusPublisher(BaseRedisPublisher):
    """Project status publisher for project management service using pub/sub mode."""

    def __init__(self, redis_client):
        """Initialize project status publisher."""
        # Use ui prefix for project progress updates
        channel_config = RedisChannelConfig("ui", ":")
        super().__init__(redis_client, channel_config, mode=RedisMode.PUB_SUB)

    def get_default_channel(self) -> str:
        """Get default channel name for project progress updates."""
        return self.channel_config.get_channel_name("project_progress")

    def get_default_stream(self) -> str:
        """Get default stream name (for streams compatibility)."""
        return self.channel_config.get_channel_name("project_progress")

    async def publish_project_update(
        self,
        project_id: UUID,
        status: str,
        processed_count: Optional[int] = None,
        total_count: Optional[int] = None,
        processed_pct: Optional[float] = None
    ) -> bool:
        """
        Publish project status update.
        
        Args:
            project_id: UUID of the project
            status: Current status of the project
            processed_count: Number of processed items
            total_count: Total number of items
            processed_pct: Percentage of completion
            
        Returns:
            bool: True if update was published successfully, False otherwise
        """
        try:
            message = ProjectProgressMessage(
                project_id=project_id,
                status=status,
                processed_count=processed_count,
                total_count=total_count,
                processed_pct=processed_pct
            )

            result = await self.publish_message(message)

            if result:
                logger.info("Project status update published", 
                           project_id=str(project_id), 
                           status=status,
                           processed_count=processed_count,
                           total_count=total_count,
                           processed_pct=processed_pct)
            else:
                logger.error("Failed to publish project status update", 
                           project_id=str(project_id), 
                           status=status)

            return result

        except Exception as e:
            logger.error("Exception while publishing project status update", 
                        project_id=str(project_id), 
                        error=str(e))
            return False 