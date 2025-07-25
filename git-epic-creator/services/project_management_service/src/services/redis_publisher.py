"""
Redis publisher service for real-time project updates.
Implements Requirements 3.1, 3.2, 3.3, and 5.3.
"""

import json
from typing import Optional
from uuid import UUID
from datetime import datetime
from dataclasses import dataclass
import structlog

from utils.redis_client import get_redis_client

logger = structlog.get_logger(__name__)


@dataclass
class ProjectProgressMessage:
    """
    Data model for project progress messages published to Redis.
    Implements consistent message structure for real-time updates.
    """
    project_id: UUID
    status: str
    processed_count: Optional[int]
    total_count: Optional[int]
    processed_pct: Optional[float]
    timestamp: datetime

    def to_dict(self) -> dict:
        """
        Convert message to dictionary for JSON serialization.
        
        Returns:
            dict: Message data as dictionary
        """
        return {
            "project_id": str(self.project_id),
            "status": self.status,
            "processed_count": self.processed_count,
            "total_count": self.total_count,
            "processed_pct": self.processed_pct,
            "timestamp": self.timestamp.isoformat()
        }


class RedisPublisher:
    """
    Redis publisher service for real-time project updates.
    Handles publishing project progress messages to Redis pub/sub channels.
    Implements Requirements 3.1, 3.2, 3.3, and 5.3.
    """

    def __init__(self):
        """Initialize Redis publisher with connection management."""
        self.redis_client = get_redis_client()
        logger.info("RedisPublisher initialized")

    def _get_channel_name(self, project_id) -> str:
        """
        Generate consistent channel name for project updates.
        Uses configurable channel prefix from Redis settings.
        
        Args:
            project_id: Project ID (UUID or string)
            
        Returns:
            str: Channel name in format {prefix}:{project_id}
        """
        from configuration.common_config import get_app_settings
        settings = get_app_settings()
        prefix = settings.redis.REDIS_PUBSUB_CHANNEL_PREFIX
        return f"{prefix}:{project_id}"

    async def check_connection(self) -> bool:
        """
        Check Redis connection health.
        
        Returns:
            bool: True if connection is healthy, False otherwise
        """
        try:
            await self.redis_client.ping()
            logger.debug("Redis connection check successful")
            return True
        except Exception as e:
            logger.error("Redis connection check failed", error=str(e))
            return False

    async def publish_project_update(self, message: ProjectProgressMessage) -> bool:
        """
        Publish project update message to Redis channel.
        Uses consistent channel naming: project_progress:{project_id}
        Implements Requirements 3.1, 3.2, and 5.3.
        
        Args:
            message: ProjectProgressMessage to publish
            
        Returns:
            bool: True if publishing succeeded, False if failed
        """
        try:
            # Use consistent channel naming convention
            channel = self._get_channel_name(message.project_id)

            # Serialize message to JSON
            message_data = json.dumps(message.to_dict())

            # Publish to Redis
            subscriber_count = await self.redis_client.publish(channel, message_data)

            logger.info(
                "Project update published to Redis",
                project_id=str(message.project_id),
                channel=channel,
                status=message.status,
                subscriber_count=subscriber_count
            )
            
            return True
            
        except Exception as e:
            logger.error(
                "Failed to publish project update to Redis",
                project_id=str(message.project_id),
                error=str(e),
                exc_info=True
            )
            return False