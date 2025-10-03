"""Redis pub/sub progress notifier for long-running tasks."""

from typing import Dict, Any
import json
import structlog
import redis.asyncio as redis

from config import GitLabClientSettings

logger = structlog.get_logger(__name__)


class ProgressNotifier:
    """Publishes progress events to Redis pub/sub."""
    
    def __init__(self, redis_client: redis.Redis, settings: GitLabClientSettings):
        """
        Initialize progress notifier.
        
        Args:
            redis_client: Configured Redis client
            settings: GitLab client settings with pub/sub prefix
        """
        self.redis = redis_client
        self.settings = settings
    
    def _make_channel(self, project_id: str) -> str:
        """
        Create Redis pub/sub channel name for a project.
        
        Args:
            project_id: Project ID
            
        Returns:
            Channel name
        """
        return f"{self.settings.EMBEDDINGS_PUBSUB_PREFIX}{project_id}"
    
    async def publish(self, project_id: str, event: Dict[str, Any]) -> None:
        """
        Publish a progress event to the project's channel.
        
        Args:
            project_id: Project ID
            event: Event data dictionary
        """
        channel = self._make_channel(project_id)
        message = json.dumps(event)
        
        try:
            await self.redis.publish(channel, message)
            
            logger.debug(
                "Progress event published",
                project_id=project_id,
                event_type=event.get("event"),
                channel=channel
            )
        except Exception as e:
            # Don't fail the operation if pub/sub fails
            logger.warning(
                "Failed to publish progress event",
                project_id=project_id,
                error=str(e),
                error_type=type(e).__name__
            )
    
    async def notify_started(self, project_id: str) -> None:
        """Notify that embedding caching has started."""
        await self.publish(project_id, {
            "event": "started",
            "project_id": project_id
        })
    
    async def notify_progress(
        self,
        project_id: str,
        scanned: int,
        total: int
    ) -> None:
        """Notify progress in scanning work items."""
        await self.publish(project_id, {
            "event": "progress",
            "project_id": project_id,
            "scanned": scanned,
            "total": total
        })
    
    async def notify_embedded(
        self,
        project_id: str,
        completed: int,
        total: int
    ) -> None:
        """Notify progress in embedding generation."""
        await self.publish(project_id, {
            "event": "embedded",
            "project_id": project_id,
            "completed": completed,
            "total": total
        })
    
    async def notify_cached(
        self,
        project_id: str,
        cached: int
    ) -> None:
        """Notify that embeddings have been cached."""
        await self.publish(project_id, {
            "event": "cached",
            "project_id": project_id,
            "cached": cached
        })
    
    async def notify_completed(self, project_id: str) -> None:
        """Notify that embedding caching is complete."""
        await self.publish(project_id, {
            "event": "completed",
            "project_id": project_id
        })
    
    async def notify_error(
        self,
        project_id: str,
        message: str
    ) -> None:
        """Notify that an error occurred."""
        await self.publish(project_id, {
            "event": "error",
            "project_id": project_id,
            "message": message
        })


