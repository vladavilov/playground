"""Generic Redis pub/sub publisher base class for progress messages."""

from typing import Any, Optional, Set
import json
import asyncio
from pydantic import BaseModel
import structlog

from constants.streams import UI_CHANNEL_PREFIX

logger = structlog.get_logger(__name__)


class RedisProgressPublisher:
    """Base class for Redis pub/sub publishers.
    
    Provides common infrastructure for publishing Pydantic progress messages
    to Redis channels. Service-specific publishers should inherit this class
    and implement their domain-specific publish methods.
    """

    def __init__(self, redis_client: Any, default_channel_name: str):
        """Initialize publisher with Redis client and default channel.
        
        Args:
            redis_client: Async Redis client (from shared redis_client.py)
            default_channel_name: Default channel suffix (e.g., "ai_requirements_progress")
        """
        self.redis_client = redis_client
        self.prefix = UI_CHANNEL_PREFIX
        self.default_channel_name = default_channel_name
        # Keep strong references to background tasks to prevent garbage collection
        self._background_tasks: Set[asyncio.Task] = set()

    def _channel(self, name: Optional[str] = None) -> str:
        """Build full channel name from prefix and name.
        
        Args:
            name: Channel name suffix (uses default if None)
            
        Returns:
            Full channel name (e.g., "ui:ai_requirements_progress")
        """
        channel_name = name or self.default_channel_name
        return f"{self.prefix}:{channel_name}"

    async def _publish_message(
        self, 
        message: BaseModel, 
        channel: Optional[str] = None
    ) -> bool:
        """Publish Pydantic message to Redis channel (fire-and-forget).
        
        Uses background task to avoid blocking workflow on Redis I/O.
        Errors are logged but don't affect workflow execution.
        
        Maintains strong references to tasks to prevent garbage collection.
        Completed tasks are automatically cleaned up via callback.
        
        Args:
            message: Pydantic model instance to publish
            channel: Optional channel name override (uses default if None)
            
        Returns:
            True (task created; actual publish happens in background)
        """
        async def _background_publish() -> None:
            try:
                target = self._channel(channel)
                serialized = json.dumps(message.model_dump(exclude_none=True))
                await self.redis_client.publish(target, serialized)
                logger.debug(
                    "progress_message_published",
                    channel=target,
                    message_type=message.model_dump().get("message_type"),
                )
            except Exception as exc:
                logger.error(
                    "progress_message_publish_failed",
                    channel=self._channel(channel),
                    error=str(exc),
                    error_type=type(exc).__name__,
                )
        
        # Create task and keep strong reference to prevent garbage collection
        task = asyncio.create_task(_background_publish())
        self._background_tasks.add(task)
        
        # Remove task from set when done (prevents memory leak)
        task.add_done_callback(self._background_tasks.discard)
        
        return True

