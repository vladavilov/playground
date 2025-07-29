"""
Task request publisher service for cross-service communication.
Now uses unified Redis abstractions from shared utilities.
"""

from utils.unified_redis_messages import (
    TaskRequestMessage,
    TaskRequestPublisher as UnifiedTaskRequestPublisher,
    RedisMessageConfig
)
from utils.redis_client import get_redis_client

# Re-export for backward compatibility
TaskRequestMessage = TaskRequestMessage


class TaskRequestPublisher(UnifiedTaskRequestPublisher):
    """
    Task request publisher with backward compatibility.
    Extends the unified TaskRequestPublisher with same interface as before.
    """

    def __init__(self):
        """Initialize Redis publisher with connection management."""
        redis_client = get_redis_client()
        config = RedisMessageConfig()
        super().__init__(redis_client, config) 