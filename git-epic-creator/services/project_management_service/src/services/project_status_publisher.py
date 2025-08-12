from typing import Optional
from uuid import UUID

from utils.redis_abstractions import RedisMode
from utils.unified_redis_messages import ProjectProgressMessage
from constants import UI_CHANNEL_PREFIX, UI_PROJECT_PROGRESS_NAME
from services.redis_publisher import SimpleRedisPublisher


class ProjectStatusPublisher(SimpleRedisPublisher):
    """Thin wrapper over SimpleRedisPublisher for UI progress."""

    def __init__(self, redis_client):
        # Build from shared constants to avoid string drift
        super().__init__(
            redis_client,
            prefix=UI_CHANNEL_PREFIX,
            default_name=UI_PROJECT_PROGRESS_NAME,
            mode=RedisMode.PUB_SUB,
        )

    async def publish_project_update(
        self,
        project_id: UUID,
        status: str,
        processed_count: Optional[int] = None,
        total_count: Optional[int] = None,
        processed_pct: Optional[float] = None,
    ) -> bool:
        message = ProjectProgressMessage(
            project_id=project_id,
            status=status,
            processed_count=processed_count,
            total_count=total_count,
            processed_pct=processed_pct,
        )
        return await self.publish_message(message)