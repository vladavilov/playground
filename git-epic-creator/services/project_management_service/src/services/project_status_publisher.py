from typing import Optional
from uuid import UUID

from utils.redis_abstractions import RedisMode
from utils.unified_redis_messages import ProjectProgressMessage
from services.redis_publisher import SimpleRedisPublisher


class ProjectStatusPublisher(SimpleRedisPublisher):
    """Thin wrapper over SimpleRedisPublisher for UI progress."""

    def __init__(self, redis_client):
        super().__init__(redis_client, prefix="ui", default_name="project_progress", mode=RedisMode.PUB_SUB)

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