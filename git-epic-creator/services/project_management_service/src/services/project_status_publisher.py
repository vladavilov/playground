from typing import Optional
from uuid import UUID

from utils.unified_redis_messages import ProjectProgressMessage
from constants import UI_CHANNEL_PREFIX, UI_PROJECT_PROGRESS_NAME
import json


class ProjectStatusPublisher:
    """Minimal Redis pub/sub publisher for UI project progress."""

    def __init__(self, redis_client):
        self.redis_client = redis_client
        self.prefix = UI_CHANNEL_PREFIX
        self.default_name = UI_PROJECT_PROGRESS_NAME

    def _channel(self, name: str) -> str:
        return f"{self.prefix}:{name}"

    async def _publish_message(self, message, channel: str | None = None) -> bool:
        try:
            target = self._channel(channel or self.default_name)
            await self.redis_client.publish(target, json.dumps(message.to_dict()))
            return True
        except Exception:
            return False

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
        return await self._publish_message(message)