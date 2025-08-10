from uuid import UUID, uuid4

from utils.redis_abstractions import RedisMode
from utils.unified_redis_messages import TaskRequestMessage
from services.redis_publisher import SimpleRedisPublisher


class TaskRequestPublisher(SimpleRedisPublisher):
    """Thin wrapper over SimpleRedisPublisher for task requests."""

    def __init__(self, redis_client):
        super().__init__(redis_client, prefix="task_streams", default_name="document_processing", mode=RedisMode.STREAMS)

    async def request_document_processing(self, project_id: UUID) -> bool:
        message = TaskRequestMessage(
            task_type="process_project_documents",
            project_id=project_id,
            correlation_id=uuid4(),
            parameters={},
        )
        return await self.publish_message(message)