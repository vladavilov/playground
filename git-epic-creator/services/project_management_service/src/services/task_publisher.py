"""
Task request publisher for project management service.
Handles publishing task requests to Redis for cross-service communication.
"""

import structlog
from uuid import UUID, uuid4

from utils.redis_abstractions import BaseRedisPublisher, RedisChannelConfig, RedisMode
from utils.unified_redis_messages import TaskRequestMessage

logger = structlog.get_logger(__name__)

class TaskRequestPublisher(BaseRedisPublisher):
    """Task request publisher for project management service using streams mode."""

    def __init__(self, redis_client):
        """Initialize task request publisher."""
        # Use task_streams prefix for document processing
        channel_config = RedisChannelConfig("task_streams", ":")
        super().__init__(redis_client, channel_config, mode=RedisMode.STREAMS)

    def get_default_channel(self) -> str:
        """Get default channel name (for pub/sub compatibility)."""
        return self.channel_config.get_channel_name("document_processing")

    def get_default_stream(self) -> str:
        """Get default stream name for task requests."""
        return self.channel_config.get_channel_name("document_processing")

    async def request_document_processing(self, project_id: UUID) -> bool:
        """
        Request document processing for a project.
        
        Args:
            project_id: UUID of the project to process documents for
            
        Returns:
            bool: True if request was published successfully, False otherwise
        """
        try:
            # Create task request message
            message = TaskRequestMessage(
                task_type="process_project_documents",
                project_id=project_id,
                correlation_id=uuid4(),
                parameters={}
            )

            # Publish the request
            result = await self.publish_message(message)

            if result:
                logger.info("Document processing request submitted", project_id=str(project_id))
            else:
                logger.error("Failed to submit document processing request", project_id=str(project_id))

            return result

        except Exception as e:
            logger.error("Exception while requesting document processing", project_id=str(project_id), error=str(e))
            return False 