"""
Task request subscriber for document processing service.
Handles Redis task requests and triggers Celery tasks.
"""

import structlog
import threading

from utils.redis_abstractions import (
    BaseRedisSubscriber,
    MessageTypeRegistry,
    RedisChannelConfig,
    RedisMode
)
from utils.unified_redis_messages import TaskRequestMessage, create_message_registry

logger = structlog.get_logger(__name__)


class TaskRequestSubscriber(BaseRedisSubscriber):
    """Task request subscriber for document processing using streams mode."""

    def __init__(
        self, 
        redis_client, 
        message_registry: MessageTypeRegistry,
        process_project_documents_task,
        consumer_group: str = "document_processors",
        consumer_name: str = "worker"
    ):
        if process_project_documents_task is None:
            raise ValueError("process_project_documents_task is required")
            
        # Use task_streams prefix for document processing
        channel_config = RedisChannelConfig("task_streams", ":")
        super().__init__(
            redis_client, 
            channel_config, 
            message_registry, 
            mode=RedisMode.STREAMS
        )
        self.consumer_group_name = consumer_group
        self.consumer_name_value = consumer_name
        self.process_project_documents_task = process_project_documents_task
        self._lock = threading.Lock()  # Thread safety for task triggering

    def get_subscription_channels(self) -> list[str]:
        return [self.channel_config.get_channel_name("document_processing")]

    def get_subscription_streams(self) -> list[str]:
        return [self.channel_config.get_channel_name("document_processing")]

    def get_consumer_group(self) -> str:
        return self.consumer_group_name

    def get_consumer_name(self) -> str:
        return self.consumer_name_value

    async def handle_message(self, message: TaskRequestMessage, channel: str) -> bool:
        try:
            if not isinstance(message, TaskRequestMessage):
                logger.error("Received non-TaskRequestMessage", message_type=type(message).__name__)
                return False
            
            logger.info("Processing task request message", 
                       task_type=message.task_type,
                       project_id=str(message.project_id),
                       correlation_id=str(message.correlation_id),
                       message_id=str(message.message_id))
            
            if message.task_type == "process_project_documents":
                task = self.process_project_documents_task
                if task is None:
                    logger.error("Celery task not available")
                    return False
                
                # Validate task configuration
                if not hasattr(task, 'apply_async'):
                    logger.error("Task does not have apply_async method", task_type=type(task).__name__)
                    return False
                
                logger.debug("Task validation passed", 
                           task_name=getattr(task, 'name', 'unknown'),
                           task_type=type(task).__name__)
                
                try:
                    # Use thread-safe approach to trigger Celery task
                    with self._lock:
                        logger.info("Triggering Celery task", 
                                   project_id=str(message.project_id),
                                   task_name=task.name,
                                   thread_id=threading.get_ident())
                        
                        # Use apply_async with explicit queue for thread safety
                        task_result = task.apply_async(
                            args=[str(message.project_id)],
                            queue='document_processing',
                            correlation_id=str(message.correlation_id)
                        )
                        
                        if task_result is None:
                            logger.error("Celery task returned None result", project_id=str(message.project_id))
                            return False

                        logger.info("Celery task triggered successfully", 
                                   project_id=str(message.project_id),
                                   task_id=task_result.id,
                                   correlation_id=str(message.correlation_id))
                        return True
                        
                except Exception as task_error:
                    logger.error("Failed to trigger Celery task", 
                               project_id=str(message.project_id),
                               error=str(task_error),
                               task_name=task.name if task else 'unknown',
                               exc_info=True)
                    return False
            
            logger.warning("Unknown task type received", task_type=message.task_type)
            return False
        except Exception as e:
            logger.error("Failed to handle task request message", 
                        project_id=str(message.project_id) if hasattr(message, 'project_id') else 'unknown',
                        error=str(e),
                        exc_info=True)
            return False


def create_task_subscriber(redis_client, process_project_documents_task) -> TaskRequestSubscriber:
    """Create task subscriber for document processing service."""    
    message_registry = create_message_registry()
    return TaskRequestSubscriber(
        redis_client,
        message_registry,
        process_project_documents_task
    ) 