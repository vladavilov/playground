"""
Unified Redis message classes that replace duplicated message types.
Implements TaskRequestMessage, ProjectProgressMessage, and their publishers using base classes.
"""

from dataclasses import dataclass
from typing import Dict, Any, Optional
from uuid import UUID, uuid4
from datetime import datetime
import structlog

from utils.redis_abstractions import (
    RedisMessage,
    BaseRedisPublisher,
    BaseRedisSubscriber,
    MessageTypeRegistry,
    RedisChannelConfig
)

logger = structlog.get_logger(__name__)


@dataclass
class TaskRequestMessage(RedisMessage):
    """
    Unified task request message that replaces duplicate TaskRequestMessage classes.
    Used for cross-service task communication via Redis pub/sub.
    """
    task_type: str
    project_id: UUID
    correlation_id: UUID
    parameters: Dict[str, Any]
    message_type: str = "task_request"

    def __init__(
        self,
        task_type: str,
        project_id: UUID,
        correlation_id: UUID,
        parameters: Dict[str, Any],
        timestamp: Optional[datetime] = None,
        message_id: Optional[UUID] = None,
        message_type: str = "task_request"
    ):
        """
        Initialize TaskRequestMessage with backward compatibility.
        
        Args:
            task_type: Type of task to be processed
            project_id: Project UUID
            correlation_id: Correlation UUID for tracking
            parameters: Task parameters dictionary
            timestamp: Message timestamp (optional, defaults to now)
            message_id: Message ID (optional, defaults to new UUID)
            message_type: Message type identifier
        """
        from uuid import uuid4
        from datetime import datetime as dt
        
        # Use provided values or generate defaults
        actual_timestamp = timestamp or dt.now()
        actual_message_id = message_id or uuid4()
        
        # Initialize base class
        super().__init__(
            message_id=actual_message_id,
            timestamp=actual_timestamp
        )
        
        # Set task-specific fields
        self.task_type = task_type
        self.project_id = project_id
        self.correlation_id = correlation_id
        self.parameters = parameters
        self.message_type = message_type

    def to_dict(self) -> Dict[str, Any]:
        """
        Convert message to dictionary for JSON serialization.
        Extends base class to handle UUID fields.
        
        Returns:
            Dict[str, Any]: Message data as dictionary
        """
        # Get base message fields
        data = super().to_dict()
        
        # Add task-specific fields with UUID conversion
        data.update({
            "message_type": self.message_type,
            "task_type": self.task_type,
            "project_id": str(self.project_id),
            "correlation_id": str(self.correlation_id),
            "parameters": self.parameters
        })
        
        return data

    @classmethod
    def from_dict(cls, data: Dict[str, Any]) -> 'TaskRequestMessage':
        """
        Create TaskRequestMessage from dictionary.
        Handles UUID conversion for task-specific fields.
        
        Args:
            data: Dictionary containing message data
            
        Returns:
            TaskRequestMessage: Parsed message
            
        Raises:
            ValueError: If data is invalid or UUIDs are malformed
        """
        try:
            # Convert string fields back to appropriate types
            if "message_id" in data:
                data["message_id"] = UUID(data["message_id"])
            if "timestamp" in data:
                data["timestamp"] = datetime.fromisoformat(data["timestamp"])
            if "project_id" in data:
                data["project_id"] = UUID(data["project_id"])
            if "correlation_id" in data:
                data["correlation_id"] = UUID(data["correlation_id"])
                
            return cls(**data)
        except (KeyError, ValueError, TypeError) as e:
            raise ValueError(f"Invalid TaskRequestMessage format: {e}")


@dataclass
class ProjectProgressMessage(RedisMessage):
    """
    Unified project progress message that replaces duplicate ProjectProgressMessage classes.
    Used for real-time project status updates via Redis pub/sub.
    """
    project_id: UUID
    status: str
    processed_count: Optional[int] = None
    total_count: Optional[int] = None
    processed_pct: Optional[float] = None
    message_type: str = "project_progress"

    def __init__(
        self,
        project_id: UUID,
        status: str,
        processed_count: Optional[int] = None,
        total_count: Optional[int] = None,
        processed_pct: Optional[float] = None,
        timestamp: Optional[datetime] = None,
        message_id: Optional[UUID] = None,
        message_type: str = "project_progress"
    ):
        """
        Initialize ProjectProgressMessage with backward compatibility.
        
        Args:
            project_id: Project UUID
            status: Current status of the project
            processed_count: Number of items processed (optional)
            total_count: Total number of items (optional) 
            processed_pct: Percentage processed (optional)
            timestamp: Message timestamp (optional, defaults to now)
            message_id: Message ID (optional, defaults to new UUID)
            message_type: Message type identifier
        """
        from uuid import uuid4
        from datetime import datetime as dt
        
        # Use provided values or generate defaults
        actual_timestamp = timestamp or dt.now()
        actual_message_id = message_id or uuid4()
        
        # Initialize base class
        super().__init__(
            message_id=actual_message_id,
            timestamp=actual_timestamp
        )
        
        # Set project-specific fields
        self.project_id = project_id
        self.status = status
        self.processed_count = processed_count
        self.total_count = total_count
        self.processed_pct = processed_pct
        self.message_type = message_type

    def to_dict(self) -> Dict[str, Any]:
        """
        Convert message to dictionary for JSON serialization.
        Extends base class to handle UUID and optional fields.
        
        Returns:
            Dict[str, Any]: Message data as dictionary
        """
        # Get base message fields
        data = super().to_dict()
        
        # Add progress-specific fields with UUID conversion
        data.update({
            "message_type": self.message_type,
            "project_id": str(self.project_id),
            "status": self.status,
            "processed_count": self.processed_count,
            "total_count": self.total_count,
            "processed_pct": self.processed_pct
        })
        
        return data

    @classmethod
    def from_dict(cls, data: Dict[str, Any]) -> 'ProjectProgressMessage':
        """
        Create ProjectProgressMessage from dictionary.
        Handles UUID conversion for progress-specific fields.
        
        Args:
            data: Dictionary containing message data
            
        Returns:
            ProjectProgressMessage: Parsed message
            
        Raises:
            ValueError: If data is invalid or UUIDs are malformed
        """
        try:
            # Convert string fields back to appropriate types
            if "message_id" in data:
                data["message_id"] = UUID(data["message_id"])
            if "timestamp" in data:
                data["timestamp"] = datetime.fromisoformat(data["timestamp"])
            if "project_id" in data:
                data["project_id"] = UUID(data["project_id"])
                
            return cls(**data)
        except (KeyError, ValueError, TypeError) as e:
            raise ValueError(f"Invalid ProjectProgressMessage format: {e}")


class RedisMessageConfig:
    """
    Configuration class for unified Redis message routing.
    Provides consistent channel naming strategies for different message types.
    """

    def __init__(
        self, 
        task_request_channel_prefix: str = "task_requests",
        project_progress_channel_prefix: str = "project_progress",
        channel_separator: str = ":"
    ):
        """
        Initialize Redis message configuration.
        
        Args:
            task_request_channel_prefix: Prefix for task request channels
            project_progress_channel_prefix: Prefix for progress update channels
            channel_separator: Separator between prefix and channel name
        """
        self.task_request_channel_prefix = task_request_channel_prefix
        self.project_progress_channel_prefix = project_progress_channel_prefix
        self.channel_separator = channel_separator

    def get_task_request_channel(self, task_type: str = "document_processing") -> str:
        """
        Generate task request channel name.
        
        Args:
            task_type: Type of task for channel naming
            
        Returns:
            str: Full task request channel name
        """
        return f"{self.task_request_channel_prefix}{self.channel_separator}{task_type}"

    def get_project_progress_channel(self, project_id: UUID) -> str:
        """
        Generate project progress channel name.
        
        Args:
            project_id: Project UUID for channel naming
            
        Returns:
            str: Full project progress channel name
        """
        return f"{self.project_progress_channel_prefix}{self.channel_separator}{project_id}"


class TaskRequestPublisher(BaseRedisPublisher):
    """
    Unified task request publisher that replaces duplicate TaskRequestPublisher classes.
    Uses BaseRedisPublisher for common functionality.
    """

    def __init__(self, redis_client, config: RedisMessageConfig):
        """
        Initialize task request publisher.
        
        Args:
            redis_client: Redis client instance
            config: Redis message configuration
        """
        # Create channel config for base class
        channel_config = RedisChannelConfig(
            channel_prefix=config.task_request_channel_prefix,
            channel_separator=config.channel_separator
        )
        
        super().__init__(redis_client, channel_config)
        self.config = config
        logger.info("TaskRequestPublisher initialized")

    def get_default_channel(self) -> str:
        """
        Get the default channel name for task requests.
        
        Returns:
            str: Default task request channel name
        """
        return self.config.get_task_request_channel()

    async def request_document_processing(self, project_id: UUID) -> bool:
        """
        Request document processing for a project.
        Creates and publishes a task request message.
        
        Args:
            project_id: UUID of the project to process documents for
            
        Returns:
            bool: True if request was published successfully, False otherwise
        """
        try:
            # Create task request message
            message = TaskRequestMessage(
                message_id=uuid4(),
                timestamp=datetime.now(),
                task_type="process_project_documents",
                project_id=project_id,
                correlation_id=uuid4(),
                parameters={}
            )

            # Publish the request using base class functionality
            result = await self.publish_message(message)

            if result:
                logger.info(
                    "Document processing request submitted successfully",
                    project_id=str(project_id),
                    correlation_id=str(message.correlation_id)
                )
            else:
                logger.error(
                    "Failed to submit document processing request",
                    project_id=str(project_id)
                )

            return result

        except Exception as e:
            logger.error(
                "Exception while requesting document processing",
                project_id=str(project_id),
                error=str(e),
                exc_info=True
            )
            return False


class ProjectProgressPublisher(BaseRedisPublisher):
    """
    Unified project progress publisher that replaces duplicate RedisPublisher classes.
    Uses BaseRedisPublisher for common functionality.
    """

    def __init__(self, redis_client, config: RedisMessageConfig):
        """
        Initialize project progress publisher.
        
        Args:
            redis_client: Redis client instance
            config: Redis message configuration
        """
        # Create channel config for base class
        channel_config = RedisChannelConfig(
            channel_prefix=config.project_progress_channel_prefix,
            channel_separator=config.channel_separator
        )
        
        super().__init__(redis_client, channel_config)
        self.config = config
        logger.info("ProjectProgressPublisher initialized")

    def get_default_channel(self) -> str:
        """
        Get the default channel name for project progress updates.
        Note: This should be overridden per project, but provides a fallback.
        
        Returns:
            str: Default project progress channel name
        """
        return f"{self.config.project_progress_channel_prefix}{self.config.channel_separator}default"

    async def publish_project_update(
        self,
        project_id: UUID,
        status: str,
        processed_count: Optional[int] = None,
        total_count: Optional[int] = None,
        processed_pct: Optional[float] = None
    ) -> bool:
        """
        Publish project progress update.
        Creates and publishes a project progress message.
        
        Args:
            project_id: UUID of the project
            status: Current status of the project
            processed_count: Number of items processed (optional)
            total_count: Total number of items (optional)
            processed_pct: Percentage processed (optional)
            
        Returns:
            bool: True if update was published successfully, False otherwise
        """
        try:
            # Create project progress message
            message = ProjectProgressMessage(
                message_id=uuid4(),
                timestamp=datetime.now(),
                project_id=project_id,
                status=status,
                processed_count=processed_count,
                total_count=total_count,
                processed_pct=processed_pct
            )

            # Get project-specific channel
            channel = self.config.get_project_progress_channel(project_id)

            # Publish the update using base class functionality
            result = await self.publish_message(message, channel)

            if result:
                logger.info(
                    "Project progress update published successfully",
                    project_id=str(project_id),
                    status=status,
                    channel=channel
                )
            else:
                logger.error(
                    "Failed to publish project progress update",
                    project_id=str(project_id),
                    status=status
                )

            return result

        except Exception as e:
            logger.error(
                "Exception while publishing project progress update",
                project_id=str(project_id),
                status=status,
                error=str(e),
                exc_info=True
            )
            return False 


class TaskRequestSubscriber(BaseRedisSubscriber):
    """
    Unified task request subscriber that replaces duplicate TaskRequestSubscriber classes.
    Uses BaseRedisSubscriber for common functionality with message type registry support.
    """

    def __init__(
        self, 
        redis_client, 
        config: RedisMessageConfig,
        message_registry: MessageTypeRegistry,
        process_project_documents_task=None
    ):
        """
        Initialize task request subscriber.
        
        Args:
            redis_client: Redis client instance
            config: Redis message configuration
            message_registry: Registry for message type routing
            process_project_documents_task: Celery task for processing (optional)
        """
        # Create channel config for base class
        channel_config = RedisChannelConfig(
            channel_prefix=config.task_request_channel_prefix,
            channel_separator=config.channel_separator
        )
        
        super().__init__(redis_client, channel_config, message_registry)
        self.config = config
        
        # Handle Celery task dependency injection
        if process_project_documents_task:
            self.process_project_documents_task = process_project_documents_task
        else:
            self._process_project_documents_task = None  # Will be lazily loaded
        
        logger.info("TaskRequestSubscriber initialized")

    @property
    def process_project_documents_task(self):
        """Lazy load the Celery task to avoid circular imports."""
        if hasattr(self, '_process_project_documents_task') and self._process_project_documents_task is None:
            from tasks.document_tasks import process_project_documents_task
            self._process_project_documents_task = process_project_documents_task
        return getattr(self, '_process_project_documents_task', None)
    
    @process_project_documents_task.setter
    def process_project_documents_task(self, value):
        """Set the Celery task explicitly."""
        self._process_project_documents_task = value

    def get_subscription_channels(self) -> list[str]:
        """
        Get list of channels to subscribe to.
        
        Returns:
            list[str]: Channel names to subscribe to
        """
        return [self.config.get_task_request_channel()]

    async def handle_message(self, message: RedisMessage, channel: str) -> bool:
        """
        Handle received task request message.
        
        Args:
            message: Deserialized task request message
            channel: Channel the message was received on
            
        Returns:
            bool: True if message was handled successfully, False otherwise
        """
        try:
            # Ensure message is a TaskRequestMessage
            if not isinstance(message, TaskRequestMessage):
                logger.error(
                    "Received non-TaskRequestMessage",
                    message_type=type(message).__name__,
                    channel=channel
                )
                return False
            
            # Process based on task type
            if message.task_type == "process_project_documents":
                # Trigger the Celery task
                task_result = self.process_project_documents_task.delay(str(message.project_id))
                
                logger.info(
                    "Celery task triggered successfully",
                    task_type=message.task_type,
                    project_id=str(message.project_id),
                    correlation_id=str(message.correlation_id),
                    task_id=task_result.id
                )
                return True
            else:
                logger.warning(
                    "Unknown task type received",
                    task_type=message.task_type,
                    project_id=str(message.project_id),
                    correlation_id=str(message.correlation_id)
                )
                return False
                
        except Exception as e:
            logger.error(
                "Failed to handle task request message",
                task_type=getattr(message, 'task_type', 'unknown'),
                project_id=str(getattr(message, 'project_id', 'unknown')),
                correlation_id=str(getattr(message, 'correlation_id', 'unknown')),
                error=str(e),
                exc_info=True
            )
            return False 


def create_message_registry() -> MessageTypeRegistry:
    """
    Create and configure message type registry for unified Redis messages.
    
    Returns:
        MessageTypeRegistry: Configured registry with all unified message types
    """
    registry = MessageTypeRegistry()
    
    # Register unified message types
    registry.register_message_type("task_request", TaskRequestMessage)
    registry.register_message_type("project_progress", ProjectProgressMessage)
    
    return registry


def create_redis_message_factory(redis_client) -> tuple[TaskRequestPublisher, ProjectProgressPublisher, TaskRequestSubscriber]:
    """
    Factory function to create unified Redis publishers and subscribers with proper configuration.
    
    Args:
        redis_client: Redis client instance
        
    Returns:
        tuple: (TaskRequestPublisher, ProjectProgressPublisher, TaskRequestSubscriber)
    """
    # Create unified configuration
    config = RedisMessageConfig()
    
    # Create message registry
    message_registry = create_message_registry()
    
    # Create publishers
    task_publisher = TaskRequestPublisher(redis_client, config)
    progress_publisher = ProjectProgressPublisher(redis_client, config)
    
    # Create subscriber
    task_subscriber = TaskRequestSubscriber(redis_client, config, message_registry)
    
    return task_publisher, progress_publisher, task_subscriber 