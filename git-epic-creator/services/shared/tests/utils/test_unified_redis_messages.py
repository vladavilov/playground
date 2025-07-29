"""
Tests for unified Redis message classes that replace duplicated message types.
Tests for TaskRequestMessage, ProjectProgressMessage, and their configurations.
"""

import pytest
import json
from unittest.mock import AsyncMock, Mock
from uuid import UUID, uuid4
from datetime import datetime
from dataclasses import dataclass
from typing import Dict, Any, Optional

from utils.unified_redis_messages import (
    TaskRequestMessage,
    ProjectProgressMessage,
    TaskRequestPublisher,
    ProjectProgressPublisher,
    TaskRequestSubscriber,
    RedisMessageConfig
)


class TestTaskRequestMessage:
    """Test the unified TaskRequestMessage class."""
    
    def test_task_request_message_creation(self):
        """Test creating a TaskRequestMessage with all required fields."""
        message_id = uuid4()
        timestamp = datetime.now()
        project_id = uuid4()
        correlation_id = uuid4()
        
        message = TaskRequestMessage(
            message_id=message_id,
            timestamp=timestamp,
            task_type="process_project_documents",
            project_id=project_id,
            correlation_id=correlation_id,
            parameters={"key": "value"}
        )
        
        assert message.message_id == message_id
        assert message.timestamp == timestamp
        assert message.task_type == "process_project_documents"
        assert message.project_id == project_id
        assert message.correlation_id == correlation_id
        assert message.parameters == {"key": "value"}
    
    def test_task_request_message_to_dict(self):
        """Test serializing TaskRequestMessage to dictionary."""
        message_id = uuid4()
        timestamp = datetime.now()
        project_id = uuid4()
        correlation_id = uuid4()
        
        message = TaskRequestMessage(
            message_id=message_id,
            timestamp=timestamp,
            task_type="process_project_documents",
            project_id=project_id,
            correlation_id=correlation_id,
            parameters={"test": "data"}
        )
        
        result = message.to_dict()
        
        assert result["message_id"] == str(message_id)
        assert result["timestamp"] == timestamp.isoformat()
        assert result["task_type"] == "process_project_documents"
        assert result["project_id"] == str(project_id)
        assert result["correlation_id"] == str(correlation_id)
        assert result["parameters"] == {"test": "data"}
    
    def test_task_request_message_from_dict(self):
        """Test deserializing TaskRequestMessage from dictionary."""
        message_id = uuid4()
        timestamp = datetime.now()
        project_id = uuid4()
        correlation_id = uuid4()
        
        data = {
            "message_id": str(message_id),
            "timestamp": timestamp.isoformat(),
            "task_type": "process_project_documents",
            "project_id": str(project_id),
            "correlation_id": str(correlation_id),
            "parameters": {"key": "value"}
        }
        
        message = TaskRequestMessage.from_dict(data)
        
        assert message.message_id == message_id
        assert message.timestamp == timestamp
        assert message.task_type == "process_project_documents"
        assert message.project_id == project_id
        assert message.correlation_id == correlation_id
        assert message.parameters == {"key": "value"}
    
    def test_task_request_message_empty_parameters(self):
        """Test TaskRequestMessage with empty parameters."""
        message = TaskRequestMessage(
            message_id=uuid4(),
            timestamp=datetime.now(),
            task_type="test_task",
            project_id=uuid4(),
            correlation_id=uuid4(),
            parameters={}
        )
        
        result = message.to_dict()
        assert result["parameters"] == {}


class TestProjectProgressMessage:
    """Test the unified ProjectProgressMessage class."""
    
    def test_project_progress_message_creation(self):
        """Test creating a ProjectProgressMessage with all fields."""
        message_id = uuid4()
        timestamp = datetime.now()
        project_id = uuid4()
        
        message = ProjectProgressMessage(
            message_id=message_id,
            timestamp=timestamp,
            project_id=project_id,
            status="processing",
            processed_count=5,
            total_count=10,
            processed_pct=50.0
        )
        
        assert message.message_id == message_id
        assert message.timestamp == timestamp
        assert message.project_id == project_id
        assert message.status == "processing"
        assert message.processed_count == 5
        assert message.total_count == 10
        assert message.processed_pct == 50.0
    
    def test_project_progress_message_optional_fields(self):
        """Test ProjectProgressMessage with optional fields as None."""
        message = ProjectProgressMessage(
            message_id=uuid4(),
            timestamp=datetime.now(),
            project_id=uuid4(),
            status="started",
            processed_count=None,
            total_count=None,
            processed_pct=None
        )
        
        result = message.to_dict()
        assert result["processed_count"] is None
        assert result["total_count"] is None
        assert result["processed_pct"] is None
    
    def test_project_progress_message_from_dict(self):
        """Test deserializing ProjectProgressMessage from dictionary."""
        message_id = uuid4()
        timestamp = datetime.now()
        project_id = uuid4()
        
        data = {
            "message_id": str(message_id),
            "timestamp": timestamp.isoformat(),
            "project_id": str(project_id),
            "status": "completed",
            "processed_count": 10,
            "total_count": 10,
            "processed_pct": 100.0
        }
        
        message = ProjectProgressMessage.from_dict(data)
        
        assert message.message_id == message_id
        assert message.timestamp == timestamp
        assert message.project_id == project_id
        assert message.status == "completed"
        assert message.processed_count == 10
        assert message.total_count == 10
        assert message.processed_pct == 100.0


class TestTaskRequestPublisher:
    """Test the refactored TaskRequestPublisher using base classes."""
    
    @pytest.fixture
    def mock_redis_client(self):
        """Mock Redis client for testing."""
        return AsyncMock()
    
    @pytest.fixture
    def mock_config(self):
        """Mock Redis message configuration."""
        config = Mock()
        config.get_task_request_channel.return_value = "task_requests:document_processing"
        return config
    
    @pytest.fixture
    def publisher(self, mock_redis_client, mock_config):
        """Create TaskRequestPublisher with mocked dependencies."""
        return TaskRequestPublisher(mock_redis_client, mock_config)
    
    @pytest.mark.asyncio
    async def test_request_document_processing_success(self, publisher, mock_redis_client):
        """Test successful document processing request."""
        project_id = uuid4()
        mock_redis_client.publish.return_value = 1
        
        result = await publisher.request_document_processing(project_id)
        
        assert result is True
        mock_redis_client.publish.assert_called_once()
        
        # Verify the published message
        call_args = mock_redis_client.publish.call_args
        channel, message_data = call_args[0]
        
        assert channel == "task_requests:document_processing"
        message_dict = json.loads(message_data)
        assert message_dict["task_type"] == "process_project_documents"
        assert UUID(message_dict["project_id"]) == project_id
    
    @pytest.mark.asyncio
    async def test_request_document_processing_failure(self, publisher, mock_redis_client):
        """Test failed document processing request."""
        project_id = uuid4()
        mock_redis_client.publish.side_effect = Exception("Redis error")
        
        result = await publisher.request_document_processing(project_id)
        
        assert result is False
    
    def test_get_default_channel(self, publisher, mock_config):
        """Test getting default channel name."""
        result = publisher.get_default_channel()
        
        mock_config.get_task_request_channel.assert_called_once()
        assert result == "task_requests:document_processing"


class TestProjectProgressPublisher:
    """Test the refactored ProjectProgressPublisher using base classes."""
    
    @pytest.fixture
    def mock_redis_client(self):
        """Mock Redis client for testing."""
        return AsyncMock()
    
    @pytest.fixture
    def mock_config(self):
        """Mock Redis message configuration."""
        config = Mock()
        config.get_project_progress_channel.return_value = "project_progress:123"
        return config
    
    @pytest.fixture
    def publisher(self, mock_redis_client, mock_config):
        """Create ProjectProgressPublisher with mocked dependencies."""
        return ProjectProgressPublisher(mock_redis_client, mock_config)
    
    @pytest.mark.asyncio
    async def test_publish_project_update_success(self, publisher, mock_redis_client):
        """Test successful project update publishing."""
        project_id = uuid4()
        mock_redis_client.publish.return_value = 1
        
        result = await publisher.publish_project_update(
            project_id=project_id,
            status="processing",
            processed_count=5,
            total_count=10,
            processed_pct=50.0
        )
        
        assert result is True
        mock_redis_client.publish.assert_called_once()
        
        # Verify the published message structure
        call_args = mock_redis_client.publish.call_args
        channel, message_data = call_args[0]
        
        message_dict = json.loads(message_data)
        assert UUID(message_dict["project_id"]) == project_id
        assert message_dict["status"] == "processing"
        assert message_dict["processed_count"] == 5
    
    @pytest.mark.asyncio
    async def test_publish_project_update_failure(self, publisher, mock_redis_client):
        """Test failed project update publishing."""
        project_id = uuid4()
        mock_redis_client.publish.side_effect = Exception("Redis error")
        
        result = await publisher.publish_project_update(
            project_id=project_id,
            status="error"
        )
        
        assert result is False


class TestRedisMessageConfig:
    """Test the Redis message configuration class."""
    
    def test_get_task_request_channel(self):
        """Test getting task request channel name."""
        config = RedisMessageConfig(
            task_request_channel_prefix="task_requests",
            project_progress_channel_prefix="project_progress"
        )
        
        result = config.get_task_request_channel("document_processing")
        
        assert result == "task_requests:document_processing"
    
    def test_get_project_progress_channel(self):
        """Test getting project progress channel name."""
        config = RedisMessageConfig(
            task_request_channel_prefix="task_requests",
            project_progress_channel_prefix="project_progress"
        )
        
        project_id = uuid4()
        result = config.get_project_progress_channel(project_id)
        
        assert result == f"project_progress:{project_id}"
    
    def test_custom_separators(self):
        """Test configuration with custom separators."""
        config = RedisMessageConfig(
            task_request_channel_prefix="tasks",
            project_progress_channel_prefix="progress",
            channel_separator="."
        )
        
        result = config.get_task_request_channel("test")
        assert result == "tasks.test" 


class TestTaskRequestSubscriber:
    """Test the unified TaskRequestSubscriber using base classes."""
    
    @pytest.fixture
    def mock_redis_client(self):
        """Mock Redis client for testing."""
        client = AsyncMock()
        client.pubsub.return_value = AsyncMock()
        return client
    
    @pytest.fixture
    def mock_config(self):
        """Mock Redis message configuration."""
        config = Mock()
        config.get_task_request_channel.return_value = "task_requests:document_processing"
        return config
    
    @pytest.fixture
    def mock_message_registry(self):
        """Mock message type registry."""
        registry = Mock()
        registry.get_message_class.return_value = TaskRequestMessage
        return registry
    
    @pytest.fixture
    def mock_celery_task(self):
        """Mock Celery task for processing."""
        task = Mock()
        task.delay.return_value = Mock(id="task-123")
        return task
    
    @pytest.fixture
    def subscriber(self, mock_redis_client, mock_config, mock_message_registry, mock_celery_task):
        """Create TaskRequestSubscriber with mocked dependencies."""
        return TaskRequestSubscriber(
            redis_client=mock_redis_client,
            config=mock_config,
            message_registry=mock_message_registry,
            process_project_documents_task=mock_celery_task
        )
    
    def test_get_subscription_channels(self, subscriber, mock_config):
        """Test getting subscription channels."""
        result = subscriber.get_subscription_channels()
        
        mock_config.get_task_request_channel.assert_called_once()
        assert result == ["task_requests:document_processing"]
    
    @pytest.mark.asyncio
    async def test_handle_task_request_message_success(self, subscriber, mock_celery_task):
        """Test successful handling of task request message."""
        project_id = uuid4()
        message = TaskRequestMessage(
            message_id=uuid4(),
            timestamp=datetime.now(),
            task_type="process_project_documents",
            project_id=project_id,
            correlation_id=uuid4(),
            parameters={}
        )
        
        result = await subscriber.handle_message(message, "task_requests:document_processing")
        
        assert result is True
        mock_celery_task.delay.assert_called_once_with(str(project_id))
    
    @pytest.mark.asyncio
    async def test_handle_unknown_task_type(self, subscriber, mock_celery_task):
        """Test handling of unknown task type."""
        message = TaskRequestMessage(
            message_id=uuid4(),
            timestamp=datetime.now(),
            task_type="unknown_task",
            project_id=uuid4(),
            correlation_id=uuid4(),
            parameters={}
        )
        
        result = await subscriber.handle_message(message, "task_requests:document_processing")
        
        assert result is False
        mock_celery_task.delay.assert_not_called()
    
    @pytest.mark.asyncio
    async def test_handle_celery_task_failure(self, subscriber, mock_celery_task):
        """Test handling when Celery task fails to trigger."""
        mock_celery_task.delay.side_effect = Exception("Celery error")
        
        message = TaskRequestMessage(
            message_id=uuid4(),
            timestamp=datetime.now(),
            task_type="process_project_documents",
            project_id=uuid4(),
            correlation_id=uuid4(),
            parameters={}
        )
        
        result = await subscriber.handle_message(message, "task_requests:document_processing")
        
        assert result is False 