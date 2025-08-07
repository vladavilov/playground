"""
Tests for TaskRequestPublisher service and task message schema.
Tests the Redis-based task queuing pattern for cross-service communication.
"""

from uuid import uuid4
from datetime import datetime
from unittest.mock import AsyncMock, Mock

from utils.unified_redis_messages import TaskRequestMessage
from services.task_publisher import TaskRequestPublisher


class TestTaskRequestMessage:
    """Test TaskRequestMessage data model."""

    def test_task_request_message_creation(self):
        """Test creating a TaskRequestMessage with all required fields."""
        project_id = uuid4()
        correlation_id = uuid4()
        timestamp = datetime.now()
        
        message = TaskRequestMessage(
            task_type="process_project_documents",
            project_id=project_id,
            correlation_id=correlation_id,
            timestamp=timestamp,
            parameters={"priority": "high"}
        )
        
        assert message.task_type == "process_project_documents"
        assert message.project_id == project_id
        assert message.correlation_id == correlation_id
        assert message.timestamp == timestamp
        assert message.parameters == {"priority": "high"}

    def test_task_request_message_to_dict(self):
        """Test converting TaskRequestMessage to dictionary for JSON serialization."""
        project_id = uuid4()
        correlation_id = uuid4()
        timestamp = datetime.now()
        
        message = TaskRequestMessage(
            task_type="process_project_documents",
            project_id=project_id,
            correlation_id=correlation_id,
            timestamp=timestamp,
            parameters={"source": "manual", "priority": "high"}
        )
        
        result = message.to_dict()
        
        # Verify basic structure
        assert result["message_type"] == "task_request"
        assert result["task_type"] == "process_project_documents"
        assert result["project_id"] == str(project_id)
        assert result["correlation_id"] == str(correlation_id)
        assert result["timestamp"] == timestamp.isoformat()
        assert result["parameters"] == {"source": "manual", "priority": "high"}

    def test_task_request_message_optional_fields(self):
        """Test creating TaskRequestMessage with minimal fields."""
        project_id = uuid4()
        correlation_id = uuid4()
        
        message = TaskRequestMessage(
            task_type="process_project_documents",
            project_id=project_id,
            correlation_id=correlation_id,
            parameters={}
        )
        
        assert message.task_type == "process_project_documents"
        assert message.project_id == project_id
        assert message.correlation_id == correlation_id
        assert message.parameters == {}
        assert isinstance(message.timestamp, datetime)

    def test_task_request_message_from_dict(self):
        """Test creating TaskRequestMessage from dictionary."""
        project_id = uuid4()
        correlation_id = uuid4()
        timestamp = datetime.now()
        
        data = {
            "message_id": str(uuid4()),
            "message_type": "task_request",
            "task_type": "process_project_documents",
            "project_id": str(project_id),
            "correlation_id": str(correlation_id),
            "timestamp": timestamp.isoformat(),
            "parameters": {"source": "api"}
        }
        
        message = TaskRequestMessage.from_dict(data)
        
        assert message.task_type == "process_project_documents"
        assert message.project_id == project_id
        assert message.correlation_id == correlation_id
        assert message.parameters == {"source": "api"}


class TestTaskRequestPublisher:
    """Test TaskRequestPublisher implementation."""

    def test_task_request_publisher_creation(self):
        """Test that TaskRequestPublisher can be created successfully."""
        mock_redis_client = AsyncMock()
        publisher = TaskRequestPublisher(mock_redis_client)
        
        assert publisher is not None
        assert hasattr(publisher, 'request_document_processing')
        assert hasattr(publisher, 'get_default_channel')
        assert hasattr(publisher, 'get_default_stream')

    def test_get_default_channel(self):
        """Test getting default channel name."""
        mock_redis_client = AsyncMock()
        publisher = TaskRequestPublisher(mock_redis_client)
        
        channel = publisher.get_default_channel()
        assert channel == "task_streams:document_processing"

    def test_get_default_stream(self):
        """Test getting default stream name."""
        mock_redis_client = AsyncMock()
        publisher = TaskRequestPublisher(mock_redis_client)
        
        stream = publisher.get_default_stream()
        assert stream == "task_streams:document_processing"

    async def test_request_document_processing_success(self):
        """Test successful document processing request."""
        mock_redis_client = AsyncMock()
        mock_redis_client.xadd.return_value = "1640000000000-0"  # Mock stream ID
        
        publisher = TaskRequestPublisher(mock_redis_client)
        project_id = uuid4()
        
        result = await publisher.request_document_processing(project_id)
        
        assert result is True
        mock_redis_client.xadd.assert_called_once()

    async def test_request_document_processing_failure(self):
        """Test failed document processing request."""
        mock_redis_client = AsyncMock()
        mock_redis_client.xadd.side_effect = Exception("Redis error")
        
        publisher = TaskRequestPublisher(mock_redis_client)
        project_id = uuid4()
        
        result = await publisher.request_document_processing(project_id)
        
        assert result is False
        mock_redis_client.xadd.assert_called_once()


class TestTaskMessageTypes:
    """Test task message type validation and processing."""

    def test_valid_task_types(self):
        """Test that valid task types are accepted."""
        valid_task_types = [
            "process_project_documents",
            "update_project_status",
            "cleanup_temporary_files"
        ]
        
        project_id = uuid4()
        correlation_id = uuid4()
        
        for task_type in valid_task_types:
            message = TaskRequestMessage(
                task_type=task_type,
                project_id=project_id,
                correlation_id=correlation_id,
                parameters={}
            )
            
            assert message.task_type == task_type
            assert message.message_type == "task_request"

    def test_task_message_serialization_roundtrip(self):
        """Test that task messages can be serialized and deserialized correctly."""
        project_id = uuid4()
        correlation_id = uuid4()
        
        original_message = TaskRequestMessage(
            task_type="process_project_documents",
            project_id=project_id,
            correlation_id=correlation_id,
            parameters={"source": "api", "priority": "high"}
        )
        
        # Serialize to dict then back to message
        message_dict = original_message.to_dict()
        reconstructed_message = TaskRequestMessage.from_dict(message_dict)
        
        assert reconstructed_message.task_type == original_message.task_type
        assert reconstructed_message.project_id == original_message.project_id
        assert reconstructed_message.correlation_id == original_message.correlation_id
        assert reconstructed_message.parameters == original_message.parameters
        assert reconstructed_message.message_type == original_message.message_type 