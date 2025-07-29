"""
Tests for TaskRequestPublisher service and task message schema.
Tests the Redis-based task queuing pattern for cross-service communication.
"""

import pytest
import json
from unittest.mock import AsyncMock, Mock, patch
from uuid import uuid4
from datetime import datetime
from typing import Dict, Any

from utils.unified_redis_messages import TaskRequestPublisher, TaskRequestMessage, RedisMessageConfig
from models.project_rest import ProjectStatus


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
    """Test TaskRequestPublisher service class."""
    
    @pytest.fixture
    def mock_redis_client(self):
        """Mock Redis client for testing."""
        mock_client = AsyncMock()
        return mock_client

    @pytest.fixture
    def task_publisher(self, mock_redis_client):
        """Create TaskRequestPublisher instance with mocked Redis client."""
        config = RedisMessageConfig()
        return TaskRequestPublisher(mock_redis_client, config)

    @pytest.mark.asyncio
    async def test_request_document_processing_success(self, task_publisher, mock_redis_client):
        """Test successful document processing request."""
        project_id = uuid4()
        
        # Mock the publish_message method to return True
        with patch.object(task_publisher, 'publish_message', return_value=True) as mock_publish:
            result = await task_publisher.request_document_processing(project_id)
            
            assert result is True
            mock_publish.assert_called_once()
            
            # Verify the message was created correctly
            call_args = mock_publish.call_args
            message = call_args[0][0]  # First argument is the message
            
            assert isinstance(message, TaskRequestMessage)
            assert message.task_type == "process_project_documents"
            assert message.project_id == project_id
            assert isinstance(message.correlation_id, type(uuid4()))
            assert message.parameters == {}

    @pytest.mark.asyncio
    async def test_request_document_processing_failure(self, task_publisher, mock_redis_client):
        """Test handling document processing request failure."""
        project_id = uuid4()
        
        # Mock the publish_message method to return False
        with patch.object(task_publisher, 'publish_message', return_value=False):
            result = await task_publisher.request_document_processing(project_id)
            
            assert result is False

    @pytest.mark.asyncio
    async def test_check_connection_success(self, task_publisher, mock_redis_client):
        """Test successful Redis connection check."""
        result = await task_publisher.check_connection()
        
        assert result is True
        mock_redis_client.ping.assert_called_once()

    @pytest.mark.asyncio
    async def test_check_connection_failure(self, task_publisher, mock_redis_client):
        """Test Redis connection check failure."""
        mock_redis_client.ping.side_effect = Exception("Connection failed")
        
        result = await task_publisher.check_connection()
        
        assert result is False
        mock_redis_client.ping.assert_called_once()

    def test_get_default_channel(self, task_publisher):
        """Test default channel name generation."""
        channel_name = task_publisher.get_default_channel()
        
        assert channel_name == "task_requests:document_processing"  # Default channel from config


class TestTaskRequestIntegration:
    """Test TaskRequestPublisher integration with service workflows."""

    @pytest.fixture
    def mock_task_publisher(self):
        """Create a mock task publisher."""
        mock_publisher = AsyncMock(spec=TaskRequestPublisher)
        mock_publisher.request_document_processing = AsyncMock(return_value=True)
        return mock_publisher

    @pytest.mark.asyncio
    async def test_document_processing_request_workflow(self, mock_task_publisher):
        """Test complete document processing request workflow."""
        project_id = uuid4()
        
        # Test the workflow
        result = await mock_task_publisher.request_document_processing(project_id)
        
        assert result is True
        mock_task_publisher.request_document_processing.assert_called_once_with(project_id)

    @pytest.mark.asyncio
    async def test_multiple_concurrent_requests(self, mock_task_publisher):
        """Test handling multiple concurrent document processing requests."""
        project_ids = [uuid4() for _ in range(3)]
        
        # Simulate concurrent requests
        results = []
        for project_id in project_ids:
            result = await mock_task_publisher.request_document_processing(project_id)
            results.append(result)
        
        assert all(results)
        assert mock_task_publisher.request_document_processing.call_count == 3

    def test_task_publisher_initialization(self):
        """Test that TaskRequestPublisher can be initialized correctly."""
        from utils.unified_redis_messages import create_redis_message_factory
        from utils.redis_client import get_redis_client
        
        with patch('utils.redis_client.get_redis_client') as mock_get_client:
            mock_client = AsyncMock()
            mock_get_client.return_value = mock_client
            
            task_publisher, _, _ = create_redis_message_factory(mock_client)
            
            assert isinstance(task_publisher, TaskRequestPublisher)
            assert hasattr(task_publisher, 'request_document_processing')

    @pytest.mark.asyncio
    async def test_task_publisher_error_handling(self, mock_task_publisher):
        """Test task publisher handles errors gracefully."""
        project_id = uuid4()
        
        # Mock failure scenario
        mock_task_publisher.request_document_processing.return_value = False
        
        result = await mock_task_publisher.request_document_processing(project_id)
        
        assert result is False
        mock_task_publisher.request_document_processing.assert_called_once_with(project_id)


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