"""
Tests for ProjectStatusPublisher service.
Tests the Redis-based project status publishing for real-time UI updates.
"""

from uuid import uuid4
from datetime import datetime
from unittest.mock import AsyncMock, Mock

from utils.unified_redis_messages import ProjectProgressMessage
from services.project_status_publisher import ProjectStatusPublisher


class TestProjectStatusPublisher:
    """Test ProjectStatusPublisher implementation."""

    def test_project_status_publisher_creation(self):
        """Test that ProjectStatusPublisher can be created successfully."""
        mock_redis_client = AsyncMock()
        publisher = ProjectStatusPublisher(mock_redis_client)
        
        assert publisher is not None
        assert hasattr(publisher, 'publish_project_update')
        assert hasattr(publisher, 'get_default_channel')
        assert hasattr(publisher, 'get_default_stream')

    def test_get_default_channel(self):
        """Test getting default channel name."""
        mock_redis_client = AsyncMock()
        publisher = ProjectStatusPublisher(mock_redis_client)
        
        channel = publisher.get_default_channel()
        assert channel == "ui:project_progress"

    def test_get_default_stream(self):
        """Test getting default stream name."""
        mock_redis_client = AsyncMock()
        publisher = ProjectStatusPublisher(mock_redis_client)
        
        stream = publisher.get_default_stream()
        assert stream == "ui:project_progress"

    async def test_publish_project_update_success(self):
        """Test successful project status update publishing."""
        mock_redis_client = AsyncMock()
        mock_redis_client.publish.return_value = 1  # Mock publish result
        
        publisher = ProjectStatusPublisher(mock_redis_client)
        project_id = uuid4()
        
        result = await publisher.publish_project_update(
            project_id=project_id,
            status="processing",
            processed_count=5,
            total_count=10,
            processed_pct=50.0
        )
        
        assert result is True
        mock_redis_client.publish.assert_called_once()

    async def test_publish_project_update_failure(self):
        """Test failed project status update publishing."""
        mock_redis_client = AsyncMock()
        mock_redis_client.publish.side_effect = Exception("Redis error")
        
        publisher = ProjectStatusPublisher(mock_redis_client)
        project_id = uuid4()
        
        result = await publisher.publish_project_update(
            project_id=project_id,
            status="processing",
            processed_count=5,
            total_count=10,
            processed_pct=50.0
        )
        
        assert result is False
        mock_redis_client.publish.assert_called_once()

    async def test_publish_project_update_with_none_values(self):
        """Test publishing project update with None values for optional fields."""
        mock_redis_client = AsyncMock()
        mock_redis_client.publish.return_value = 1
        
        publisher = ProjectStatusPublisher(mock_redis_client)
        project_id = uuid4()
        
        result = await publisher.publish_project_update(
            project_id=project_id,
            status="started",
            processed_count=None,
            total_count=None,
            processed_pct=None
        )
        
        assert result is True
        mock_redis_client.publish.assert_called_once()

    async def test_publish_project_update_minimal(self):
        """Test publishing project update with minimal required fields."""
        mock_redis_client = AsyncMock()
        mock_redis_client.publish.return_value = 1
        
        publisher = ProjectStatusPublisher(mock_redis_client)
        project_id = uuid4()
        
        result = await publisher.publish_project_update(
            project_id=project_id,
            status="completed"
        )
        
        assert result is True
        mock_redis_client.publish.assert_called_once()


class TestProjectProgressMessage:
    """Test ProjectProgressMessage data model."""

    def test_project_progress_message_creation(self):
        """Test creating a ProjectProgressMessage with all fields."""
        project_id = uuid4()
        timestamp = datetime.now()
        
        message = ProjectProgressMessage(
            project_id=project_id,
            status="processing",
            processed_count=5,
            total_count=10,
            processed_pct=50.0,
            timestamp=timestamp
        )
        
        assert message.project_id == project_id
        assert message.status == "processing"
        assert message.processed_count == 5
        assert message.total_count == 10
        assert message.processed_pct == 50.0
        assert message.timestamp == timestamp

    def test_project_progress_message_to_dict(self):
        """Test converting ProjectProgressMessage to dictionary for JSON serialization."""
        project_id = uuid4()
        timestamp = datetime.now()
        
        message = ProjectProgressMessage(
            project_id=project_id,
            status="completed",
            processed_count=10,
            total_count=10,
            processed_pct=100.0,
            timestamp=timestamp
        )
        
        result = message.to_dict()
        
        # Verify basic structure
        assert result["message_type"] == "project_progress"
        assert result["project_id"] == str(project_id)
        assert result["status"] == "completed"
        assert result["processed_count"] == 10
        assert result["total_count"] == 10
        assert result["processed_pct"] == 100.0
        assert result["timestamp"] == timestamp.isoformat()

    def test_project_progress_message_optional_fields(self):
        """Test creating ProjectProgressMessage with minimal fields."""
        project_id = uuid4()
        
        message = ProjectProgressMessage(
            project_id=project_id,
            status="started",
            processed_count=None,
            total_count=None,
            processed_pct=None
        )
        
        assert message.project_id == project_id
        assert message.status == "started"
        assert message.processed_count is None
        assert message.total_count is None
        assert message.processed_pct is None
        assert isinstance(message.timestamp, datetime)

    def test_project_progress_message_from_dict(self):
        """Test creating ProjectProgressMessage from dictionary."""
        project_id = uuid4()
        timestamp = datetime.now()
        
        data = {
            "message_id": str(uuid4()),
            "message_type": "project_progress",
            "project_id": str(project_id),
            "status": "completed",
            "processed_count": 10,
            "total_count": 10,
            "processed_pct": 100.0,
            "timestamp": timestamp.isoformat()
        }
        
        message = ProjectProgressMessage.from_dict(data)
        
        assert message.project_id == project_id
        assert message.status == "completed"
        assert message.processed_count == 10
        assert message.total_count == 10
        assert message.processed_pct == 100.0 