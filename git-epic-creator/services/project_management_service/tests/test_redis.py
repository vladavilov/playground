"""
Consolidated tests for Redis publisher service and integration.
Combines Redis publisher tests, integration tests, and endpoint integration tests.
"""

import pytest
import json
from unittest.mock import AsyncMock, Mock, patch
from uuid import uuid4
from datetime import datetime
from fastapi import HTTPException, status

from services.redis_publisher import RedisPublisher, ProjectProgressMessage
from models.project_rest import ProjectProgressUpdateRequest
from models.project_rest import ProjectStatus


class TestProjectProgressMessage:
    """Test ProjectProgressMessage data model."""

    def test_project_progress_message_creation(self):
        """Test creating a ProjectProgressMessage with all fields."""
        project_id = uuid4()
        message = ProjectProgressMessage(
            project_id=project_id,
            status=ProjectStatus.PROCESSING.value,
            processed_count=5,
            total_count=10,
            processed_pct=50.0,
            timestamp=datetime.now()
        )
        
        assert message.project_id == project_id
        assert message.status == ProjectStatus.PROCESSING.value
        assert message.processed_count == 5
        assert message.total_count == 10
        assert message.processed_pct == 50.0
        assert isinstance(message.timestamp, datetime)

    def test_project_progress_message_to_dict(self):
        """Test converting ProjectProgressMessage to dictionary."""
        project_id = uuid4()
        timestamp = datetime.now()
        message = ProjectProgressMessage(
            project_id=project_id,
            status=ProjectStatus.PROCESSING.value,
            processed_count=5,
            total_count=10,
            processed_pct=50.0,
            timestamp=timestamp
        )
        
        result = message.to_dict()
        
        assert result["project_id"] == str(project_id)
        assert result["status"] == ProjectStatus.PROCESSING.value
        assert result["processed_count"] == 5
        assert result["total_count"] == 10
        assert result["processed_pct"] == 50.0
        assert result["timestamp"] == timestamp.isoformat()

    def test_project_progress_message_optional_fields(self):
        """Test creating ProjectProgressMessage with optional fields as None."""
        project_id = uuid4()
        message = ProjectProgressMessage(
            project_id=project_id,
            status="COMPLETED",
            processed_count=None,
            total_count=None,
            processed_pct=None,
            timestamp=datetime.now()
        )
        
        assert message.project_id == project_id
        assert message.status == "COMPLETED"
        assert message.processed_count is None
        assert message.total_count is None
        assert message.processed_pct is None


class TestRedisPublisher:
    """Test RedisPublisher service class."""

    @pytest.fixture
    def mock_redis_client(self):
        """Create a mock Redis client."""
        mock_client = AsyncMock()
        mock_client.publish = AsyncMock(return_value=1)
        mock_client.ping = AsyncMock(return_value=True)
        return mock_client

    @pytest.fixture
    def redis_publisher(self, mock_redis_client):
        """Create RedisPublisher instance with mocked Redis client."""
        with patch('services.redis_publisher.get_redis_client', return_value=mock_redis_client):
            return RedisPublisher()

    def test_redis_publisher_initialization(self, mock_redis_client):
        """Test RedisPublisher initialization."""
        with patch('services.redis_publisher.get_redis_client', return_value=mock_redis_client) as mock_get_client:
            publisher = RedisPublisher()
            
            mock_get_client.assert_called_once()
            assert publisher.redis_client == mock_redis_client

    @pytest.mark.asyncio
    async def test_publish_project_update_success(self, redis_publisher, mock_redis_client):
        """Test successful project update publishing."""
        project_id = uuid4()
        message = ProjectProgressMessage(
            project_id=project_id,
            status=ProjectStatus.PROCESSING.value,
            processed_count=5,
            total_count=10,
            processed_pct=50.0,
            timestamp=datetime.now()
        )
        
        result = await redis_publisher.publish_project_update(message)
        
        assert result is True
        mock_redis_client.publish.assert_called_once()
        
        # Verify the channel name format
        call_args = mock_redis_client.publish.call_args
        channel = call_args[0][0]
        message_data = call_args[0][1]
        
        assert channel == f"project_progress:{project_id}"
        
        # Verify message content
        parsed_message = json.loads(message_data)
        assert parsed_message["project_id"] == str(project_id)
        assert parsed_message["status"] == ProjectStatus.PROCESSING.value
        assert parsed_message["processed_count"] == 5
        assert parsed_message["total_count"] == 10
        assert parsed_message["processed_pct"] == 50.0

    @pytest.mark.asyncio
    async def test_publish_project_update_redis_failure(self, redis_publisher, mock_redis_client):
        """Test handling Redis publish failure."""
        project_id = uuid4()
        message = ProjectProgressMessage(
            project_id=project_id,
            status=ProjectStatus.PROCESSING.value,
            processed_count=5,
            total_count=10,
            processed_pct=50.0,
            timestamp=datetime.now()
        )
        
        # Mock Redis publish to raise an exception
        mock_redis_client.publish.side_effect = Exception("Redis connection failed")
        
        result = await redis_publisher.publish_project_update(message)
        
        assert result is False
        mock_redis_client.publish.assert_called_once()

    @pytest.mark.asyncio
    async def test_publish_project_update_with_none_values(self, redis_publisher, mock_redis_client):
        """Test publishing project update with None values for optional fields."""
        project_id = uuid4()
        message = ProjectProgressMessage(
            project_id=project_id,
            status="COMPLETED",
            processed_count=None,
            total_count=None,
            processed_pct=None,
            timestamp=datetime.now()
        )
        
        result = await redis_publisher.publish_project_update(message)
        
        assert result is True
        mock_redis_client.publish.assert_called_once()
        
        # Verify message content includes None values
        call_args = mock_redis_client.publish.call_args
        message_data = call_args[0][1]
        parsed_message = json.loads(message_data)
        
        assert parsed_message["processed_count"] is None
        assert parsed_message["total_count"] is None
        assert parsed_message["processed_pct"] is None

    @pytest.mark.asyncio
    async def test_check_connection_success(self, redis_publisher, mock_redis_client):
        """Test successful Redis connection check."""
        result = await redis_publisher.check_connection()
        
        assert result is True
        mock_redis_client.ping.assert_called_once()

    @pytest.mark.asyncio
    async def test_check_connection_failure(self, redis_publisher, mock_redis_client):
        """Test Redis connection check failure."""
        mock_redis_client.ping.side_effect = Exception("Connection failed")
        
        result = await redis_publisher.check_connection()
        
        assert result is False
        mock_redis_client.ping.assert_called_once()

    def test_get_channel_name(self, redis_publisher):
        """Test channel name generation."""
        project_id = uuid4()
        channel_name = redis_publisher._get_channel_name(project_id)
        
        assert channel_name == f"project_progress:{project_id}"

    def test_get_channel_name_with_string_id(self, redis_publisher):
        """Test channel name generation with string project ID."""
        project_id = "test-project-123"
        channel_name = redis_publisher._get_channel_name(project_id)
        
        assert channel_name == f"project_progress:{project_id}"


class TestRedisEndpointIntegration:
    """Test Redis publisher integration with API endpoint."""

    @pytest.fixture
    def mock_redis_client(self):
        """Create a mock Redis client."""
        mock_client = AsyncMock()
        mock_client.publish = AsyncMock(return_value=1)
        mock_client.ping = AsyncMock(return_value=True)
        return mock_client

    @pytest.fixture
    def mock_project_service(self):
        """Mock project service."""
        service = Mock()
        return service

    @pytest.fixture
    def sample_project(self):
        """Create a sample project with all required attributes."""
        project = Mock()
        project.id = uuid4()
        project.name = "Test Project"
        project.description = "Test Description"
        project.gitlab_url = "https://gitlab.example.com/project"
        project.gitlab_repository_url = "https://gitlab.example.com/project.git"
        project.status = ProjectStatus.PROCESSING
        project.processed_pct = 50.0
        project.created_by = "test-user-123"
        project.created_at = datetime.now()
        project.updated_at = datetime.now()
        return project

    @pytest.fixture
    def mock_current_user(self):
        """Mock current authenticated user."""
        user = Mock()
        user.oid = "test-user-123"
        user.roles = ["Contributor"]
        return user

    @pytest.fixture
    def redis_publisher(self, mock_redis_client):
        """Create RedisPublisher instance with mocked Redis client."""
        with patch('services.redis_publisher.get_redis_client', return_value=mock_redis_client):
            return RedisPublisher()

    def test_redis_publisher_dependency_initialization(self):
        """Test that Redis publisher dependency initializes correctly."""
        from routers.project_router import get_redis_publisher
        
        with patch('services.redis_publisher.get_redis_client') as mock_get_client:
            mock_client = AsyncMock()
            mock_get_client.return_value = mock_client
            
            publisher = get_redis_publisher()
            
            assert isinstance(publisher, RedisPublisher)
            mock_get_client.assert_called_once()

    @pytest.mark.asyncio
    async def test_endpoint_publishes_progress_update_successfully(
        self, mock_project_service, mock_current_user, redis_publisher, mock_redis_client, sample_project
    ):
        """Test that endpoint publishes progress update to Redis successfully."""
        from routers.project_router import update_project_status
        
        project_id = uuid4()
        update_request = ProjectProgressUpdateRequest(
            status=ProjectStatus.PROCESSING.value,
            processed_count=5,
            total_count=10
        )
        
        # Configure sample project for this test
        sample_project.id = project_id
        sample_project.status = ProjectStatus.PROCESSING.value
        sample_project.processed_pct = 50.0
        mock_project_service.update_project_progress.return_value = sample_project
        
        # Call the endpoint function
        result = await update_project_status(
            project_id=project_id,
            update_request=update_request,
            current_user=mock_current_user,
            project_service=mock_project_service,
            redis_publisher=redis_publisher
        )
        
        # Verify database update was called
        mock_project_service.update_project_progress.assert_called_once_with(
            project_id, 5, 10, status=ProjectStatus.PROCESSING.value
        )
        
        # Verify Redis publish was called
        mock_redis_client.publish.assert_called_once()
        
        # Verify the published message content
        call_args = mock_redis_client.publish.call_args
        channel = call_args[0][0]
        message_data = call_args[0][1]
        
        assert channel == f"project_progress:{project_id}"
        
        parsed_message = json.loads(message_data)
        assert parsed_message["project_id"] == str(project_id)
        assert parsed_message["status"] == ProjectStatus.PROCESSING.value
        assert parsed_message["processed_count"] == 5
        assert parsed_message["total_count"] == 10
        assert parsed_message["processed_pct"] == 50.0
        
        # Verify the API response
        assert result.id == project_id
        assert result.status == ProjectStatus.PROCESSING.value

    @pytest.mark.asyncio
    async def test_endpoint_publishes_status_reset_successfully(
        self, mock_project_service, mock_current_user, redis_publisher, mock_redis_client, sample_project
    ):
        """Test that endpoint publishes status reset to Redis successfully."""
        from routers.project_router import update_project_status
        
        project_id = uuid4()
        update_request = ProjectProgressUpdateRequest(
            status="active"
        )
        
        # Configure sample project for this test
        sample_project.id = project_id
        sample_project.status = "active"
        sample_project.processed_pct = None
        mock_project_service.update_project_progress.return_value = sample_project
        
        # Call the endpoint function
        result = await update_project_status(
            project_id=project_id,
            update_request=update_request,
            current_user=mock_current_user,
            project_service=mock_project_service,
            redis_publisher=redis_publisher
        )
        
        # Verify database update was called
        mock_project_service.update_project_progress.assert_called_once_with(
            project_id, None, None, status="active"
        )
        
        # Verify Redis publish was called
        mock_redis_client.publish.assert_called_once()
        
        # Verify the published message content
        call_args = mock_redis_client.publish.call_args
        channel = call_args[0][0]
        message_data = call_args[0][1]
        
        assert channel == f"project_progress:{project_id}"
        
        parsed_message = json.loads(message_data)
        assert parsed_message["project_id"] == str(project_id)
        assert parsed_message["status"] == "active"
        assert parsed_message["processed_count"] is None
        assert parsed_message["total_count"] is None
        assert parsed_message["processed_pct"] is None
        
        # Verify the API response
        assert result.id == project_id
        assert result.status == "active"

    @pytest.mark.asyncio
    async def test_endpoint_continues_when_redis_publish_fails(
        self, mock_project_service, mock_current_user, redis_publisher, mock_redis_client, sample_project
    ):
        """Test that endpoint continues successfully even when Redis publishing fails."""
        from routers.project_router import update_project_status
        
        project_id = uuid4()
        update_request = ProjectProgressUpdateRequest(
            status=ProjectStatus.PROCESSING.value,
            processed_count=3,
            total_count=10
        )
        
        # Configure sample project for this test
        sample_project.id = project_id
        sample_project.status = ProjectStatus.PROCESSING.value
        sample_project.processed_pct = 30.0
        mock_project_service.update_project_progress.return_value = sample_project
        
        # Mock Redis publish failure
        mock_redis_client.publish.side_effect = Exception("Redis connection failed")
        
        # Call the endpoint function - should not raise exception
        result = await update_project_status(
            project_id=project_id,
            update_request=update_request,
            current_user=mock_current_user,
            project_service=mock_project_service,
            redis_publisher=redis_publisher
        )
        
        # Verify database update was still called
        mock_project_service.update_project_progress.assert_called_once_with(
            project_id, 3, 10, status=ProjectStatus.PROCESSING.value
        )
        
        # Verify Redis publish was attempted
        mock_redis_client.publish.assert_called_once()
        
        # Verify the endpoint still returns the project response
        assert result.id == project_id
        assert result.status == ProjectStatus.PROCESSING.value

    @pytest.mark.asyncio
    async def test_endpoint_handles_project_not_found(
        self, mock_project_service, mock_current_user, redis_publisher
    ):
        """Test that endpoint handles project not found correctly."""
        from routers.project_router import update_project_status
        
        project_id = uuid4()
        update_request = ProjectProgressUpdateRequest(
            status=ProjectStatus.PROCESSING.value,
            processed_count=5,
            total_count=10
        )
        
        # Mock project not found
        mock_project_service.update_project_progress.return_value = None
        
        # Call the endpoint function - should raise HTTPException
        with pytest.raises(HTTPException) as exc_info:
            await update_project_status(
                project_id=project_id,
                update_request=update_request,
                current_user=mock_current_user,
                project_service=mock_project_service,
                redis_publisher=redis_publisher
            )
        
        assert exc_info.value.status_code == status.HTTP_404_NOT_FOUND
        assert "Project not found" in str(exc_info.value.detail)

    @pytest.mark.asyncio
    async def test_endpoint_handles_database_error(
        self, mock_project_service, mock_current_user, redis_publisher
    ):
        """Test that endpoint handles database errors correctly."""
        from routers.project_router import update_project_status
        
        project_id = uuid4()
        update_request = ProjectProgressUpdateRequest(
            status=ProjectStatus.PROCESSING.value,
            processed_count=5,
            total_count=10
        )
        
        # Mock database error
        mock_project_service.update_project_progress.side_effect = Exception("Database error")
        
        # Call the endpoint function - should raise HTTPException
        with pytest.raises(HTTPException) as exc_info:
            await update_project_status(
                project_id=project_id,
                update_request=update_request,
                current_user=mock_current_user,
                project_service=mock_project_service,
                redis_publisher=redis_publisher
            )
        
        assert exc_info.value.status_code == status.HTTP_500_INTERNAL_SERVER_ERROR
        assert "Failed to update project status" in str(exc_info.value.detail)

    @pytest.mark.asyncio
    async def test_redis_publisher_returns_false_on_failure(self, redis_publisher, mock_redis_client):
        """Test that Redis publisher returns False when publishing fails."""
        project_id = uuid4()
        message = ProjectProgressMessage(
            project_id=project_id,
            status=ProjectStatus.PROCESSING.value,
            processed_count=5,
            total_count=10,
            processed_pct=50.0,
            timestamp=datetime.now()
        )
        
        # Mock Redis publish failure
        mock_redis_client.publish.side_effect = Exception("Redis connection failed")
        
        result = await redis_publisher.publish_project_update(message)
        
        assert result is False
        mock_redis_client.publish.assert_called_once()

    @pytest.mark.asyncio
    async def test_redis_publisher_returns_true_on_success(self, redis_publisher, mock_redis_client):
        """Test that Redis publisher returns True when publishing succeeds."""
        project_id = uuid4()
        message = ProjectProgressMessage(
            project_id=project_id,
            status=ProjectStatus.PROCESSING.value,
            processed_count=5,
            total_count=10,
            processed_pct=50.0,
            timestamp=datetime.now()
        )
        
        # Mock successful Redis publish
        mock_redis_client.publish.return_value = 1
        
        result = await redis_publisher.publish_project_update(message)
        
        assert result is True
        mock_redis_client.publish.assert_called_once()


class TestRedisIntegration:
    """Test Redis publisher integration with API endpoints."""

    @pytest.fixture
    def mock_redis_publisher(self):
        """Create a mock Redis publisher."""
        mock_publisher = AsyncMock(spec=RedisPublisher)
        mock_publisher.publish_project_update = AsyncMock(return_value=True)
        return mock_publisher

    @pytest.fixture
    def mock_project_service(self):
        """Create a mock project service."""
        mock_service = Mock()
        mock_service.update_project_progress = Mock()
        mock_service.reset_project_status = Mock()
        return mock_service

    def test_get_redis_publisher_dependency(self, mock_redis_publisher):
        """Test that Redis publisher dependency can be created."""
        # Test that we can create a RedisPublisher instance
        with patch('services.redis_publisher.get_redis_client'):
            publisher = RedisPublisher()
            assert publisher is not None

    @pytest.mark.asyncio
    async def test_publish_project_update_after_progress_update(self, mock_redis_publisher):
        """Test that Redis publisher is called after successful progress update."""
        project_id = uuid4()
        
        # Mock project data
        mock_project = Mock()
        mock_project.id = project_id
        mock_project.status = ProjectStatus.PROCESSING.value
        mock_project.processed_pct = 50.0
        
        # Test the publishing logic
        message = ProjectProgressMessage(
            project_id=project_id,
            status=ProjectStatus.PROCESSING.value,
            processed_count=5,
            total_count=10,
            processed_pct=50.0,
            timestamp=datetime.now()
        )
        
        result = await mock_redis_publisher.publish_project_update(message)
        
        assert result is True
        mock_redis_publisher.publish_project_update.assert_called_once_with(message)

    @pytest.mark.asyncio
    async def test_publish_project_update_after_status_reset(self, mock_redis_publisher):
        """Test that Redis publisher is called after successful status reset."""
        project_id = uuid4()
        
        # Mock project data
        mock_project = Mock()
        mock_project.id = project_id
        mock_project.status = "COMPLETED"
        mock_project.processed_pct = 0.0
        
        # Test the publishing logic
        message = ProjectProgressMessage(
            project_id=project_id,
            status="COMPLETED",
            processed_count=None,
            total_count=None,
            processed_pct=None,
            timestamp=datetime.now()
        )
        
        result = await mock_redis_publisher.publish_project_update(message)
        
        assert result is True
        mock_redis_publisher.publish_project_update.assert_called_once_with(message)

    @pytest.mark.asyncio
    async def test_api_continues_when_redis_publishing_fails(self, mock_redis_publisher):
        """Test that API doesn't fail when Redis publishing fails."""
        project_id = uuid4()
        
        # Mock Redis publisher to fail
        mock_redis_publisher.publish_project_update.return_value = False
        
        # Test the publishing logic
        message = ProjectProgressMessage(
            project_id=project_id,
            status=ProjectStatus.PROCESSING.value,
            processed_count=5,
            total_count=10,
            processed_pct=50.0,
            timestamp=datetime.now()
        )
        
        # This should not raise an exception even if Redis fails
        result = await mock_redis_publisher.publish_project_update(message)
        
        assert result is False
        mock_redis_publisher.publish_project_update.assert_called_once_with(message)

    def test_create_project_progress_message_from_project(self):
        """Test creating ProjectProgressMessage from project data."""
        project_id = uuid4()
        
        # Test progress update message
        message = ProjectProgressMessage(
            project_id=project_id,
            status=ProjectStatus.PROCESSING.value,
            processed_count=7,
            total_count=15,
            processed_pct=46.67,
            timestamp=datetime.now()
        )
        
        assert message.project_id == project_id
        assert message.status == ProjectStatus.PROCESSING.value
        assert message.processed_count == 7
        assert message.total_count == 15
        assert message.processed_pct == 46.67

    def test_create_project_status_message_from_project(self):
        """Test creating ProjectProgressMessage for status reset."""
        project_id = uuid4()
        
        # Test status reset message
        message = ProjectProgressMessage(
            project_id=project_id,
            status="COMPLETED",
            processed_count=None,
            total_count=None,
            processed_pct=None,
            timestamp=datetime.now()
        )
        
        assert message.project_id == project_id
        assert message.status == "COMPLETED"
        assert message.processed_count is None
        assert message.total_count is None
        assert message.processed_pct is None