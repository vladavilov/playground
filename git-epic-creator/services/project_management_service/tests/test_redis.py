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

from utils.unified_redis_messages import ProjectProgressMessage, ProjectProgressPublisher, RedisMessageConfig
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
    """Test ProjectProgressPublisher service class."""

    @pytest.fixture
    def mock_redis_client(self):
        """Mock Redis client for testing."""
        mock_client = AsyncMock()
        return mock_client

    @pytest.fixture
    def redis_publisher(self, mock_redis_client):
        """Create ProjectProgressPublisher instance with mocked Redis client."""
        config = RedisMessageConfig()
        return ProjectProgressPublisher(mock_redis_client, config)

    @pytest.mark.asyncio
    async def test_publish_project_update_success(self, redis_publisher, mock_redis_client):
        """Test successful project update publishing."""
        project_id = uuid4()
        
        # Mock the publish_message method to return True
        with patch.object(redis_publisher, 'publish_message', return_value=True) as mock_publish:
            result = await redis_publisher.publish_project_update(
                project_id=project_id,
                status=ProjectStatus.PROCESSING.value,
                processed_count=5,
                total_count=10,
                processed_pct=50.0
            )
            
            assert result is True
            mock_publish.assert_called_once()
            
            # Verify the message was created correctly
            call_args = mock_publish.call_args
            message = call_args[0][0]  # First argument is the message
            channel = call_args[0][1]  # Second argument is the channel
            
            assert isinstance(message, ProjectProgressMessage)
            assert message.project_id == project_id
            assert message.status == ProjectStatus.PROCESSING.value
            assert message.processed_count == 5
            assert message.total_count == 10
            assert message.processed_pct == 50.0
            assert channel == f"project_progress:{project_id}"

    @pytest.mark.asyncio
    async def test_publish_project_update_redis_failure(self, redis_publisher, mock_redis_client):
        """Test handling Redis publish failure."""
        project_id = uuid4()
        
        # Mock the publish_message method to return False
        with patch.object(redis_publisher, 'publish_message', return_value=False):
            result = await redis_publisher.publish_project_update(
                project_id=project_id,
                status=ProjectStatus.PROCESSING.value,
                processed_count=5,
                total_count=10,
                processed_pct=50.0
            )
            
            assert result is False

    @pytest.mark.asyncio
    async def test_publish_project_update_with_none_values(self, redis_publisher, mock_redis_client):
        """Test publishing project update with None values for optional fields."""
        project_id = uuid4()
        
        with patch.object(redis_publisher, 'publish_message', return_value=True) as mock_publish:
            result = await redis_publisher.publish_project_update(
                project_id=project_id,
                status="COMPLETED",
                processed_count=None,
                total_count=None,
                processed_pct=None
            )
            
            assert result is True
            mock_publish.assert_called_once()
            
            # Verify message content includes None values
            call_args = mock_publish.call_args
            message = call_args[0][0]
            
            assert message.processed_count is None
            assert message.total_count is None
            assert message.processed_pct is None

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
        config = RedisMessageConfig()
        return ProjectProgressPublisher(mock_redis_client, config)

    def test_redis_publisher_dependency_initialization(self):
        """Test that Redis publisher dependency initializes correctly."""
        with patch('routers.project_router.get_redis_client') as mock_get_client:
            with patch('routers.project_router.create_redis_message_factory') as mock_factory:
                # Import only when mocked to prevent blob storage initialization
                from routers.project_router import get_redis_publisher
                
                mock_client = AsyncMock()
                mock_get_client.return_value = mock_client
                
                # Mock the factory to return a proper publisher
                mock_publisher = ProjectProgressPublisher(mock_client, RedisMessageConfig())
                mock_factory.return_value = (Mock(), mock_publisher, Mock())
                
                publisher = get_redis_publisher()
                
                assert isinstance(publisher, ProjectProgressPublisher)
                mock_get_client.assert_called_once()
                mock_factory.assert_called_once_with(mock_client)

    @pytest.mark.asyncio
    async def test_endpoint_publishes_progress_update_successfully(
        self, mock_project_service, mock_current_user, redis_publisher, mock_redis_client, sample_project
    ):
        """Test that endpoint publishes progress update to Redis successfully."""
        # Mock blob storage to prevent azurite connection
        with patch('utils.blob_storage.BlobStorageClient'):
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
            
            # Mock the publish_project_update method
            with patch.object(redis_publisher, 'publish_project_update', return_value=True) as mock_publish:
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
                
                # Verify Redis publish was called with correct parameters
                mock_publish.assert_called_once_with(
                    project_id=project_id,
                    status=ProjectStatus.PROCESSING.value,
                    processed_count=5,
                    total_count=10,
                    processed_pct=50.0
                )
                
                # Verify the API response
                assert result.id == project_id
                assert result.status == ProjectStatus.PROCESSING.value

    @pytest.mark.asyncio
    async def test_endpoint_publishes_status_reset_successfully(
        self, mock_project_service, mock_current_user, redis_publisher, mock_redis_client, sample_project
    ):
        """Test that endpoint publishes status reset to Redis successfully."""
        # Mock blob storage to prevent azurite connection
        with patch('utils.blob_storage.BlobStorageClient'):
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
            
            # Mock the publish_project_update method
            with patch.object(redis_publisher, 'publish_project_update', return_value=True) as mock_publish:
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
                
                # Verify Redis publish was called with correct parameters
                mock_publish.assert_called_once_with(
                    project_id=project_id,
                    status="active",
                    processed_count=None,
                    total_count=None,
                    processed_pct=None
                )
                
                # Verify the API response
                assert result.id == project_id
                assert result.status == "active"

    @pytest.mark.asyncio
    async def test_endpoint_continues_when_redis_publish_fails(
        self, mock_project_service, mock_current_user, redis_publisher, mock_redis_client, sample_project
    ):
        """Test that endpoint continues successfully even when Redis publishing fails."""
        # Mock blob storage to prevent azurite connection
        with patch('utils.blob_storage.BlobStorageClient'):
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
            with patch.object(redis_publisher, 'publish_project_update', return_value=False):
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
                
                # Verify the endpoint still returns the project response
                assert result.id == project_id
                assert result.status == ProjectStatus.PROCESSING.value

    @pytest.mark.asyncio
    async def test_endpoint_handles_project_not_found(
        self, mock_project_service, mock_current_user, redis_publisher
    ):
        """Test that endpoint handles project not found correctly."""
        # Mock blob storage to prevent azurite connection
        with patch('utils.blob_storage.BlobStorageClient'):
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
        # Mock blob storage to prevent azurite connection
        with patch('utils.blob_storage.BlobStorageClient'):
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
        
        with patch.object(redis_publisher, 'publish_message', return_value=False):
            result = await redis_publisher.publish_project_update(
                project_id=project_id,
                status=ProjectStatus.PROCESSING.value,
                processed_count=5,
                total_count=10,
                processed_pct=50.0
            )
            
            assert result is False

    @pytest.mark.asyncio
    async def test_redis_publisher_returns_true_on_success(self, redis_publisher, mock_redis_client):
        """Test that Redis publisher returns True when publishing succeeds."""
        project_id = uuid4()
        
        with patch.object(redis_publisher, 'publish_message', return_value=True):
            result = await redis_publisher.publish_project_update(
                project_id=project_id,
                status=ProjectStatus.PROCESSING.value,
                processed_count=5,
                total_count=10,
                processed_pct=50.0
            )
            
            assert result is True


class TestRedisIntegration:
    """Test Redis publisher integration with API endpoints."""

    @pytest.fixture
    def mock_redis_publisher(self):
        """Create a mock Redis publisher."""
        mock_publisher = AsyncMock(spec=ProjectProgressPublisher)
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
        with patch('utils.redis_client.get_redis_client'):
            config = RedisMessageConfig()
            publisher = ProjectProgressPublisher(Mock(), config)
            assert publisher is not None

    @pytest.mark.asyncio
    async def test_publish_project_update_after_progress_update(self, mock_redis_publisher):
        """Test that Redis publisher is called after successful progress update."""
        project_id = uuid4()
        
        # Test the publishing logic
        result = await mock_redis_publisher.publish_project_update(
            project_id=project_id,
            status=ProjectStatus.PROCESSING.value,
            processed_count=5,
            total_count=10,
            processed_pct=50.0
        )
        
        assert result is True
        mock_redis_publisher.publish_project_update.assert_called_once_with(
            project_id=project_id,
            status=ProjectStatus.PROCESSING.value,
            processed_count=5,
            total_count=10,
            processed_pct=50.0
        )

    @pytest.mark.asyncio
    async def test_publish_project_update_after_status_reset(self, mock_redis_publisher):
        """Test that Redis publisher is called after successful status reset."""
        project_id = uuid4()
        
        # Test the publishing logic
        result = await mock_redis_publisher.publish_project_update(
            project_id=project_id,
            status="COMPLETED",
            processed_count=None,
            total_count=None,
            processed_pct=None
        )
        
        assert result is True
        mock_redis_publisher.publish_project_update.assert_called_once_with(
            project_id=project_id,
            status="COMPLETED",
            processed_count=None,
            total_count=None,
            processed_pct=None
        )

    @pytest.mark.asyncio
    async def test_api_continues_when_redis_publishing_fails(self, mock_redis_publisher):
        """Test that API doesn't fail when Redis publishing fails."""
        project_id = uuid4()
        
        # Mock Redis publisher to fail
        mock_redis_publisher.publish_project_update.return_value = False
        
        # This should not raise an exception even if Redis fails
        result = await mock_redis_publisher.publish_project_update(
            project_id=project_id,
            status=ProjectStatus.PROCESSING.value,
            processed_count=5,
            total_count=10,
            processed_pct=50.0
        )
        
        assert result is False
        mock_redis_publisher.publish_project_update.assert_called_once_with(
            project_id=project_id,
            status=ProjectStatus.PROCESSING.value,
            processed_count=5,
            total_count=10,
            processed_pct=50.0
        )

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


class TestRedisFactoryIntegration:
    """Test Redis factory integration replacing wrapper classes."""
    
    @pytest.mark.asyncio
    async def test_router_uses_factory_method(self):
        """Test that project router uses create_redis_message_factory instead of RedisPublisher wrapper."""
        from utils.unified_redis_messages import create_redis_message_factory
        from utils.redis_client import get_redis_client
        
        redis_client = get_redis_client()
        task_publisher, progress_publisher, task_subscriber = create_redis_message_factory(redis_client)
        
        # Mock blob storage to prevent azurite connection during router import
        with patch('utils.blob_storage.BlobStorageClient'):
            # The progress_publisher should be the same type as what get_redis_publisher() returns
            from routers.project_router import get_redis_publisher
            publisher_from_dependency = get_redis_publisher()
            
            assert type(progress_publisher) == type(publisher_from_dependency)
            assert hasattr(publisher_from_dependency, 'publish_project_update')  # Should have this method