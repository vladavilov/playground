"""
Tests for update_project_status method in ProjectManagementClient.
"""
import pytest
from unittest.mock import AsyncMock, patch, MagicMock
import httpx
from uuid import uuid4

from services.project_management_client import ProjectManagementClient


class TestUpdateProjectStatus:
    """Test cases for update_project_status method."""

    @pytest.fixture
    def mock_app_settings(self):
        """Mock application settings."""
        settings = MagicMock()
        http_client_config = MagicMock()
        http_client_config.PROJECT_MANAGEMENT_SERVICE_URL = "http://test-service:8001"
        http_client_config.CONNECTION_TIMEOUT = 30.0
        http_client_config.READ_TIMEOUT = 60.0
        http_client_config.MAX_RETRIES = 3
        http_client_config.RETRY_BACKOFF_FACTOR = 2.0
        http_client_config.MAX_CONNECTIONS = 100
        http_client_config.MAX_KEEPALIVE_CONNECTIONS = 20
        settings.http_client = http_client_config
        return settings

    @pytest.fixture
    def client(self, mock_app_settings):
        """Create ProjectManagementClient instance."""
        with patch('services.project_management_client.get_app_settings', return_value=mock_app_settings):
            return ProjectManagementClient()

    @pytest.mark.asyncio
    async def test_update_project_progress(self, client):
        """Test updating project progress with processed count and total count."""
        project_id = str(uuid4())
        
        with patch('httpx.AsyncClient.request') as mock_request:
            mock_request.return_value = httpx.Response(200, json={"status": "success"})
            
            async with client as c:
                result = await c.update_project_status(
                    project_id=project_id,
                    processed_count=5,
                    total_count=10
                )
                
            assert result.success is True
            assert result.status_code == 200
            
            # Verify the correct endpoint was called
            mock_request.assert_called_once()
            call_args = mock_request.call_args
            assert call_args[0][0] == "PUT"  # HTTP method
            assert f"/projects/{project_id}/status" in call_args[0][1]  # endpoint
            
            # Verify the request payload
            request_json = call_args[1]["json"]
            assert request_json["processed_count"] == 5
            assert request_json["total_count"] == 10
            assert request_json["processed_pct"] == 50.0
            assert request_json["status"] == "processing"

    @pytest.mark.asyncio
    async def test_update_project_completion(self, client):
        """Test updating project status when processing is complete."""
        project_id = str(uuid4())
        
        with patch('httpx.AsyncClient.request') as mock_request:
            mock_request.return_value = httpx.Response(200, json={"status": "success"})
            
            async with client as c:
                result = await c.update_project_status(
                    project_id=project_id,
                    processed_count=10,
                    total_count=10
                )
                
            assert result.success is True
            assert result.status_code == 200
            
            # Verify the request payload for completion
            call_args = mock_request.call_args
            request_json = call_args[1]["json"]
            assert request_json["processed_count"] == 10
            assert request_json["total_count"] == 10
            assert request_json["processed_pct"] == 100.0
            assert request_json["status"] == "active"  # Status changes to active when complete

    @pytest.mark.asyncio
    async def test_update_project_status_reset(self, client):
        """Test resetting project status without progress counts."""
        project_id = str(uuid4())
        
        with patch('httpx.AsyncClient.request') as mock_request:
            mock_request.return_value = httpx.Response(200, json={"status": "success"})
            
            async with client as c:
                result = await c.update_project_status(
                    project_id=project_id,
                    status="active"
                )
                
            assert result.success is True
            assert result.status_code == 200
            
            # Verify the request payload for status reset
            call_args = mock_request.call_args
            request_json = call_args[1]["json"]
            assert request_json["status"] == "active"
            assert "processed_count" not in request_json
            assert "total_count" not in request_json
            assert "processed_pct" not in request_json

    @pytest.mark.asyncio
    async def test_update_project_status_validation_error(self, client):
        """Test validation error when neither progress nor status is provided."""
        project_id = str(uuid4())
        
        async with client as c:
            with pytest.raises(ValueError, match="Either progress counts or status must be provided"):
                await c.update_project_status(project_id=project_id)

    @pytest.mark.asyncio
    async def test_update_project_status_invalid_progress(self, client):
        """Test validation error for invalid progress counts."""
        project_id = str(uuid4())
        
        async with client as c:
            with pytest.raises(ValueError, match="processed_count cannot be greater than total_count"):
                await c.update_project_status(
                    project_id=project_id,
                    processed_count=15,
                    total_count=10
                )

    @pytest.mark.asyncio
    async def test_update_project_status_negative_counts(self, client):
        """Test validation error for negative counts."""
        project_id = str(uuid4())
        
        async with client as c:
            with pytest.raises(ValueError, match="Counts must be non-negative"):
                await c.update_project_status(
                    project_id=project_id,
                    processed_count=-1,
                    total_count=10
                )

    @pytest.mark.asyncio
    async def test_update_project_status_http_error(self, client):
        """Test handling of HTTP errors."""
        project_id = str(uuid4())
        
        with patch('httpx.AsyncClient.request') as mock_request:
            mock_request.return_value = httpx.Response(400, json={"error": "Bad request"})
            
            async with client as c:
                result = await c.update_project_status(
                    project_id=project_id,
                    processed_count=5,
                    total_count=10
                )
                
            assert result.success is False
            assert result.status_code == 400
            assert "Bad request" in result.error_message

    @pytest.mark.asyncio
    async def test_update_project_status_connection_error(self, client):
        """Test handling of connection errors with retry."""
        project_id = str(uuid4())
        
        with patch('httpx.AsyncClient.request') as mock_request:
            # First call fails, second succeeds
            mock_request.side_effect = [
                httpx.ConnectError("Connection failed"),
                httpx.Response(200, json={"status": "success"})
            ]
            
            async with client as c:
                result = await c.update_project_status(
                    project_id=project_id,
                    processed_count=5,
                    total_count=10
                )
                
            assert result.success is True
            assert result.status_code == 200
            assert mock_request.call_count == 2  # Retry worked

    @pytest.mark.asyncio
    async def test_update_project_status_invalid_uuid(self, client):
        """Test validation error for invalid project UUID."""
        async with client as c:
            with pytest.raises(ValueError, match="Invalid project_id format"):
                await c.update_project_status(
                    project_id="not-a-uuid",
                    processed_count=5,
                    total_count=10
                )