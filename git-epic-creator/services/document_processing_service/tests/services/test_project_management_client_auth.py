"""
Tests for ProjectManagementClient authentication integration.
"""
import pytest
from unittest.mock import AsyncMock, patch, MagicMock
from uuid import uuid4

from clients.project_management_client import ProjectManagementClient, UpdateProjectStatusResult


class TestProjectManagementClientAuth:
    """Test ProjectManagementClient with Azure authentication."""

    @pytest.fixture
    def mock_settings(self):
        """Mock application settings."""
        settings = MagicMock()
        settings.http_client = MagicMock()
        settings.http_client.PROJECT_MANAGEMENT_SERVICE_URL = "https://test-api.com"
        settings.http_client.CONNECTION_TIMEOUT = 30.0
        settings.http_client.READ_TIMEOUT = 60.0
        settings.http_client.MAX_RETRIES = 3
        settings.http_client.RETRY_BACKOFF_FACTOR = 2.0
        settings.http_client.MAX_CONNECTIONS = 100
        settings.http_client.MAX_KEEPALIVE_CONNECTIONS = 20
        return settings

    @pytest.fixture
    def mock_settings_no_auth(self):
        """Mock application settings with Azure auth disabled."""
        settings = MagicMock()
        settings.http_client = MagicMock()
        settings.http_client.PROJECT_MANAGEMENT_SERVICE_URL = "https://test-api.com"
        settings.http_client.CONNECTION_TIMEOUT = 30.0
        settings.http_client.READ_TIMEOUT = 60.0
        settings.http_client.MAX_RETRIES = 3
        settings.http_client.RETRY_BACKOFF_FACTOR = 2.0
        settings.http_client.MAX_CONNECTIONS = 100
        settings.http_client.MAX_KEEPALIVE_CONNECTIONS = 20
        return settings

    @pytest.mark.asyncio
    async def test_init_succeeds(self, mock_settings):
        """Test initialization without Azure injection path."""
        with patch('clients.project_management_client.get_app_settings', return_value=mock_settings):
            client = ProjectManagementClient()
            assert client._client is None

    @pytest.mark.asyncio
    async def test_context_manager(self, mock_settings):
        with patch('clients.project_management_client.get_app_settings', return_value=mock_settings):
            client = ProjectManagementClient()
            async with client as c:
                assert c == client
                assert client._client is not None

    @pytest.mark.asyncio
    async def test_make_request_passes_headers(self, mock_settings):
        """Client must pass through provided Authorization without overrides."""
        with patch('clients.project_management_client.get_app_settings', return_value=mock_settings):
            client = ProjectManagementClient()
            mock_response = MagicMock()
            mock_response.status_code = 200
            mock_response.json.return_value = {"ok": True}
            mock_response.content = b'{"ok": true}'
            with patch('httpx.AsyncClient') as mock_client_class:
                mock_client_instance = AsyncMock()
                mock_client_instance.request.return_value = mock_response
                mock_client_class.return_value = mock_client_instance
                async with client:
                    response = await client._make_request_with_retry("GET", "/test", headers={"Authorization": "Bearer local"})
                    mock_client_instance.request.assert_called_once()
                    call_args = mock_client_instance.request.call_args
                    assert call_args.kwargs["headers"]["Authorization"] == "Bearer local"

    @pytest.mark.asyncio
    async def test_update_project_status_with_local_token(self, mock_settings):
        project_id = str(uuid4())
        with patch('clients.project_management_client.get_app_settings', return_value=mock_settings):
            client = ProjectManagementClient()
            mock_response = MagicMock()
            mock_response.status_code = 200
            mock_response.json.return_value = {"id": project_id, "status": "processing"}
            mock_response.content = b'{"id": "x"}'
            with patch('httpx.AsyncClient') as mock_client_class:
                mock_client_instance = AsyncMock()
                mock_client_instance.request.return_value = mock_response
                mock_client_class.return_value = mock_client_instance
                async with client:
                    result = await client.update_project_status(project_id=project_id, processed_count=1, total_count=2, authorization_header="Bearer local")
                    assert result.success is True
                    call_args = mock_client_instance.request.call_args
                    assert call_args.kwargs["headers"]["Authorization"] == "Bearer local"

    @pytest.mark.asyncio
    async def test_close(self, mock_settings):
        with patch('clients.project_management_client.get_app_settings', return_value=mock_settings):
            client = ProjectManagementClient()
            async with client:
                pass


    @pytest.mark.asyncio
    async def test_make_request_without_auth_headers(self, mock_settings_no_auth):
        """Test that requests don't include authentication headers when auth is disabled."""
        with patch('clients.project_management_client.get_app_settings', return_value=mock_settings_no_auth):
            client = ProjectManagementClient()
            
            # Mock the HTTP client
            mock_response = MagicMock()
            mock_response.status_code = 200
            mock_response.json.return_value = {"id": "test"}

            with patch('httpx.AsyncClient') as mock_client_class:
                mock_client_instance = AsyncMock()
                mock_client_instance.request.return_value = mock_response
                mock_client_class.return_value = mock_client_instance

                async with client:
                    # Make a request using the private method
                    response = await client._make_request_with_retry("GET", "/test")

                    # Verify no auth header was added
                    mock_client_instance.request.assert_called_once()
                    call_args = mock_client_instance.request.call_args
                    
                    # Headers should not contain Authorization
                    if "headers" in call_args.kwargs:
                        assert "Authorization" not in call_args.kwargs["headers"]


    @pytest.mark.asyncio
    async def test_update_project_status_adds_bearer_prefix_to_raw_token(self, mock_settings):
        """Test that update_project_status adds Bearer prefix when raw token is provided."""
        project_id = str(uuid4())
        raw_token = "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIn0.dozjgNryP4J3jVmNHl0w5N_XgL0n3I9PlFUP0THsR8U"
        
        with patch('clients.project_management_client.get_app_settings', return_value=mock_settings):
            client = ProjectManagementClient()
            mock_response = MagicMock()
            mock_response.status_code = 200
            mock_response.json.return_value = {"id": project_id, "status": "processing"}
            mock_response.content = b'{"id": "x"}'
            
            with patch('httpx.AsyncClient') as mock_client_class:
                mock_client_instance = AsyncMock()
                mock_client_instance.request.return_value = mock_response
                mock_client_class.return_value = mock_client_instance
                
                async with client:
                    result = await client.update_project_status(
                        project_id=project_id,
                        processed_count=1,
                        total_count=2,
                        authorization_header=raw_token
                    )
                    
                    assert result.success is True
                    call_args = mock_client_instance.request.call_args
                    assert "headers" in call_args.kwargs
                    # Verify Bearer prefix was added
                    assert call_args.kwargs["headers"]["Authorization"] == f"Bearer {raw_token}"

    @pytest.mark.asyncio
    async def test_update_project_status_preserves_bearer_prefix_if_present(self, mock_settings):
        """Test that update_project_status does not double-add Bearer prefix."""
        project_id = str(uuid4())
        token_with_bearer = "Bearer eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIn0.dozjgNryP4J3jVmNHl0w5N_XgL0n3I9PlFUP0THsR8U"
        
        with patch('clients.project_management_client.get_app_settings', return_value=mock_settings):
            client = ProjectManagementClient()
            mock_response = MagicMock()
            mock_response.status_code = 200
            mock_response.json.return_value = {"id": project_id, "status": "processing"}
            mock_response.content = b'{"id": "x"}'
            
            with patch('httpx.AsyncClient') as mock_client_class:
                mock_client_instance = AsyncMock()
                mock_client_instance.request.return_value = mock_response
                mock_client_class.return_value = mock_client_instance
                
                async with client:
                    result = await client.update_project_status(
                        project_id=project_id,
                        processed_count=1,
                        total_count=2,
                        authorization_header=token_with_bearer
                    )
                    
                    assert result.success is True
                    call_args = mock_client_instance.request.call_args
                    assert "headers" in call_args.kwargs
                    # Verify Bearer prefix was not duplicated
                    assert call_args.kwargs["headers"]["Authorization"] == token_with_bearer
                    assert not call_args.kwargs["headers"]["Authorization"].startswith("Bearer Bearer")