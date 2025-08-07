"""
Tests for ProjectManagementClient authentication integration.
"""
import pytest
from unittest.mock import AsyncMock, patch, MagicMock
from uuid import uuid4

from services.project_management_client import ProjectManagementClient, UpdateProjectStatusResult


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
        settings.http_client.ENABLE_AZURE_AUTH = True
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
        settings.http_client.ENABLE_AZURE_AUTH = False
        return settings

    @pytest.mark.asyncio
    async def test_init_with_auth_enabled(self, mock_settings):
        """Test initialization with Azure auth enabled."""
        with patch('services.project_management_client.get_app_settings', return_value=mock_settings):
            client = ProjectManagementClient()
            
            assert client.config.ENABLE_AZURE_AUTH is True
            assert client._token_provider is None

    @pytest.mark.asyncio
    async def test_init_with_auth_disabled(self, mock_settings_no_auth):
        """Test initialization with Azure auth disabled."""
        with patch('services.project_management_client.get_app_settings', return_value=mock_settings_no_auth):
            client = ProjectManagementClient()
            
            assert client.config.ENABLE_AZURE_AUTH is False
            assert client._token_provider is None

    @pytest.mark.asyncio
    async def test_context_manager_with_auth(self, mock_settings):
        """Test context manager initialization with authentication."""
        with patch('services.project_management_client.get_app_settings', return_value=mock_settings):
            with patch('services.project_management_client.AzureTokenProvider') as mock_token_provider:
                mock_provider_instance = AsyncMock()
                mock_token_provider.return_value = mock_provider_instance

                client = ProjectManagementClient()
                
                async with client as c:
                    assert c == client
                    assert client._client is not None
                    assert client._token_provider is not None
                    mock_provider_instance.__aenter__.assert_called_once()

    @pytest.mark.asyncio
    async def test_context_manager_without_auth(self, mock_settings_no_auth):
        """Test context manager initialization without authentication."""
        with patch('services.project_management_client.get_app_settings', return_value=mock_settings_no_auth):
            client = ProjectManagementClient()
            
            async with client as c:
                assert c == client
                assert client._client is not None
                assert client._token_provider is None

    @pytest.mark.asyncio
    async def test_close_with_auth(self, mock_settings):
        """Test close method with authentication resources."""
        with patch('services.project_management_client.get_app_settings', return_value=mock_settings):
            with patch('services.project_management_client.AzureTokenProvider') as mock_token_provider:
                mock_provider_instance = AsyncMock()
                mock_token_provider.return_value = mock_provider_instance

                client = ProjectManagementClient()
                
                async with client:
                    pass  # Context manager will call close
                
                mock_provider_instance.close.assert_called_once()

    @pytest.mark.asyncio
    async def test_make_request_with_auth_headers(self, mock_settings):
        """Test that requests include authentication headers when auth is enabled."""
        with patch('services.project_management_client.get_app_settings', return_value=mock_settings):
            with patch('services.project_management_client.AzureTokenProvider') as mock_token_provider:
                mock_provider_instance = AsyncMock()
                mock_provider_instance.get_authorization_header.return_value = {
                    "Authorization": "Bearer test_token"
                }
                mock_token_provider.return_value = mock_provider_instance

                client = ProjectManagementClient()
                
                # Mock the HTTP client
                mock_response = MagicMock()
                mock_response.status_code = 200
                mock_response.json.return_value = {"id": "test"}
                mock_response.content = b'{"id": "test"}'

                with patch('httpx.AsyncClient') as mock_client_class:
                    mock_client_instance = AsyncMock()
                    mock_client_instance.request.return_value = mock_response
                    mock_client_class.return_value = mock_client_instance

                    async with client:
                        # Make a request using the private method to test auth header injection
                        response = await client._make_request_with_retry("GET", "/test")

                        # Verify auth header was added to the request
                        mock_client_instance.request.assert_called_once()
                        call_args = mock_client_instance.request.call_args
                        
                        # Check that headers were passed and include Authorization
                        assert "headers" in call_args.kwargs
                        assert "Authorization" in call_args.kwargs["headers"]
                        assert call_args.kwargs["headers"]["Authorization"] == "Bearer test_token"

    @pytest.mark.asyncio
    async def test_make_request_without_auth_headers(self, mock_settings_no_auth):
        """Test that requests don't include authentication headers when auth is disabled."""
        with patch('services.project_management_client.get_app_settings', return_value=mock_settings_no_auth):
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
    async def test_token_invalidation_on_401(self, mock_settings):
        """Test that token is invalidated when receiving 401 response."""
        with patch('services.project_management_client.get_app_settings', return_value=mock_settings):
            with patch('services.project_management_client.AzureTokenProvider') as mock_token_provider:
                mock_provider_instance = AsyncMock()
                mock_provider_instance.get_authorization_header.return_value = {
                    "Authorization": "Bearer expired_token"
                }
                mock_token_provider.return_value = mock_provider_instance

                client = ProjectManagementClient()
                
                # Mock 401 response
                mock_response = MagicMock()
                mock_response.status_code = 401

                with patch('httpx.AsyncClient') as mock_client_class:
                    mock_client_instance = AsyncMock()
                    mock_client_instance.request.return_value = mock_response
                    mock_client_class.return_value = mock_client_instance

                    async with client:
                        response = await client._make_request_with_retry("GET", "/test")

                        # Verify token was invalidated
                        mock_provider_instance.invalidate_token.assert_called_once()

    @pytest.mark.asyncio
    async def test_auth_token_acquisition_failure(self, mock_settings):
        """Test handling of authentication token acquisition failures."""
        with patch('services.project_management_client.get_app_settings', return_value=mock_settings):
            with patch('services.project_management_client.AzureTokenProvider') as mock_token_provider:
                mock_provider_instance = AsyncMock()
                mock_provider_instance.get_authorization_header.side_effect = Exception("Token acquisition failed")
                mock_token_provider.return_value = mock_provider_instance

                client = ProjectManagementClient()

                async with client:
                    # Token acquisition failure should be re-raised
                    with pytest.raises(Exception) as exc_info:
                        await client._make_request_with_retry("GET", "/test")
                    
                    assert "Token acquisition failed" in str(exc_info.value)

    @pytest.mark.asyncio
    async def test_update_project_status_with_auth(self, mock_settings):
        """Test update_project_status with authentication integration."""
        project_id = str(uuid4())
        
        with patch('services.project_management_client.get_app_settings', return_value=mock_settings):
            with patch('services.project_management_client.AzureTokenProvider') as mock_token_provider:
                mock_provider_instance = AsyncMock()
                mock_provider_instance.get_authorization_header.return_value = {
                    "Authorization": "Bearer test_token"
                }
                mock_token_provider.return_value = mock_provider_instance

                client = ProjectManagementClient()
                
                # Mock successful response
                mock_response = MagicMock()
                mock_response.status_code = 200
                mock_response.json.return_value = {
                    "id": project_id,
                    "status": "processing",
                    "processed_count": 5,
                    "total_count": 10
                }
                mock_response.content = b'{"status": "success"}'

                with patch('httpx.AsyncClient') as mock_client_class:
                    mock_client_instance = AsyncMock()
                    mock_client_instance.request.return_value = mock_response
                    mock_client_class.return_value = mock_client_instance

                    async with client:
                        result = await client.update_project_status(
                            project_id=project_id,
                            processed_count=5,
                            total_count=10
                        )

                        assert result.success is True
                        assert result.status_code == 200
                        
                        # Verify the request was made with auth headers
                        mock_client_instance.request.assert_called_once()
                        call_args = mock_client_instance.request.call_args
                        assert "headers" in call_args.kwargs
                        assert call_args.kwargs["headers"]["Authorization"] == "Bearer test_token"

    @pytest.mark.asyncio
    async def test_header_merging(self, mock_settings):
        """Test that authentication headers are properly merged with existing headers."""
        with patch('services.project_management_client.get_app_settings', return_value=mock_settings):
            with patch('services.project_management_client.AzureTokenProvider') as mock_token_provider:
                mock_provider_instance = AsyncMock()
                mock_provider_instance.get_authorization_header.return_value = {
                    "Authorization": "Bearer test_token"
                }
                mock_token_provider.return_value = mock_provider_instance

                client = ProjectManagementClient()
                
                # Mock the HTTP client
                mock_response = MagicMock()
                mock_response.status_code = 200

                with patch('httpx.AsyncClient') as mock_client_class:
                    mock_client_instance = AsyncMock()
                    mock_client_instance.request.return_value = mock_response
                    mock_client_class.return_value = mock_client_instance

                    async with client:
                        # Make a request with existing headers
                        response = await client._make_request_with_retry(
                            "POST", 
                            "/test",
                            headers={"Content-Type": "application/json"}
                        )

                        # Verify both headers are present
                        mock_client_instance.request.assert_called_once()
                        call_args = mock_client_instance.request.call_args
                        
                        headers = call_args.kwargs["headers"]
                        assert "Authorization" in headers
                        assert "Content-Type" in headers
                        assert headers["Authorization"] == "Bearer test_token"
                        assert headers["Content-Type"] == "application/json"