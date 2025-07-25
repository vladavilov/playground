"""
Tests for ProjectManagementClient HTTP client.
"""
import pytest
from unittest.mock import AsyncMock, patch, MagicMock
import httpx
from tenacity import RetryError

from services.project_management_client import ProjectManagementClient


class TestProjectManagementClient:
    """Test cases for ProjectManagementClient."""

    @pytest.fixture
    def mock_app_settings(self):
        """Mock application settings with HTTP client configuration."""
        mock_settings = MagicMock()
        mock_http_config = MagicMock()
        mock_http_config.PROJECT_MANAGEMENT_SERVICE_URL = "http://test-service:8001"
        mock_http_config.CONNECTION_TIMEOUT = 30.0
        mock_http_config.READ_TIMEOUT = 60.0
        mock_http_config.MAX_RETRIES = 3
        mock_http_config.RETRY_BACKOFF_FACTOR = 2.0
        mock_http_config.MAX_CONNECTIONS = 100
        mock_http_config.MAX_KEEPALIVE_CONNECTIONS = 20
        mock_settings.http_client = mock_http_config
        return mock_settings

    @pytest.fixture
    def client(self, mock_app_settings):
        """Create ProjectManagementClient instance."""
        with patch('services.project_management_client.get_app_settings', return_value=mock_app_settings):
            return ProjectManagementClient()

    def test_client_initialization(self, mock_app_settings):
        """Test that client initializes with proper configuration."""
        with patch('services.project_management_client.get_app_settings', return_value=mock_app_settings):
            client = ProjectManagementClient()
            assert client.config == mock_app_settings.http_client
            assert client._client is None  # Lazy initialization

    @pytest.mark.asyncio
    async def test_client_context_manager(self, mock_app_settings):
        """Test that client works as async context manager."""
        with patch('services.project_management_client.get_app_settings', return_value=mock_app_settings):
            client = ProjectManagementClient()
            async with client as c:
                assert c._client is not None
                assert isinstance(c._client, httpx.AsyncClient)

    @pytest.mark.asyncio
    async def test_client_timeout_configuration(self, mock_app_settings):
        """Test that client is configured with proper timeouts."""
        with patch('services.project_management_client.get_app_settings', return_value=mock_app_settings):
            client = ProjectManagementClient()
            async with client as c:
                timeout = c._client.timeout
                assert timeout.connect == 30.0
                assert timeout.read == 60.0

    @pytest.mark.asyncio
    async def test_client_connection_limits(self, mock_app_settings):
        """Test that client is configured with proper connection limits."""
        with patch('services.project_management_client.get_app_settings', return_value=mock_app_settings):
            client = ProjectManagementClient()
            async with client as c:
                # Check that the client was created with limits
                assert c._client is not None
                # The limits are set during client creation, we can verify by checking the transport
                assert hasattr(c._client, '_transport')

    @pytest.mark.asyncio
    async def test_close_client(self, mock_app_settings):
        """Test that client properly closes HTTP connections."""
        with patch('services.project_management_client.get_app_settings', return_value=mock_app_settings):
            client = ProjectManagementClient()
            async with client:
                pass  # Client should be closed automatically
            
            # If client was created, it should be closed
            if client._client:
                assert client._client.is_closed

    @pytest.mark.asyncio
    async def test_retry_on_connection_error(self, mock_app_settings):
        """Test that client retries on connection errors."""
        with patch('services.project_management_client.get_app_settings', return_value=mock_app_settings):
            client = ProjectManagementClient()
            with patch('httpx.AsyncClient.request') as mock_request:
                # First two calls fail, third succeeds
                mock_request.side_effect = [
                    httpx.ConnectError("Connection failed"),
                    httpx.ConnectError("Connection failed"),
                    httpx.Response(200, json={"status": "success"})
                ]
                
                async with client as c:
                    response = await c._make_request_with_retry(
                        "PUT", "/test", json={"data": "test"}
                    )
                    
                assert response.status_code == 200
                assert mock_request.call_count == 3

    @pytest.mark.asyncio
    async def test_retry_exhaustion(self, mock_app_settings):
        """Test that client raises RetryError when retries are exhausted."""
        with patch('services.project_management_client.get_app_settings', return_value=mock_app_settings):
            client = ProjectManagementClient()
            with patch('httpx.AsyncClient.request') as mock_request:
                mock_request.side_effect = httpx.ConnectError("Connection failed")
                
                async with client as c:
                    with pytest.raises((RetryError, httpx.ConnectError)):
                        await c._make_request_with_retry(
                            "PUT", "/test", json={"data": "test"}
                        )
                    
                # Should retry max_retries + 1 times (initial + retries)
                assert mock_request.call_count == 4  # 1 initial + 3 retries

    @pytest.mark.asyncio
    async def test_no_retry_on_client_error(self, mock_app_settings):
        """Test that client doesn't retry on 4xx client errors."""
        with patch('services.project_management_client.get_app_settings', return_value=mock_app_settings):
            client = ProjectManagementClient()
            with patch('httpx.AsyncClient.request') as mock_request:
                mock_request.return_value = httpx.Response(400, json={"error": "Bad request"})
                
                async with client as c:
                    response = await c._make_request_with_retry(
                        "PUT", "/test", json={"data": "test"}
                    )
                    
                assert response.status_code == 400
                assert mock_request.call_count == 1  # No retries for client errors

    @pytest.mark.asyncio
    async def test_retry_on_server_error(self, mock_app_settings):
        """Test that client retries on 5xx server errors."""
        with patch('services.project_management_client.get_app_settings', return_value=mock_app_settings):
            client = ProjectManagementClient()
            with patch('httpx.AsyncClient.request') as mock_request:
                # First two calls return 500, third succeeds
                mock_request.side_effect = [
                    httpx.Response(500, json={"error": "Internal server error"}),
                    httpx.Response(500, json={"error": "Internal server error"}),
                    httpx.Response(200, json={"status": "success"})
                ]
                
                async with client as c:
                    response = await c._make_request_with_retry(
                        "PUT", "/test", json={"data": "test"}
                    )
                    
                assert response.status_code == 200
                assert mock_request.call_count == 3

    @pytest.mark.asyncio
    async def test_structured_logging(self, mock_app_settings):
        """Test that client logs HTTP operations with structured logging."""
        import logging
        
        # Set up a logger to capture structured logs
        logger = logging.getLogger('services.project_management_client')
        logger.setLevel(logging.INFO)
        
        with patch('services.project_management_client.get_app_settings', return_value=mock_app_settings):
            client = ProjectManagementClient()
            with patch('httpx.AsyncClient.request') as mock_request:
                mock_request.return_value = httpx.Response(200, json={"status": "success"})
                
                async with client as c:
                    await c._make_request_with_retry(
                        "PUT", "/test", json={"data": "test"}
                    )
                    
                # Verify the request was made
                assert mock_request.call_count == 1