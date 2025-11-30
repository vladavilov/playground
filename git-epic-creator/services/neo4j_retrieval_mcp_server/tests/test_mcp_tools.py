"""Tests for MCP server tools."""

import pytest
from unittest.mock import AsyncMock, patch, MagicMock


@pytest.fixture(autouse=True)
def mock_shared_settings():
    """Mock shared library settings for all tests."""
    mock_http_settings = MagicMock()
    mock_http_settings.PROJECT_MANAGEMENT_SERVICE_URL = "http://test-pms:8000"
    mock_http_settings.GRAPH_RAG_SERVICE_URL = "http://test-retrieval:8000"
    mock_http_settings.CONNECTION_TIMEOUT = 30.0
    mock_http_settings.READ_TIMEOUT = 120.0
    mock_http_settings.MAX_RETRIES = 3
    mock_http_settings.RETRY_BACKOFF_FACTOR = 2.0
    mock_http_settings.MAX_CONNECTIONS = 100
    mock_http_settings.MAX_KEEPALIVE_CONNECTIONS = 20

    mock_redis_settings = MagicMock()
    mock_redis_settings.REDIS_URL = "redis://localhost:6379"
    mock_redis_settings.REDIS_PASSWORD = None
    mock_redis_settings.REDIS_DB = 0
    mock_redis_settings.REDIS_MAX_CONNECTIONS = 10
    mock_redis_settings.REDIS_SOCKET_CONNECT_TIMEOUT = 5.0
    mock_redis_settings.REDIS_SOCKET_TIMEOUT = 5.0

    mock_app_settings = MagicMock()
    mock_app_settings.http_client = mock_http_settings
    mock_app_settings.redis = mock_redis_settings
    mock_app_settings.API_PORT = 8000

    with patch("configuration.common_config.get_app_settings", return_value=mock_app_settings):
        with patch("config.get_app_settings", return_value=mock_app_settings):
            yield mock_app_settings


class TestResolveProject:
    """Tests for resolve_project tool."""

    @pytest.mark.asyncio
    async def test_resolve_project_single_match(self, mock_shared_settings):
        """Test successful project resolution with single match."""
        from adapter import ProjectManagementAdapter
        
        with patch.object(ProjectManagementAdapter, "_make_request") as mock_request:
            mock_response = MagicMock()
            mock_response.status_code = 200
            mock_response.json.return_value = [
                {"id": "uuid-123", "name": "Billing System"}
            ]
            mock_request.return_value = mock_response
            
            adapter = ProjectManagementAdapter()
            result = await adapter.search_projects("billing")
            
            assert result["success"] is True
            assert result["project_id"] == "uuid-123"
            assert result["project_name"] == "Billing System"

    @pytest.mark.asyncio
    async def test_resolve_project_not_found(self, mock_shared_settings):
        """Test project resolution with no matches."""
        from adapter import ProjectManagementAdapter
        
        with patch.object(ProjectManagementAdapter, "_make_request") as mock_request:
            mock_response = MagicMock()
            mock_response.status_code = 200
            mock_response.json.return_value = []
            mock_request.return_value = mock_response
            
            adapter = ProjectManagementAdapter()
            result = await adapter.search_projects("nonexistent")
            
            assert result["success"] is False
            assert result["error"] == "not_found"
            assert "nonexistent" in result["message"]

    @pytest.mark.asyncio
    async def test_resolve_project_ambiguous(self, mock_shared_settings):
        """Test project resolution with multiple matches."""
        from adapter import ProjectManagementAdapter
        
        with patch.object(ProjectManagementAdapter, "_make_request") as mock_request:
            mock_response = MagicMock()
            mock_response.status_code = 200
            mock_response.json.return_value = [
                {"id": "uuid-1", "name": "Billing Core"},
                {"id": "uuid-2", "name": "Billing Analytics"},
            ]
            mock_request.return_value = mock_response
            
            adapter = ProjectManagementAdapter()
            result = await adapter.search_projects("billing")
            
            assert result["success"] is False
            assert result["error"] == "ambiguous_results"
            assert len(result["matches"]) == 2
            assert "Billing Core" in result["message"]
            assert "Billing Analytics" in result["message"]

    @pytest.mark.asyncio
    async def test_resolve_project_auth_failure(self, mock_shared_settings):
        """Test project resolution with authentication failure."""
        from adapter import ProjectManagementAdapter
        
        with patch.object(ProjectManagementAdapter, "_make_request") as mock_request:
            mock_response = MagicMock()
            mock_response.status_code = 401
            mock_request.return_value = mock_response
            
            adapter = ProjectManagementAdapter()
            result = await adapter.search_projects("billing")
            
            assert result["success"] is False
            assert result["error"] == "authentication_failed"


class TestRetrieveContext:
    """Tests for retrieve_context tool."""

    @pytest.mark.asyncio
    async def test_retrieve_context_success(self, mock_shared_settings):
        """Test successful context retrieval."""
        from adapter import RetrievalServiceAdapter
        
        with patch.object(RetrievalServiceAdapter, "_make_request") as mock_request:
            mock_response = MagicMock()
            mock_response.status_code = 200
            mock_response.json.return_value = {
                "final_answer": "Authentication uses JWT tokens...",
                "key_facts": [
                    {
                        "fact": "JWT tokens expire after 24 hours",
                        "citations": [
                            {
                                "chunk_id": "chunk-123",
                                "document_name": "auth.md",
                                "span": "Tokens expire..."
                            }
                        ]
                    }
                ],
                "residual_uncertainty": "",
                "no_data_found": False
            }
            mock_request.return_value = mock_response
            
            adapter = RetrievalServiceAdapter()
            result = await adapter.retrieve(
                query="How does authentication work?",
                project_id="uuid-123",
                top_k=5
            )
            
            assert result["final_answer"] == "Authentication uses JWT tokens..."
            assert len(result["key_facts"]) == 1
            assert result["no_data_found"] is False

    @pytest.mark.asyncio
    async def test_retrieve_context_no_data(self, mock_shared_settings):
        """Test retrieval with no data found."""
        from adapter import RetrievalServiceAdapter
        
        with patch.object(RetrievalServiceAdapter, "_make_request") as mock_request:
            mock_response = MagicMock()
            mock_response.status_code = 200
            mock_response.json.return_value = {
                "final_answer": "",
                "key_facts": [],
                "residual_uncertainty": "",
                "no_data_found": True
            }
            mock_request.return_value = mock_response
            
            adapter = RetrievalServiceAdapter()
            result = await adapter.retrieve(
                query="Obscure question",
                project_id="uuid-123"
            )
            
            assert result["no_data_found"] is True
            assert result["final_answer"] == ""

    @pytest.mark.asyncio
    async def test_retrieve_context_service_error(self, mock_shared_settings):
        """Test retrieval with service error."""
        from adapter import RetrievalServiceAdapter
        import httpx
        
        with patch.object(RetrievalServiceAdapter, "_make_request") as mock_request:
            mock_response = MagicMock()
            mock_response.status_code = 500
            error = httpx.HTTPStatusError(
                "Server error",
                request=MagicMock(),
                response=mock_response
            )
            mock_request.side_effect = error
            
            adapter = RetrievalServiceAdapter()
            result = await adapter.retrieve(
                query="Any question",
                project_id="uuid-123"
            )
            
            assert result["no_data_found"] is True
            assert "HTTP 500" in result["residual_uncertainty"]


class TestConfig:
    """Tests for configuration."""

    def test_mcp_settings_defaults(self, mock_shared_settings):
        """Test default MCP settings values."""
        from config import get_mcp_settings
        
        # Clear cache for fresh settings
        get_mcp_settings.cache_clear()
        
        settings = get_mcp_settings()
        assert settings.PORT == 8082
        assert settings.MCP_TRANSPORT == "stdio"

    def test_get_service_urls(self, mock_shared_settings):
        """Test service URL retrieval from shared config."""
        from config import get_project_management_url, get_retrieval_service_url
        
        pms_url = get_project_management_url()
        retrieval_url = get_retrieval_service_url()
        
        assert pms_url == "http://test-pms:8000"
        assert retrieval_url == "http://test-retrieval:8000"

    def test_http_client_settings(self, mock_shared_settings):
        """Test HTTP client settings from shared config."""
        from config import get_http_client_settings
        
        settings = get_http_client_settings()
        
        assert settings.CONNECTION_TIMEOUT == 30.0
        assert settings.READ_TIMEOUT == 120.0
        assert settings.MAX_RETRIES == 3
