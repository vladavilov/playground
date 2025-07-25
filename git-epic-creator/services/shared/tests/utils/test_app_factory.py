"""
Tests for FastAPI application factory.
"""

import pytest
from unittest.mock import Mock, patch
from fastapi import FastAPI
from fastapi.middleware.cors import CORSMiddleware
from fastapi.testclient import TestClient
from fastapi_azure_auth import SingleTenantAzureAuthorizationCodeBearer
from utils.app_factory import FastAPIFactory

def test_create_app_basic():
    """Test basic FastAPI application creation."""
    app = FastAPIFactory.create_app(
        title="Test API",
        description="Test API description",
        version="1.0.0",
        enable_azure_auth=False,
        enable_cors=False
    )
    
    assert isinstance(app, FastAPI)
    assert app.title == "Test API"
    assert app.description == "Test API description"
    assert app.version == "1.0.0"
    
    health_route = next((route for route in app.routes if route.path == "/health"), None)
    version_route = next((route for route in app.routes if route.path == "/version"), None)
    
    assert health_route is not None
    assert version_route is not None

@patch('utils.app_factory.get_postgres_client')
def test_create_app_with_postgres_enabled(mock_get_postgres_client):
    """Test FastAPI application creation with PostgreSQL enabled."""
    mock_postgres_client = Mock()
    mock_get_postgres_client.return_value = mock_postgres_client
    
    with patch('utils.app_factory.PostgresHealthChecker.check_health_with_details') as mock_check_health:
        mock_check_health.return_value = {"healthy": True, "version": "13.3"}
        
        app = FastAPIFactory.create_app(
            title="Test API",
            description="Test API description",
            version="1.0.0",
            enable_postgres=True,
            enable_azure_auth=False,
            enable_cors=False
        )
        
        client = TestClient(app)
        response = client.get("/health/postgres")
        
        assert response.status_code == 200
        assert response.json() == {"healthy": True, "version": "13.3"}
        mock_get_postgres_client.assert_called_once()
        mock_check_health.assert_called_once_with(mock_postgres_client)

@patch('utils.app_factory.get_neo4j_client')
def test_create_app_with_neo4j_enabled(mock_get_neo4j_client):
    """Test FastAPI application creation with Neo4j enabled."""
    mock_neo4j_client = Mock()
    mock_get_neo4j_client.return_value = mock_neo4j_client
    
    with patch('utils.app_factory.Neo4jHealthChecker.check_health_with_details') as mock_check_health:
        mock_check_health.return_value = {"healthy": True, "version": "4.4"}
        
        app = FastAPIFactory.create_app(
            title="Test API",
            description="Test API description",
            version="1.0.0",
            enable_neo4j=True,
            enable_azure_auth=False,
            enable_cors=False
        )
        
        client = TestClient(app)
        response = client.get("/health/neo4j")
        
        assert response.status_code == 200
        assert response.json() == {"healthy": True, "version": "4.4"}
        mock_get_neo4j_client.assert_called_once()
        mock_check_health.assert_called_once_with(mock_neo4j_client)

@pytest.mark.asyncio
@patch('utils.app_factory.get_redis_client')
async def test_create_app_with_redis_enabled(mock_get_redis_client):
    """Test FastAPI application creation with Redis enabled."""
    mock_redis_client = Mock()
    mock_get_redis_client.return_value = mock_redis_client
    
    with patch('utils.app_factory.RedisHealthChecker.check_health_with_details') as mock_check_health:
        # Since check_health_with_details is an async function, the mock needs to be awaitable
        mock_check_health.return_value = {"healthy": True, "version": "6.2"}
        
        app = FastAPIFactory.create_app(
            title="Test API",
            description="Test API description",
            version="1.0.0",
            enable_redis=True,
            enable_azure_auth=False,
            enable_cors=False
        )
        
        client = TestClient(app)
        response = client.get("/health/redis")
        
        assert response.status_code == 200
        assert response.json() == {"healthy": True, "version": "6.2"}
        mock_get_redis_client.assert_called_once()
        # The actual call inside the endpoint is `check_health_with_details`, which is async.
        # The test client will run the event loop.
        mock_check_health.assert_called_once_with(mock_redis_client)





@patch('utils.app_factory.get_azure_auth_settings')
@patch('utils.app_factory.SingleTenantAzureAuthorizationCodeBearer')
@patch('utils.app_factory.AzureAuthMiddleware')
def test_create_app_with_azure_auth(
    mock_azure_middleware_class,
    mock_azure_scheme_class,
    mock_get_azure_settings
):
    """Test FastAPI application creation with Azure AD auth."""
    mock_azure_settings = Mock()
    mock_azure_settings.AZURE_CLIENT_ID = "test-client-id"
    mock_azure_settings.AZURE_TENANT_ID = "test-tenant-id"
    mock_azure_settings.SCOPES = {"api://test": "test"}
    mock_azure_settings.OPENAPI_CLIENT_ID = "test-openapi-client-id"
    mock_get_azure_settings.return_value = mock_azure_settings
    
    mock_azure_scheme = Mock(spec=SingleTenantAzureAuthorizationCodeBearer)
    mock_azure_scheme_class.return_value = mock_azure_scheme
    
    mock_azure_middleware = Mock()
    mock_azure_middleware_class.return_value = mock_azure_middleware
    
    app = FastAPIFactory.create_app(
        title="Test API",
        description="Test API description",
        version="1.0.0",
        enable_azure_auth=True,
        enable_docs_auth=True,
        enable_cors=False
    )
    
    assert isinstance(app, FastAPI)
    
    mock_get_azure_settings.assert_called_once()
    mock_azure_scheme_class.assert_called_once_with(
        app_client_id=mock_azure_settings.AZURE_CLIENT_ID,
        tenant_id=mock_azure_settings.AZURE_TENANT_ID,
        scopes=mock_azure_settings.SCOPES
    )
    mock_azure_middleware_class.assert_called_once_with(mock_azure_scheme)
    
    assert app.swagger_ui_init_oauth == {
        "clientId": mock_azure_settings.OPENAPI_CLIENT_ID,
        "appName": "Test API",
        "usePkceWithAuthorizationCodeGrant": True,
        "scopes": list(mock_azure_settings.SCOPES.keys())
    }

@patch('utils.app_factory.get_azure_auth_settings')
def test_create_app_with_cors(mock_get_azure_settings):
    """Test FastAPI application creation with CORS."""
    mock_azure_settings = Mock()
    mock_azure_settings.BACKEND_CORS_ORIGINS = ["http://localhost:8000"]
    mock_azure_settings.SCOPES = {"api://test": "test"}
    mock_get_azure_settings.return_value = mock_azure_settings
    
    app = FastAPIFactory.create_app(
        title="Test API",
        description="Test API description",
        version="1.0.0",
        enable_azure_auth=True,
        enable_cors=True
    )
    
    assert any(issubclass(middleware.cls, CORSMiddleware) for middleware in app.user_middleware)

def test_create_app_without_azure_auth():
    """Test that the /me endpoint is not available when Azure auth is disabled."""
    app = FastAPIFactory.create_app(
        title="Test API",
        description="Test API description",
        version="1.0.0",
        enable_azure_auth=False
    )
    
    me_route = next((route for route in app.routes if route.path == "/me"), None)
    assert me_route is None
    
def test_health_endpoint():
    """Test health endpoint."""
    app = FastAPIFactory.create_app(
        title="Test API",
        description="Test API description",
        version="1.0.0",
        enable_azure_auth=False,
        enable_cors=False
    )
    
    health_route = next((r for r in app.routes if r.path == "/health"), None)
    assert health_route is not None
    response = health_route.endpoint()
    assert response == {"status": "ok"}

def test_version_endpoint():
    """Test version endpoint."""
    app = FastAPIFactory.create_app(
        title="Test API",
        description="Test API description",
        version="1.0.0",
        enable_azure_auth=False,
        enable_cors=False
    )
    
    version_route = next((r for r in app.routes if r.path == "/version"), None)
    assert version_route is not None
    response = version_route.endpoint()
    assert response == {"version": "1.0.0"}