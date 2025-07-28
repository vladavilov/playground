"""
Tests for FastAPI application factory.
"""

import pytest
from unittest.mock import Mock, patch, AsyncMock
from fastapi import FastAPI
from fastapi.middleware.cors import CORSMiddleware
from fastapi.testclient import TestClient
from fastapi_azure_auth import MultiTenantAzureAuthorizationCodeBearer
from utils.app_factory import FastAPIFactory


class TestFastAPIFactory:
    """Test cases for FastAPIFactory."""

    def test_create_app_basic_configuration(self):
        """Test basic FastAPI application creation without optional features."""
        # Arrange
        title = "Test API"
        description = "Test API description"
        version = "1.0.0"
        
        # Act
        app = FastAPIFactory.create_app(
            title=title,
            description=description,
            version=version,
            enable_azure_auth=False,
            enable_cors=False,
            enable_postgres=False,
            enable_neo4j=False,
            enable_redis=False
        )
        
        # Assert
        assert isinstance(app, FastAPI)
        assert app.title == title
        assert app.description == description
        assert app.version == version
        
        # Check required routes exist
        route_paths = [route.path for route in app.routes]
        assert "/health" in route_paths
        assert "/version" in route_paths
        assert "/me" not in route_paths  # Should not exist when auth is disabled

    def test_create_app_health_endpoint(self):
        """Test that health endpoint returns correct response."""
        # Arrange
        app = FastAPIFactory.create_app(
            title="Test API",
            description="Test description",
            version="1.0.0",
            enable_azure_auth=False,
            enable_cors=False
        )
        client = TestClient(app)
        
        # Act
        response = client.get("/health")
        
        # Assert
        assert response.status_code == 200
        assert response.json() == {"status": "ok"}

    def test_create_app_version_endpoint(self):
        """Test that version endpoint returns correct response."""
        # Arrange
        version = "2.1.0"
        app = FastAPIFactory.create_app(
            title="Test API",
            description="Test description",
            version=version,
            enable_azure_auth=False,
            enable_cors=False
        )
        client = TestClient(app)
        
        # Act
        response = client.get("/version")
        
        # Assert
        assert response.status_code == 200
        assert response.json() == {"version": version}

    @patch('utils.app_factory.get_postgres_client')
    @patch('utils.app_factory.PostgresHealthChecker.check_health_with_details')
    def test_create_app_with_postgres_enabled(self, mock_check_health, mock_get_postgres_client):
        """Test FastAPI application creation with PostgreSQL enabled."""
        # Arrange
        mock_postgres_client = Mock()
        mock_get_postgres_client.return_value = mock_postgres_client
        mock_health_response = {"healthy": True, "version": "13.3"}
        mock_check_health.return_value = mock_health_response
        
        # Act
        app = FastAPIFactory.create_app(
            title="Test API",
            description="Test API description",
            version="1.0.0",
            enable_postgres=True,
            enable_azure_auth=False,
            enable_cors=False
        )
        
        # Assert
        assert hasattr(app.state, 'postgres_client')
        assert app.state.postgres_client == mock_postgres_client
        
        # Test health endpoint
        client = TestClient(app)
        response = client.get("/health/postgres")
        
        assert response.status_code == 200
        assert response.json() == mock_health_response
        mock_get_postgres_client.assert_called_once()
        mock_check_health.assert_called_once_with(mock_postgres_client)

    @patch('utils.app_factory.get_neo4j_client')
    @patch('utils.app_factory.Neo4jHealthChecker.check_health_with_details')
    def test_create_app_with_neo4j_enabled(self, mock_check_health, mock_get_neo4j_client):
        """Test FastAPI application creation with Neo4j enabled."""
        # Arrange
        mock_neo4j_client = Mock()
        mock_get_neo4j_client.return_value = mock_neo4j_client
        mock_health_response = {"healthy": True, "version": "4.4"}
        mock_check_health.return_value = mock_health_response
        
        # Act
        app = FastAPIFactory.create_app(
            title="Test API",
            description="Test API description",
            version="1.0.0",
            enable_neo4j=True,
            enable_azure_auth=False,
            enable_cors=False
        )
        
        # Assert
        assert hasattr(app.state, 'neo4j_client')
        assert app.state.neo4j_client == mock_neo4j_client
        
        # Test health endpoint
        client = TestClient(app)
        response = client.get("/health/neo4j")
        
        assert response.status_code == 200
        assert response.json() == mock_health_response
        mock_get_neo4j_client.assert_called_once()
        mock_check_health.assert_called_once_with(mock_neo4j_client)

    @patch('utils.app_factory.get_redis_client')
    @patch('utils.app_factory.RedisHealthChecker.check_health_with_details')
    def test_create_app_with_redis_enabled(self, mock_check_health, mock_get_redis_client):
        """Test FastAPI application creation with Redis enabled."""
        # Arrange
        mock_redis_client = Mock()
        mock_get_redis_client.return_value = mock_redis_client
        mock_health_response = {"healthy": True, "version": "6.2"}
        mock_check_health.return_value = mock_health_response
        
        # Act
        app = FastAPIFactory.create_app(
            title="Test API",
            description="Test API description",
            version="1.0.0",
            enable_redis=True,
            enable_azure_auth=False,
            enable_cors=False
        )
        
        # Assert
        assert hasattr(app.state, 'redis_client')
        assert app.state.redis_client == mock_redis_client
        
        # Test health endpoint
        client = TestClient(app)
        response = client.get("/health/redis")
        
        assert response.status_code == 200
        assert response.json() == mock_health_response
        mock_get_redis_client.assert_called_once()
        mock_check_health.assert_called_once_with(mock_redis_client)





    @patch('utils.app_factory.get_azure_auth_settings')
    @patch('utils.app_factory.MultiTenantAzureAuthorizationCodeBearer')
    @patch('utils.app_factory.AzureAuthMiddleware')
    def test_create_app_with_azure_auth(
        self,
        mock_azure_middleware_class,
        mock_azure_scheme_class,
        mock_get_azure_settings
    ):
        """Test FastAPI application creation with Azure AD authentication."""
        # Arrange
        mock_azure_settings = Mock()
        mock_azure_settings.AZURE_CLIENT_ID = "test-client-id"
        mock_azure_settings.AZURE_TENANT_ID = "test-tenant-id"
        mock_azure_settings.SCOPES = {"api://test": "test"}
        mock_azure_settings.OPENAPI_CLIENT_ID = "test-openapi-client-id"
        mock_azure_settings.OPENAPI_AUTHORIZATION_URL = "https://login.microsoftonline.com/test-tenant/oauth2/v2.0/authorize"
        mock_azure_settings.OPENAPI_TOKEN_URL = "https://login.microsoftonline.com/test-tenant/oauth2/v2.0/token"
        mock_get_azure_settings.return_value = mock_azure_settings
        
        mock_azure_scheme = Mock()
        mock_azure_scheme_class.return_value = mock_azure_scheme
        
        mock_azure_middleware = Mock()
        mock_azure_middleware_class.return_value = mock_azure_middleware
        
        # Act
        app = FastAPIFactory.create_app(
            title="Test API",
            description="Test API description",
            version="1.0.0",
            enable_azure_auth=True,
            enable_docs_auth=True,
            enable_cors=False
        )
        
        # Assert
        assert isinstance(app, FastAPI)
        
        mock_get_azure_settings.assert_called_once()
        mock_azure_scheme_class.assert_called_once_with(
            app_client_id=mock_azure_settings.AZURE_CLIENT_ID,
            scopes=mock_azure_settings.SCOPES,
            openapi_authorization_url=mock_azure_settings.OPENAPI_AUTHORIZATION_URL,
            openapi_token_url=mock_azure_settings.OPENAPI_TOKEN_URL,
            validate_iss=False
        )
        mock_azure_middleware_class.assert_called_once_with(mock_azure_scheme)
        
        # Check OAuth configuration
        assert app.swagger_ui_init_oauth == {
            "clientId": mock_azure_settings.OPENAPI_CLIENT_ID,
            "appName": "Test API",
            "usePkceWithAuthorizationCodeGrant": True,
            "scopes": list(mock_azure_settings.SCOPES.keys())
        }
        
        # Check /me endpoint exists
        route_paths = [route.path for route in app.routes]
        assert "/me" in route_paths

    @patch('utils.app_factory.get_azure_auth_settings')
    @patch('utils.app_factory.MultiTenantAzureAuthorizationCodeBearer')
    @patch('utils.app_factory.AzureAuthMiddleware')
    def test_create_app_with_cors(self, mock_azure_middleware_class, mock_azure_scheme_class, mock_get_azure_settings):
        """Test FastAPI application creation with CORS enabled."""
        # Arrange
        mock_azure_settings = Mock()
        mock_azure_settings.BACKEND_CORS_ORIGINS = ["http://localhost:8000", "http://localhost:3000"]
        mock_azure_settings.SCOPES = {"api://test": "test"}
        mock_azure_settings.AZURE_CLIENT_ID = "test-client-id"
        mock_azure_settings.AZURE_TENANT_ID = "test-tenant-id"
        mock_azure_settings.OPENAPI_CLIENT_ID = "test-openapi-client-id"
        mock_azure_settings.OPENAPI_AUTHORIZATION_URL = "https://login.microsoftonline.com/test-tenant/oauth2/v2.0/authorize"
        mock_azure_settings.OPENAPI_TOKEN_URL = "https://login.microsoftonline.com/test-tenant/oauth2/v2.0/token"
        mock_get_azure_settings.return_value = mock_azure_settings
        
        mock_azure_scheme = Mock()
        mock_azure_scheme_class.return_value = mock_azure_scheme
        
        mock_azure_middleware = Mock()
        mock_azure_middleware_class.return_value = mock_azure_middleware
        
        # Act
        app = FastAPIFactory.create_app(
            title="Test API",
            description="Test API description",
            version="1.0.0",
            enable_azure_auth=True,
            enable_cors=True
        )
        
        # Assert
        cors_middleware = next(
            (middleware for middleware in app.user_middleware 
             if issubclass(middleware.cls, CORSMiddleware)), 
            None
        )
        assert cors_middleware is not None

    def test_create_app_without_azure_auth_no_me_endpoint(self):
        """Test that the /me endpoint is not available when Azure auth is disabled."""
        # Arrange & Act
        app = FastAPIFactory.create_app(
            title="Test API",
            description="Test API description",
            version="1.0.0",
            enable_azure_auth=False
        )
        
        # Assert
        route_paths = [route.path for route in app.routes]
        assert "/me" not in route_paths

    def test_create_app_custom_openapi_urls(self):
        """Test FastAPI application creation with custom OpenAPI URLs."""
        # Arrange
        custom_openapi_url = "/custom-openapi.json"
        custom_docs_url = "/custom-docs"
        custom_redoc_url = "/custom-redoc"
        
        # Act
        app = FastAPIFactory.create_app(
            title="Test API",
            description="Test API description",
            version="1.0.0",
            enable_azure_auth=False,
            openapi_url=custom_openapi_url,
            docs_url=custom_docs_url,
            redoc_url=custom_redoc_url
        )
        
        # Assert
        assert app.openapi_url == custom_openapi_url
        assert app.docs_url == custom_docs_url
        assert app.redoc_url == custom_redoc_url

    def test_create_app_multiple_databases_enabled(self):
        """Test FastAPI application creation with multiple databases enabled."""
        # Arrange
        with patch('utils.app_factory.get_postgres_client') as mock_postgres, \
             patch('utils.app_factory.get_neo4j_client') as mock_neo4j, \
             patch('utils.app_factory.get_redis_client') as mock_redis:
            
            mock_postgres_client = Mock()
            mock_neo4j_client = Mock()
            mock_redis_client = Mock()
            
            mock_postgres.return_value = mock_postgres_client
            mock_neo4j.return_value = mock_neo4j_client
            mock_redis.return_value = mock_redis_client
            
            # Act
            app = FastAPIFactory.create_app(
                title="Test API",
                description="Test API description",
                version="1.0.0",
                enable_azure_auth=False,
                enable_postgres=True,
                enable_neo4j=True,
                enable_redis=True
            )
            
            # Assert
            assert hasattr(app.state, 'postgres_client')
            assert hasattr(app.state, 'neo4j_client')
            assert hasattr(app.state, 'redis_client')
            
            route_paths = [route.path for route in app.routes]
            assert "/health/postgres" in route_paths
            assert "/health/neo4j" in route_paths
            assert "/health/redis" in route_paths