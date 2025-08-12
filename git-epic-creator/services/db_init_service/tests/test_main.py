"""
Unit tests for the database initialization service.
"""
import sys
from unittest.mock import patch, MagicMock
import pytest
from fastapi.testclient import TestClient

# Use a test-specific database URL (in-memory SQLite)
TEST_DATABASE_URL = "sqlite:///:memory:"

# Mock classes for testing
class MockBase:
    """Mock SQLAlchemy Base class."""
    metadata = MagicMock()

class MockPostgresClient:
    """Mock PostgreSQL client for testing."""
    def __init__(self, settings=None):
        self.settings = settings or MockPostgresSettings()
        # Create a mock sync_engine instead of real SQLAlchemy engine
        self.sync_engine = MagicMock()

    def get_sync_session(self):
        """Get a mock session."""
        return MagicMock()

class MockPostgresSettings:
    """Mock PostgreSQL settings for testing."""
    def __init__(self):
        self.POSTGRES_HOST = "localhost"
        self.POSTGRES_PORT = 5432
        self.POSTGRES_DB = "test_db"
        self.POSTGRES_USER = "test_user"

class MockPostgresHealthChecker:
    """Mock PostgreSQL health checker."""
    @staticmethod
    def check_health(client):
        """Mock health check method."""
        return True

    @staticmethod
    def check_health_with_details(client):
        """Mock detailed health check method."""
        return {
            "healthy": True,
            "version": "PostgreSQL 14.5",
            "connections": 1,
            "database": "test_db",
            "host": "localhost",
            "port": 5432
        }

# Session-scoped fixture to prevent SQLAlchemy re-registration
@pytest.fixture(scope="session", autouse=True)
def mock_sqlalchemy_modules():
    """Mock SQLAlchemy modules to prevent import conflicts."""
    # Create comprehensive mocks for all SQLAlchemy components
    mock_base = MagicMock()
    mock_base.Base = MockBase
    mock_base.declarative_base.return_value = MockBase

    mock_project_db = MagicMock()
    mock_project_db.Base = MockBase
    mock_project_db.Project = MagicMock()
    mock_project_db.ProjectMember = MagicMock()

    mock_models_init = MagicMock()
    mock_models_init.Project = MagicMock()
    mock_models_init.ProjectMember = MagicMock()

    # Mock all the modules that could cause SQLAlchemy conflicts
    modules_to_mock = {
        'models.project_db': mock_project_db,
        'models': mock_models_init,
        'sqlalchemy': MagicMock(),
        'sqlalchemy.sql': MagicMock(),
        'sqlalchemy.orm': MagicMock(),
        'sqlalchemy.inspection': MagicMock(),
        'sqlalchemy.orm.base': MagicMock(),
        'sqlalchemy.orm.attributes': MagicMock(),
        'sqlalchemy.orm.collections': MagicMock(),
        'sqlalchemy.orm.exc': MagicMock(),
        'sqlalchemy.orm.util': MagicMock(),
    }

    with patch.dict(sys.modules, modules_to_mock):
        yield

# Fixtures
@pytest.fixture(autouse=True)
def mock_imports():
    """Mock imports for testing."""
    mock_postgres_client = MagicMock(
        PostgresClient=MockPostgresClient,
        PostgresHealthChecker=MockPostgresHealthChecker,
        get_postgres_client=lambda: MockPostgresClient()
    )

    mock_postgres_config = MagicMock(
        PostgresSettings=MockPostgresSettings,
        get_postgres_settings=lambda: MockPostgresSettings()
    )

    mock_common_config = MagicMock()
    mock_common_config.get_app_settings.return_value = MagicMock(API_PORT=8000)

    mock_logging_config = MagicMock()
    # Create a real FastAPI app for testing with health endpoint
    from fastapi import FastAPI, APIRouter
    mock_app = FastAPI()
    
    # Add the health endpoint that the FastAPI factory would normally add
    health_router = APIRouter(prefix="/health", tags=["Health"])
    
    @health_router.get("/postgres")
    def postgres_health_check():
        """Mock PostgreSQL health check endpoint."""
        return MockPostgresHealthChecker.check_health_with_details(None)
    
    mock_app.include_router(health_router)

    # Register shared error handlers to test formatted exceptions
    try:
        from utils.error_handler import ErrorHandler
        ErrorHandler().register_exception_handlers(mock_app)
    except Exception:
        pass
    
    mock_app_factory = MagicMock()
    mock_app_factory.FastAPIFactory.create_app.return_value = mock_app

    modules_to_mock = {
        'utils.postgres_client': mock_postgres_client,
        'configuration.postgres_config': mock_postgres_config,
        'configuration.common_config': mock_common_config,
        'configuration.logging_config': mock_logging_config,
        'utils.app_factory': mock_app_factory,
        'structlog': MagicMock(),
        'uvicorn': MagicMock(),
    }

    with patch.dict(sys.modules, modules_to_mock):
        yield

@pytest.fixture
def mock_postgres_client():
    """Create a mock PostgreSQL client."""
    return MockPostgresClient()

@pytest.fixture
def mock_get_postgres_client(mock_postgres_client):
    """Mock the get_postgres_client function."""
    with patch('main.get_postgres_client', return_value=mock_postgres_client):
        yield

@pytest.fixture
def client():
    """Create a test client with mocked dependencies."""
    from main import app
    return TestClient(app)

# Tests
def test_postgres_health_check_success(client):
    """Test the postgres health check endpoint when database is healthy."""
    response = client.get("/health/postgres")
    assert response.status_code == 200
    data = response.json()
    assert data["healthy"] is True

def test_postgres_health_check_failure(client):
    """Test the postgres health check endpoint when database is unhealthy."""
    with patch(
        'utils.postgres_client.PostgresHealthChecker.check_health_with_details', 
        return_value={
            "healthy": False,
            "error": "Connection refused",
            "database": "test_db",
            "host": "localhost",
            "port": 5432
        }
    ) as mock_check:
        response = client.get("/health/postgres")
        assert response.status_code == 200
        data = response.json()
        assert data["healthy"] is False
        mock_check.assert_called_once()


def test_init_db_success(client):
    """Test successful database initialization."""
    with patch.object(MockBase.metadata, 'create_all') as mock_create_all:
        response = client.post("/db/init")
        assert response.status_code == 200
        assert response.json()["status"] == "Database initialized successfully"
        mock_create_all.assert_called_once()

def test_init_db_error(client):
    """Test error handling during database initialization (formatted)."""
    with patch.object(MockBase.metadata, 'create_all', side_effect=Exception("Test error")):
        response = client.post("/db/init")
        assert response.status_code == 500
        data = response.json()
        assert data["status"] == "error"
        assert "Unexpected error" in data["detail"]
        assert "Test error" in data["detail"]

def test_init_db_is_idempotent(client):
    """Test that running init-db multiple times is safe (idempotent)."""
    with patch.object(MockBase.metadata, 'create_all'):
        for _ in range(3):
            response = client.post("/db/init")
            assert response.status_code == 200
            assert response.json()["status"] == "Database initialized successfully"