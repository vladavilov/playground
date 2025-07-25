"""
Unit tests for the database initialization service.
"""
from unittest.mock import patch, MagicMock
import pytest
from fastapi.testclient import TestClient
from sqlalchemy import create_engine

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
        self.sync_engine = create_engine(TEST_DATABASE_URL)
  
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

# Fixtures
@pytest.fixture(autouse=True)
def mock_imports():
    """Mock imports for testing."""
    with patch.dict('sys.modules', {
        'models.base': MagicMock(Base=MockBase),
        'utils.postgres_client': MagicMock(
            PostgresClient=MockPostgresClient,
            PostgresHealthChecker=MockPostgresHealthChecker,
            get_postgres_client=lambda: MockPostgresClient()
        ),
        'configuration.postgres_config': MagicMock(
            PostgresSettings=MockPostgresSettings,
            get_postgres_settings=lambda: MockPostgresSettings()
        )
    }):
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
    """Test error handling during database initialization."""
    with patch.object(MockBase.metadata, 'create_all', 
                     side_effect=Exception("Test error")):
        response = client.post("/db/init")
        assert response.status_code == 200
        data = response.json()
        assert data["status"] == "error"
        assert "Test error" in data["detail"]

def test_init_db_is_idempotent(client):
    """Test that running init-db multiple times is safe (idempotent)."""
    with patch.object(MockBase.metadata, 'create_all'):
        for _ in range(3):
            response = client.post("/db/init")
            assert response.status_code == 200
            assert response.json()["status"] == "Database initialized successfully"