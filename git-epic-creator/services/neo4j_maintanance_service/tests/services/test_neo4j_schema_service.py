"""Tests for Neo4j Schema Service."""
import asyncio
from unittest.mock import patch, MagicMock

import pytest
import neo4j

from services.neo4j_schema_service import Neo4jSchemaService
from services.schema_query_builder import SchemaQueryBuilder
from utils.neo4j_client import Neo4jClient

# Test configuration
TEST_NEO4J_CONFIG = {
    'NEO4J_URI': 'bolt://localhost:7687',
    'NEO4J_USERNAME': 'test_user',
    'NEO4J_PASSWORD': 'test_password',
    'NEO4J_DATABASE': 'test_db',
    'NEO4J_CONNECTION_TIMEOUT': '30.0',
    'NEO4J_MAX_RETRY_ATTEMPTS': '3',
    'NEO4J_RETRY_DELAY': '1.0'
}

class MockSettings:
    """Mock settings for testing."""
    
    def __init__(self):
        """Initialize mock settings with test configuration."""
        self.NEO4J_URI = TEST_NEO4J_CONFIG['NEO4J_URI']
        self.NEO4J_USERNAME = TEST_NEO4J_CONFIG['NEO4J_USERNAME']
        self.NEO4J_PASSWORD = TEST_NEO4J_CONFIG['NEO4J_PASSWORD']
        self.NEO4J_DATABASE = TEST_NEO4J_CONFIG['NEO4J_DATABASE']
        self.NEO4J_CONNECTION_TIMEOUT = float(
            TEST_NEO4J_CONFIG['NEO4J_CONNECTION_TIMEOUT']
        )
        self.NEO4J_MAX_RETRY_ATTEMPTS = int(
            TEST_NEO4J_CONFIG['NEO4J_MAX_RETRY_ATTEMPTS']
        )
        self.NEO4J_RETRY_DELAY = float(TEST_NEO4J_CONFIG['NEO4J_RETRY_DELAY'])

@pytest.fixture
def mock_settings():
    """Create mock settings for testing."""
    return MockSettings()


@pytest.fixture
def mock_neo4j_client():
    """Create mock Neo4j client for testing."""
    return MagicMock(spec=Neo4jClient)


@pytest.fixture
def mock_query_builder():
    """Create mock query builder for testing."""
    return MagicMock(spec=SchemaQueryBuilder)


@pytest.fixture
def schema_service(mock_neo4j_client, mock_query_builder, mock_settings):
    """Create Neo4j schema service instance for testing."""
    return Neo4jSchemaService(mock_neo4j_client, mock_query_builder, mock_settings)


def setup_successful_session_mock(mock_neo4j_client):
    """Set up a successful session mock for testing."""
    mock_session = MagicMock()
    mock_result = MagicMock()
    mock_neo4j_client.get_session.return_value.__enter__.return_value = mock_session
    mock_neo4j_client.get_session.return_value.__exit__.return_value = None
    mock_session.run.return_value = mock_result
    mock_result.consume.return_value = None
    return mock_session, mock_result


def setup_failing_session_mock(mock_neo4j_client, exception):
    """Set up a failing session mock for testing."""
    mock_session = MagicMock()
    mock_neo4j_client.get_session.return_value.__enter__.return_value = mock_session
    mock_neo4j_client.get_session.return_value.__exit__.return_value = None
    mock_session.run.side_effect = exception
    return mock_session


async def run_async_test(test_func):
    """Helper to run async test functions."""
    return await test_func()

def test_schema_service_initialization(schema_service):
    """Test that Neo4j schema service initializes correctly with dependencies."""
    assert schema_service is not None
    assert hasattr(schema_service, 'neo4j_client')
    assert hasattr(schema_service, 'query_builder')
    assert hasattr(schema_service, 'settings')


def test_schema_service_requires_dependencies():
    """Test that Neo4j schema service requires all dependencies."""
    with pytest.raises(TypeError):
        Neo4jSchemaService()


def test_execute_query_success(schema_service, mock_neo4j_client):
    """Test successful query execution."""
    setup_successful_session_mock(mock_neo4j_client)
    
    async def test_async():
        result = await schema_service.execute_query("CREATE CONSTRAINT test")
        assert result["success"] is True
        assert result["query"] == "CREATE CONSTRAINT test"
        assert result["attempts"] == 1
    
    asyncio.run(test_async())

def test_execute_query_with_retry(schema_service, mock_neo4j_client):
    """Test query execution with retry logic on transient errors."""
    mock_session = MagicMock()
    mock_result = MagicMock()
    mock_neo4j_client.get_session.return_value.__enter__.return_value = mock_session
    mock_neo4j_client.get_session.return_value.__exit__.return_value = None
    mock_session.run.side_effect = [
        neo4j.exceptions.TransientError("Temporary failure"),
        mock_result
    ]
    mock_result.consume.return_value = None
    
    async def test_async():
        result = await schema_service.execute_query("CREATE CONSTRAINT test")
        assert result["success"] is True
        assert result["query"] == "CREATE CONSTRAINT test"
        assert result["attempts"] == 2
    
    asyncio.run(test_async())

def test_execute_query_failure_after_retries(schema_service, mock_neo4j_client):
    """Test query execution failure after maximum retry attempts."""
    mock_session = setup_failing_session_mock(
        mock_neo4j_client, neo4j.exceptions.ClientError("Schema error")
    )
    
    async def test_async():
        result = await schema_service.execute_query("CREATE CONSTRAINT test")
        assert result["success"] is False
        assert result["query"] == "CREATE CONSTRAINT test"
        assert result["attempts"] == 3
        assert "Schema error" in result["error"]
        assert mock_session.run.call_count == 3
    
    asyncio.run(test_async())


def test_execute_queries_batch_success(schema_service, mock_neo4j_client):
    """Test successful batch query execution."""
    mock_session, _ = setup_successful_session_mock(mock_neo4j_client)
    
    queries = ["CREATE CONSTRAINT test1", "CREATE CONSTRAINT test2"]
    
    async def test_async():
        results = await schema_service.execute_queries_batch(queries)
        assert results["total_queries"] == 2
        assert len(results["executed_queries"]) == 2
        assert len(results["failed_queries"]) == 0
        assert results["success_rate"] == "100.0%"
        assert mock_session.run.call_count == 2
    
    asyncio.run(test_async())


def test_execute_queries_batch_mixed_results(schema_service, mock_neo4j_client):
    """Test batch query execution with mixed success and failure results."""
    mock_session = MagicMock()
    mock_result = MagicMock()
    mock_neo4j_client.get_session.return_value.__enter__.return_value = mock_session
    mock_neo4j_client.get_session.return_value.__exit__.return_value = None
    mock_session.run.side_effect = [
        mock_result,  # Success
        neo4j.exceptions.ClientError("Schema error")
    ] + [neo4j.exceptions.ClientError("Schema error")] * 2
    mock_result.consume.return_value = None
    
    queries = ["CREATE CONSTRAINT test1", "CREATE CONSTRAINT test2"]
    
    async def test_async():
        results = await schema_service.execute_queries_batch(queries)
        assert results["total_queries"] == 2
        assert len(results["executed_queries"]) == 1
        assert len(results["failed_queries"]) == 1
        assert results["success_rate"] == "50.0%"
    
    asyncio.run(test_async())

@patch('services.neo4j_schema_service.Neo4jHealthChecker')
def test_initialize_schema_success(mock_health_checker, schema_service, mock_neo4j_client, mock_query_builder):
    """Test successful schema initialization."""
    setup_successful_session_mock(mock_neo4j_client)
    mock_query_builder.get_constraint_queries.return_value = ["CREATE CONSTRAINT test1"]
    mock_query_builder.get_index_queries.return_value = ["CREATE INDEX test2"]
    mock_query_builder.get_relationship_type_queries.return_value = ["CREATE RELATIONSHIP test3"]
    
    # Mock the async health check method
    async def mock_health_check(client):
        return True
    mock_health_checker.check_health = mock_health_check
    
    async def test_async():
        result = await schema_service.initialize_schema()
        assert result["success"] is True
        assert result["constraints"]["total_queries"] == 1
        assert result["indexes"]["total_queries"] == 1
        assert result["relationship_types"]["total_queries"] == 1
        assert result["summary"]["total_queries"] == 3
        assert result["summary"]["executed_successfully"] == 3
        assert result["summary"]["failed"] == 0
    
    asyncio.run(test_async())


@patch('services.neo4j_schema_service.Neo4jHealthChecker')
def test_initialize_schema_connection_failure(mock_health_checker, mock_neo4j_client, mock_query_builder, mock_settings):
    """Test schema initialization with connection failure."""
    async def mock_health_check_failure(client):
        raise neo4j.exceptions.ServiceUnavailable("Connection failed")
    mock_health_checker.check_health = mock_health_check_failure
    service = Neo4jSchemaService(mock_neo4j_client, mock_query_builder, mock_settings)
    
    async def test_async():
        with pytest.raises(neo4j.exceptions.ServiceUnavailable):
            await service.initialize_schema()
    
    asyncio.run(test_async())


@patch('services.neo4j_schema_service.Neo4jHealthChecker')
def test_initialize_schema_verification_failure(mock_health_checker, mock_neo4j_client, mock_query_builder, mock_settings):
    """Test schema initialization with verification failure."""
    async def mock_health_check_false(client):
        return False
    mock_health_checker.check_health = mock_health_check_false
    service = Neo4jSchemaService(mock_neo4j_client, mock_query_builder, mock_settings)
    
    async def test_async():
        with pytest.raises(neo4j.exceptions.Neo4jError, match="Neo4j connection verification failed"):
            await service.initialize_schema()
    
    asyncio.run(test_async())


@patch('services.neo4j_schema_service.Neo4jHealthChecker')
def test_initialize_schema_partial_failure(mock_health_checker, mock_neo4j_client, mock_query_builder, mock_settings):
    """Test schema initialization with partial failure."""
    mock_session = MagicMock()
    mock_result = MagicMock()
    mock_neo4j_client.get_session.return_value.__enter__.return_value = mock_session
    mock_neo4j_client.get_session.return_value.__exit__.return_value = None
    mock_session.run.side_effect = [
        mock_result,  # Success
        neo4j.exceptions.ClientError("Schema error")
    ] + [neo4j.exceptions.ClientError("Schema error")] * 2
    mock_result.consume.return_value = None
    mock_query_builder.get_constraint_queries.return_value = ["CREATE CONSTRAINT test1"]
    mock_query_builder.get_index_queries.return_value = ["CREATE INDEX test2"]
    mock_query_builder.get_relationship_type_queries.return_value = []

    async def mock_health_check_true(client):
        return True
    mock_health_checker.check_health = mock_health_check_true

    service = Neo4jSchemaService(mock_neo4j_client, mock_query_builder, mock_settings)

    async def test_async():
        result = await service.initialize_schema()
        assert result["success"] is False
        assert result["summary"]["total_queries"] == 2
        assert result["summary"]["executed_successfully"] == 1
        assert result["summary"]["failed"] == 1
        assert result["summary"]["success_rate"] == "50.0%"
    
    asyncio.run(test_async())


@patch('services.neo4j_schema_service.Neo4jHealthChecker')
def test_initialize_schema_complete_failure(mock_health_checker, mock_neo4j_client, mock_query_builder, mock_settings):
    """Test schema initialization with complete failure."""
    mock_session = setup_failing_session_mock(
        mock_neo4j_client, neo4j.exceptions.ClientError("Schema error")
    )
    mock_query_builder.get_constraint_queries.return_value = ["CREATE CONSTRAINT test1"]
    mock_query_builder.get_index_queries.return_value = ["CREATE INDEX test2"]
    mock_query_builder.get_relationship_type_queries.return_value = []
    
    async def mock_health_check_true(client):
        return True
    mock_health_checker.check_health = mock_health_check_true

    service = Neo4jSchemaService(mock_neo4j_client, mock_query_builder, mock_settings)

    async def test_async():
        result = await service.initialize_schema()
        assert result["success"] is False
        assert result["summary"]["total_queries"] == 2
        assert result["summary"]["executed_successfully"] == 0
        assert result["summary"]["failed"] == 2
        assert result["summary"]["success_rate"] == "0.0%"
    
    asyncio.run(test_async())


def test_calculate_success_rate(mock_neo4j_client, mock_query_builder, mock_settings):
    """Test success rate calculation method."""
    service = Neo4jSchemaService(mock_neo4j_client, mock_query_builder, mock_settings)
    assert service._calculate_success_rate(5, 10) == "50.0%"
    assert service._calculate_success_rate(10, 10) == "100.0%"
    assert service._calculate_success_rate(0, 10) == "0.0%"
    assert service._calculate_success_rate(0, 0) == "0%"
    assert service._calculate_success_rate(3, 7) == "42.9%"