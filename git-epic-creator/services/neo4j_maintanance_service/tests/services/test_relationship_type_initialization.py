"""Tests for relationship type initialization functionality."""
import asyncio
from unittest.mock import patch, MagicMock

import pytest
import neo4j

from services.neo4j_schema_service import Neo4jSchemaService
from services.schema_query_builder import SchemaQueryBuilder
from utils.neo4j_client import Neo4jClient


class MockSettings:
    """Mock settings for testing."""
    
    def __init__(self):
        """Initialize mock settings with test configuration."""
        self.NEO4J_URI = 'bolt://localhost:7687'
        self.NEO4J_USERNAME = 'test_user'
        self.NEO4J_PASSWORD = 'test_password'
        self.NEO4J_DATABASE = 'test_db'
        self.NEO4J_CONNECTION_TIMEOUT = 30.0
        self.NEO4J_MAX_RETRY_ATTEMPTS = 3
        self.NEO4J_RETRY_DELAY = 1.0


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


# --- SchemaQueryBuilder Tests for Relationship Type Queries ---

def test_schema_query_builder_has_get_relationship_type_queries_method():
    """Test that SchemaQueryBuilder has get_relationship_type_queries method."""
    builder = SchemaQueryBuilder()
    assert hasattr(builder, 'get_relationship_type_queries')
    assert callable(getattr(builder, 'get_relationship_type_queries'))


def test_get_relationship_type_queries_returns_list():
    """Test that get_relationship_type_queries returns a list."""
    builder = SchemaQueryBuilder()
    queries = builder.get_relationship_type_queries()
    assert isinstance(queries, list)
    assert len(queries) > 0


def test_get_relationship_type_queries_content():
    """Test that relationship type queries have correct content and structure."""
    builder = SchemaQueryBuilder()
    queries = builder.get_relationship_type_queries()
    
    # Expected relationship types from the existing get_relationship_types method
    expected_relationships = ["REFERENCED_BY", "EVIDENCED_BY", "MERGED_FROM", "RELATED_TO", "DESCRIBED_IN"]
    
    assert len(queries) == len(expected_relationships)
    
    for query in queries:
        assert query.startswith("CALL db.createRelationshipType(")
        assert query.endswith(")")
        assert any(rel_type in query for rel_type in expected_relationships)
    
    # Verify each relationship type has a corresponding query
    query_text = " ".join(queries)
    for rel_type in expected_relationships:
        assert f"'{rel_type}'" in query_text


def test_get_relationship_type_queries_idempotent():
    """Test that relationship type queries are idempotent (can be called multiple times)."""
    builder = SchemaQueryBuilder()
    queries1 = builder.get_relationship_type_queries()
    queries2 = builder.get_relationship_type_queries()
    assert queries1 == queries2


def test_get_all_queries_includes_relationship_types():
    """Test that get_all_queries includes relationship type queries."""
    builder = SchemaQueryBuilder()
    constraints = builder.get_constraint_queries()
    indexes = builder.get_index_queries()
    relationship_types = builder.get_relationship_type_queries()
    all_queries = builder.get_all_queries()
    
    expected_total = len(constraints) + len(indexes) + len(relationship_types)
    assert len(all_queries) == expected_total
    
    # Verify relationship type queries are included
    for rel_query in relationship_types:
        assert rel_query in all_queries


def test_get_schema_info_includes_relationship_type_queries():
    """Test that get_schema_info includes relationship type queries."""
    builder = SchemaQueryBuilder()
    schema_info = builder.get_schema_info()
    
    assert "relationship_type_queries" in schema_info
    assert isinstance(schema_info["relationship_type_queries"], list)
    assert len(schema_info["relationship_type_queries"]) > 0


# --- Neo4jSchemaService Tests for Relationship Type Initialization ---

def test_execute_relationship_type_query_success(schema_service, mock_neo4j_client):
    """Test successful execution of relationship type creation query."""
    setup_successful_session_mock(mock_neo4j_client)
    
    async def test_async():
        result = await schema_service.execute_query("CALL db.createRelationshipType('REFERENCED_BY')")
        assert result["success"] is True
        assert result["query"] == "CALL db.createRelationshipType('REFERENCED_BY')"
        assert result["attempts"] == 1
    
    asyncio.run(test_async())


def test_execute_relationship_type_queries_batch_success(schema_service, mock_neo4j_client):
    """Test successful batch execution of relationship type queries."""
    mock_session, _ = setup_successful_session_mock(mock_neo4j_client)
    
    queries = [
        "CALL db.createRelationshipType('REFERENCED_BY')",
        "CALL db.createRelationshipType('EVIDENCED_BY')"
    ]
    
    async def test_async():
        results = await schema_service.execute_queries_batch(queries)
        assert results["total_queries"] == 2
        assert len(results["executed_queries"]) == 2
        assert len(results["failed_queries"]) == 0
        assert results["success_rate"] == "100.0%"
        assert mock_session.run.call_count == 2
    
    asyncio.run(test_async())


def test_execute_relationship_type_queries_with_retry(schema_service, mock_neo4j_client):
    """Test relationship type query execution with retry logic."""
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
        result = await schema_service.execute_query("CALL db.createRelationshipType('REFERENCED_BY')")
        assert result["success"] is True
        assert result["attempts"] == 2
    
    asyncio.run(test_async())


def test_execute_relationship_type_queries_failure_after_retries(schema_service, mock_neo4j_client):
    """Test relationship type query failure after maximum retry attempts."""
    mock_session = setup_failing_session_mock(
        mock_neo4j_client, neo4j.exceptions.ClientError("Permission denied")
    )
    
    async def test_async():
        result = await schema_service.execute_query("CALL db.createRelationshipType('REFERENCED_BY')")
        assert result["success"] is False
        assert result["attempts"] == 3
        assert "Permission denied" in result["error"]
        assert mock_session.run.call_count == 3
    
    asyncio.run(test_async())


@patch('services.neo4j_schema_service.Neo4jHealthChecker')
def test_initialize_schema_includes_relationship_types(mock_health_checker, schema_service, mock_neo4j_client, mock_query_builder):
    """Test that schema initialization includes relationship type creation."""
    setup_successful_session_mock(mock_neo4j_client)
    mock_query_builder.get_constraint_queries.return_value = ["CREATE CONSTRAINT test1"]
    mock_query_builder.get_index_queries.return_value = ["CREATE INDEX test2"]
    mock_query_builder.get_relationship_type_queries.return_value = [
        "CALL db.createRelationshipType('REFERENCED_BY')",
        "CALL db.createRelationshipType('EVIDENCED_BY')"
    ]

    async def mock_health_check_true(client):
        return True
    mock_health_checker.check_health = mock_health_check_true

    async def test_async():
        result = await schema_service.initialize_schema()
        assert result["success"] is True
        assert "relationship_types" in result
        assert result["relationship_types"]["total_queries"] == 2
        assert len(result["relationship_types"]["executed_queries"]) == 2
        assert len(result["relationship_types"]["failed_queries"]) == 0
        
        # Verify total counts include relationship types
        expected_total = 1 + 1 + 2  # constraints + indexes + relationship_types
        assert result["summary"]["total_queries"] == expected_total
        assert result["summary"]["executed_successfully"] == expected_total
        assert result["summary"]["failed"] == 0
    
    asyncio.run(test_async())


@patch('services.neo4j_schema_service.Neo4jHealthChecker')
def test_initialize_schema_partial_relationship_type_failure(mock_health_checker, mock_neo4j_client, mock_query_builder, mock_settings):
    """Test schema initialization with partial relationship type creation failure."""
    mock_session = MagicMock()
    mock_result = MagicMock()
    mock_neo4j_client.get_session.return_value.__enter__.return_value = mock_session
    mock_neo4j_client.get_session.return_value.__exit__.return_value = None
    
    # First 3 calls succeed (constraint, index, first relationship type)
    # Last call fails (second relationship type)
    mock_session.run.side_effect = [
        mock_result,  # constraint success
        mock_result,  # index success
        mock_result,  # first relationship type success
        neo4j.exceptions.ClientError("Permission denied")  # second relationship type fails
    ] + [neo4j.exceptions.ClientError("Permission denied")] * 2  # retries for failed query
    
    mock_result.consume.return_value = None
    mock_query_builder.get_constraint_queries.return_value = ["CREATE CONSTRAINT test1"]
    mock_query_builder.get_index_queries.return_value = ["CREATE INDEX test2"]
    mock_query_builder.get_relationship_type_queries.return_value = [
        "CALL db.createRelationshipType('REFERENCED_BY')",
        "CALL db.createRelationshipType('EVIDENCED_BY')"
    ]

    async def mock_health_check_true(client):
        return True
    mock_health_checker.check_health = mock_health_check_true

    service = Neo4jSchemaService(mock_neo4j_client, mock_query_builder, mock_settings)

    async def test_async():
        result = await service.initialize_schema()
        assert result["success"] is False
        assert result["relationship_types"]["total_queries"] == 2
        assert len(result["relationship_types"]["executed_queries"]) == 1
        assert len(result["relationship_types"]["failed_queries"]) == 1
        assert result["summary"]["total_queries"] == 4
        assert result["summary"]["executed_successfully"] == 3
        assert result["summary"]["failed"] == 1
    
    asyncio.run(test_async())


@patch('services.neo4j_schema_service.Neo4jHealthChecker')
def test_initialize_schema_all_relationship_types_fail(mock_health_checker, mock_neo4j_client, mock_query_builder, mock_settings):
    """Test schema initialization when all relationship type creations fail."""
    mock_session = MagicMock()
    mock_result = MagicMock()
    mock_neo4j_client.get_session.return_value.__enter__.return_value = mock_session
    mock_neo4j_client.get_session.return_value.__exit__.return_value = None
    
    # First 2 calls succeed (constraint, index)
    # All relationship type calls fail
    mock_session.run.side_effect = [
        mock_result,  # constraint success
        mock_result,  # index success
        neo4j.exceptions.ClientError("Permission denied"),  # first relationship type fails
        neo4j.exceptions.ClientError("Permission denied"),  # retry 1
        neo4j.exceptions.ClientError("Permission denied"),  # retry 2
        neo4j.exceptions.ClientError("Permission denied"),  # second relationship type fails
        neo4j.exceptions.ClientError("Permission denied"),  # retry 1
        neo4j.exceptions.ClientError("Permission denied"),  # retry 2
    ]
    
    mock_result.consume.return_value = None
    mock_query_builder.get_constraint_queries.return_value = ["CREATE CONSTRAINT test1"]
    mock_query_builder.get_index_queries.return_value = ["CREATE INDEX test2"]
    mock_query_builder.get_relationship_type_queries.return_value = [
        "CALL db.createRelationshipType('REFERENCED_BY')",
        "CALL db.createRelationshipType('EVIDENCED_BY')"
    ]

    async def mock_health_check_true(client):
        return True
    mock_health_checker.check_health = mock_health_check_true

    service = Neo4jSchemaService(mock_neo4j_client, mock_query_builder, mock_settings)

    async def test_async():
        result = await service.initialize_schema()
        assert result["success"] is False
        assert result["relationship_types"]["total_queries"] == 2
        assert len(result["relationship_types"]["executed_queries"]) == 0
        assert len(result["relationship_types"]["failed_queries"]) == 2
        assert result["summary"]["total_queries"] == 4
        assert result["summary"]["executed_successfully"] == 2
        assert result["summary"]["failed"] == 2
    
    asyncio.run(test_async())


# --- Integration Tests ---

def test_relationship_type_initialization_end_to_end():
    """Test end-to-end relationship type initialization with real SchemaQueryBuilder."""
    builder = SchemaQueryBuilder()
    
    # Test that relationship type queries are generated correctly
    rel_queries = builder.get_relationship_type_queries()
    expected_relationships = ["REFERENCED_BY", "EVIDENCED_BY", "MERGED_FROM", "RELATED_TO", "DESCRIBED_IN"]
    
    assert len(rel_queries) == len(expected_relationships)
    
    for i, rel_type in enumerate(expected_relationships):
        expected_query = f"CALL db.createRelationshipType('{rel_type}')"
        assert rel_queries[i] == expected_query
    
    # Test that all queries include relationship types
    all_queries = builder.get_all_queries()
    for rel_query in rel_queries:
        assert rel_query in all_queries
    
    # Test schema info includes relationship type queries
    schema_info = builder.get_schema_info()
    assert "relationship_type_queries" in schema_info
    assert schema_info["relationship_type_queries"] == rel_queries


def test_relationship_type_queries_are_valid_cypher():
    """Test that generated relationship type queries are valid Cypher syntax."""
    builder = SchemaQueryBuilder()
    queries = builder.get_relationship_type_queries()
    
    for query in queries:
        # Basic syntax validation
        assert query.startswith("CALL db.createRelationshipType(")
        assert query.endswith(")")
        assert query.count("'") == 2  # Should have exactly two single quotes
        assert query.count("(") == 1   # Should have exactly one opening parenthesis
        assert query.count(")") == 1   # Should have exactly one closing parenthesis
        
        # Extract relationship type name
        start_quote = query.find("'")
        end_quote = query.find("'", start_quote + 1)
        rel_type = query[start_quote + 1:end_quote]
        
        # Validate relationship type name format
        assert rel_type.isupper()
        assert "_" in rel_type or rel_type.isalpha()
        assert not rel_type.startswith("_")
        assert not rel_type.endswith("_")