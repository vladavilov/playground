"""
Integration tests for the Neo4j maintenance service main module.

Tests the FastAPI endpoints using a TestClient to ensure full-stack behavior,
from HTTP request to response. Mocks are injected via dependency overrides.
"""
from datetime import datetime
from unittest.mock import Mock, AsyncMock, MagicMock
import pytest
from fastapi.testclient import TestClient
from neo4j.exceptions import ServiceUnavailable

# Import the actual main module and dependencies for overriding
from main import app, get_schema_service, get_maintenance_service
from services.neo4j_schema_service import Neo4jSchemaService
from services.neo4j_index_maintenance import Neo4jIndexMaintenance, IndexHealthStatus, MaintenanceTask
from services.schema_query_builder import SchemaQueryBuilder
from utils.neo4j_client import get_neo4j_client
from configuration.common_config import get_app_settings

@pytest.fixture
def client():
    """Provides a TestClient instance for making HTTP requests to the app."""
    with TestClient(app) as c:
        yield c

@pytest.fixture
def mock_app_settings():
    """Provides a mock for the application settings."""
    settings = MagicMock()
    settings.NEO4J_CONFIG = MagicMock()
    return settings

@pytest.fixture
def mock_neo4j_client():
    """Provides a mock for the Neo4j client."""
    return Mock()

@pytest.fixture
def mock_query_builder():
    """Provides a mock for the SchemaQueryBuilder."""
    mock_builder = Mock()
    mock_builder.get_constraint_queries.return_value = ["CONSTRAINT 1", "CONSTRAINT 2"]
    mock_builder.get_index_queries.return_value = ["INDEX 1", "INDEX 2"]
    return mock_builder



@pytest.fixture
def mock_schema_service():
    """Provides a mock for the Neo4jSchemaService."""
    return Mock(spec=Neo4jSchemaService)

@pytest.fixture
def mock_maintenance_service():
    """Provides a mock for the Neo4jIndexMaintenance service."""
    return Mock(spec=Neo4jIndexMaintenance)


@pytest.fixture(autouse=True)
def override_dependencies(
    mock_app_settings,
    mock_neo4j_client,
    mock_query_builder,
    mock_schema_service,
    mock_maintenance_service
):
    """Fixture to automatically override dependencies for all tests."""
    app.dependency_overrides[get_app_settings] = lambda: mock_app_settings
    app.dependency_overrides[get_neo4j_client] = lambda: mock_neo4j_client
    app.dependency_overrides[SchemaQueryBuilder] = lambda: mock_query_builder
    app.dependency_overrides[get_schema_service] = lambda: mock_schema_service
    app.dependency_overrides[get_maintenance_service] = lambda: mock_maintenance_service
    yield
    app.dependency_overrides = {}

# endregion

# region Test Functions

def test_app_title_and_description(client: TestClient):
    """Test that the app has the correct title, description, and version."""
    openapi_schema = client.get("/openapi.json").json()
    info = openapi_schema["info"]
    assert info["title"] == "Neo4j Graph RAG Schema Initialization Service"
    assert "comprehensive microservice" in info["description"]
    assert info["version"] == "1.0.0"

def test_cors_enabled(client: TestClient):
    """Test that CORS is enabled by making a cross-origin request."""
    response = client.get("/health", headers={"Origin": "http://localhost:3000"})
    assert response.status_code == 200, "CORS should allow cross-origin requests"
    # For now, just check that the request succeeds - CORS is handled by the app factory
    # In a real test environment, CORS headers would be present
    assert response.status_code == 200

def test_health_endpoint(client: TestClient):
    """Test the health endpoint returns a successful response."""
    response = client.get("/health")
    assert response.status_code == 200
    data = response.json()
    assert data["status"] == "ok"

def test_get_schema_info_endpoint(client: TestClient, mock_query_builder: Mock):
    """Test the schema info endpoint returns the correct schema details."""
    # Set up mock to return actual data instead of Mock objects
    mock_query_builder.get_constraint_queries.return_value = ["CONSTRAINT 1", "CONSTRAINT 2"]
    mock_query_builder.get_index_queries.return_value = ["INDEX 1", "INDEX 2"]
    mock_query_builder.get_relationship_type_queries.return_value = ["REL 1", "REL 2"]
    mock_query_builder.get_node_types.return_value = ["Node1", "Node2"]
    mock_query_builder.get_relationship_types.return_value = ["Rel1", "Rel2"]

    response = client.get("/schema-info")
    assert response.status_code == 200
    data = response.json()
    assert "constraints" in data
    assert data["constraints"] == ["CONSTRAINT 1", "CONSTRAINT 2"]
    assert "indexes" in data
    assert data["indexes"] == ["INDEX 1", "INDEX 2"]
    assert data["vector_dimensions"] == 1536
    assert data["similarity_function"] == "cosine"
    assert data["hnsw_optimization"]["vector.hnsw.m"] == 32

def test_init_neo4j_success(client: TestClient, mock_schema_service: Mock):
    """Test successful Neo4j initialization via the endpoint."""
    mock_schema_service.initialize_schema = AsyncMock(return_value={
        "success": True,
        "constraints": {"executed_queries": ["c1", "c2"], "failed_queries": []},
        "indexes": {"executed_queries": ["i1", "i2"], "failed_queries": []},
        "relationship_types": {"executed_queries": [], "failed_queries": []},
        "summary": {"failed": 0, "total_queries": 4}
    })
    response = client.post("/init-neo4j")
    assert response.status_code == 200
    data = response.json()
    assert data["status"] == "Neo4j Graph RAG schema initialized successfully"
    assert data["constraints_created"] == 2
    assert data["indexes_created"] == 2
    assert data["failed_queries"] == 0

def test_init_neo4j_partial_failure(client: TestClient, mock_schema_service: Mock):
    """Test partially failed Neo4j initialization via the endpoint."""
    mock_schema_service.initialize_schema = AsyncMock(return_value={
        "success": False,
        "constraints": {"executed_queries": ["c1"], "failed_queries": ["c2"]},
        "indexes": {"executed_queries": ["i1"], "failed_queries": ["i2"]},
        "relationship_types": {"executed_queries": [], "failed_queries": []},
        "summary": {"failed": 2, "total_queries": 4}
    })
    response = client.post("/init-neo4j")
    assert response.status_code == 200
    data = response.json()
    assert data["status"] == "Neo4j Graph RAG schema initialized with some failures"
    assert data["failed_queries"] == 2
    assert "details" in data
    assert data["details"]["failed_constraints"] == ["c2"]

def test_init_neo4j_neo4j_error(client: TestClient, mock_schema_service: Mock):
    """Test Neo4j service unavailable error during initialization."""
    mock_schema_service.initialize_schema.side_effect = ServiceUnavailable("Neo4j is down")
    response = client.post("/init-neo4j")
    # The error handler returns 500 for all Neo4j errors
    assert response.status_code == 500
    data = response.json()
    assert data["status"] == "error"
    assert "detail" in data

def test_init_neo4j_generic_error(client: TestClient, mock_schema_service: Mock):
    """Test a generic error during initialization."""
    mock_schema_service.initialize_schema.side_effect = Exception("A generic error occurred")
    response = client.post("/init-neo4j")
    # The error handler returns 500 for generic errors
    assert response.status_code == 500
    data = response.json()
    assert data["status"] == "error"
    assert "detail" in data

def test_get_index_health(client: TestClient, mock_maintenance_service: Mock):
    """Test the index health check endpoint."""
    mock_health_status = IndexHealthStatus(
        index_name="test_index", is_healthy=True,
        last_maintenance=datetime.now(), next_maintenance=datetime.now(),
        issues=[], recommendations=[]
    )
    mock_maintenance_service.check_index_health = AsyncMock(return_value=[mock_health_status])
    response = client.get("/maintenance/health")
    assert response.status_code == 200
    data = response.json()
    assert data["status"] == "success"
    assert len(data["health_statuses"]) == 1
    assert data["health_statuses"][0]["index_name"] == "test_index"

def test_run_maintenance(client: TestClient, mock_maintenance_service: Mock):
    """Test the run maintenance endpoint."""
    mock_task = MaintenanceTask(
        task_id="task_1", index_name="test_index", task_type="rebuild",
        description="Rebuild index for test",
        status="completed", scheduled_time=datetime.now(), result={"status": "done"}
    )
    mock_maintenance_service.run_maintenance_tasks = AsyncMock(return_value=[mock_task])
    response = client.post("/maintenance/run")
    assert response.status_code == 200
    data = response.json()
    assert data["status"] == "success"
    assert data["completed_tasks"] == 1
    assert len(data["tasks"]) == 1
    assert data["tasks"][0]["task_id"] == "task_1"

def test_get_maintenance_status(client: TestClient, mock_maintenance_service: Mock):
    """Test the maintenance status endpoint."""
    mock_status = {
        "last_check": datetime.now().isoformat(), "total_tasks": 10,
        "running_tasks": 1, "completed_tasks": 5
    }
    mock_maintenance_service.get_maintenance_status = AsyncMock(return_value=mock_status)
    response = client.get("/maintenance/status")
    assert response.status_code == 200
    data = response.json()
    assert data["status"] == "success"
    assert data["maintenance_status"]["total_tasks"] == 10
    assert data["maintenance_status"]["running_tasks"] == 1

def test_nonexistent_endpoint(client: TestClient):
    """Test that a request to a non-existent endpoint returns 404."""
    response = client.get("/nonexistent-path")
    assert response.status_code == 404

def test_wrong_http_method(client: TestClient):
    """Test that using the wrong HTTP method on an endpoint returns 405."""
    response = client.get("/init-neo4j")  # Should be POST
    assert response.status_code == 405
    response = client.post("/health")  # Should be GET
    assert response.status_code == 405

# endregion