"""
Shared pytest fixtures and utilities for end-to-end tests.

This module provides reusable fixtures for authentication, database connections,
service health checks, and test data management.
"""

import uuid
import time
import os
from pathlib import Path
from typing import Dict, Any, Generator, Optional
import urllib3

import pytest
import redis
import requests
import psycopg2
from neo4j import GraphDatabase
from jose import jwt

from config import TestConfig, TestConstants
from services.redis_test_monitor import RedisTestMonitor
from shared_utils import ServiceHealthChecker
from services.workflow_assertions import WorkflowAssertions

# Disable SSL warnings for development/testing with self-signed certificates
urllib3.disable_warnings(urllib3.exceptions.InsecureRequestWarning)

# Disable SSL verification globally for requests in e2e tests
# This is safe for local development with self-signed certificates
requests.packages.urllib3.disable_warnings()  # type: ignore


@pytest.fixture(scope="session")
def test_config() -> TestConfig:
    """Provide TestConfig instance for the test session."""
    return TestConfig()


@pytest.fixture(scope="session") 
def service_urls(test_config: TestConfig) -> Dict[str, str]:
    """Get service URLs for the test session."""
    return test_config.get_service_urls()


@pytest.fixture(scope="session")
def postgres_config(test_config: TestConfig) -> Dict[str, any]:
    """Get PostgreSQL configuration for the test session."""
    return test_config.get_postgres_config()


@pytest.fixture(scope="session")
def neo4j_config(test_config: TestConfig) -> Dict[str, any]:
    """Get Neo4j configuration for the test session."""
    return test_config.get_neo4j_config()


@pytest.fixture(scope="session")
def redis_config(test_config: TestConfig) -> Dict[str, any]:
    """Get Redis configuration for the test session."""
    return test_config.get_redis_config()


@pytest.fixture(scope="session")
def auth_config(test_config: TestConfig) -> Dict[str, str]:
    """Get authentication configuration for the test session."""
    return test_config.get_auth_config()


@pytest.fixture(scope="session")
def services_ready(service_urls: Dict[str, str]) -> None:
    """
    Ensure all required services are healthy before running tests.
    
    This fixture will wait for all services to be ready or fail the test session.
    """
    try:
        ServiceHealthChecker.wait_for_all_services(service_urls)
    except RuntimeError as e:
        pytest.skip(f"Skipping tests: {e}")


def _create_local_jwt_token(oid: str = None, roles: list = None, username: str = None) -> str:
    """
    Create LOCAL JWT token for backend service authentication.
    
    Backend services validate tokens using LOCAL_JWT_SECRET (shared secret),
    NOT Azure AD tokens. This mimics what UI service does when minting S2S tokens.
    
    Args:
        oid: User object ID (defaults to test user)
        roles: User roles (defaults to ["Admin", "User"])
        username: Preferred username (defaults to test user email)
        
    Returns:
        Signed JWT token string
    """
    secret = os.getenv("LOCAL_JWT_SECRET", "dev-local-jwt-secret")
    now = int(time.time())
    
    claims = {
        "oid": oid or str(uuid.uuid4()),
        "preferred_username": username or "test.user@example.com",
        "roles": roles or ["Admin", "User"],
        "iss": "ui-service",
        "iat": now,
        "nbf": now,
        "exp": now + 3600,  # 1 hour validity
    }
    
    return jwt.encode(claims, secret, algorithm="HS256")


@pytest.fixture(scope="session", autouse=True)
def postgres_initialized(service_urls: Dict[str, str]) -> bool:
    """
    Initialize PostgreSQL database once before all tests.
    
    This fixture calls the init_db_service to set up the database schema.
    Uses LOCAL JWT token (not Azure AD) for backend service authentication.
    
    Args:
        service_urls: Service URL configuration
        
    Returns:
        True if initialization successful
        
    Raises:
        pytest.fail: If initialization fails
    """
    try:
        # Create LOCAL JWT token for backend service authentication
        token = _create_local_jwt_token(roles=["Admin"])
        auth_headers = {"Authorization": f"Bearer {token}"}
        
        response = requests.post(
            f"{service_urls['init_db_service']}/db/init",
            headers=auth_headers,
            timeout=TestConstants.DEFAULT_TIMEOUT,
            verify=False
        )

        if response.status_code == TestConstants.HTTP_OK:
            return True

        pytest.fail(
            f"Failed to initialize PostgreSQL database: "
            f"[{service_urls['init_db_service']}/db/init] "
            f"{response.status_code} - {response.text}"
        )
    except requests.RequestException as e:
        pytest.fail(f"Error initializing PostgreSQL database: {e}")


@pytest.fixture
def auth_headers() -> Dict[str, str]:
    """
    Get authentication headers with LOCAL JWT token for backend services.
    
    Backend services use LOCAL_JWT_SECRET validation (not Azure AD).
    This token mimics what UI service mints for S2S authentication.
    
    Returns:
        Dict with Authorization header containing Bearer token
    """
    token = _create_local_jwt_token(roles=["Admin", "User"])
    return {"Authorization": f"Bearer {token}"}


@pytest.fixture
def postgres_connection(postgres_config: Dict[str, any]) -> Generator[psycopg2.extensions.connection, None, None]:
    """
    Provide a PostgreSQL database connection.
    
    Yields:
        psycopg2 connection object
    """
    conn = None
    try:
        conn = psycopg2.connect(**postgres_config)
        yield conn
    finally:
        if conn:
            conn.close()


@pytest.fixture
def neo4j_driver(neo4j_config: Dict[str, any]) -> Generator[GraphDatabase.driver, None, None]:
    """
    Provide a Neo4j driver instance.
    
    Yields:
        Neo4j driver object
    """
    driver = None
    try:
        driver = GraphDatabase.driver(
            neo4j_config["uri"],
            auth=(neo4j_config["username"], neo4j_config["password"])
        )
        yield driver
    finally:
        if driver:
            driver.close()


@pytest.fixture
def redis_client(redis_config: Dict[str, any]) -> Generator[redis.Redis, None, None]:
    """
    Provide a Redis client instance.
    
    Yields:
        Redis client object
    """
    client = None
    try:
        client = redis.Redis(**redis_config)
        # Test connection
        client.ping()
        yield client
    finally:
        if client:
            client.close()


@pytest.fixture
def test_pdf_content(test_config: TestConfig) -> bytes:
    """Get test PDF content for document upload tests."""
    return test_config.read_dummy_pdf()


@pytest.fixture
def unique_test_filename() -> str:
    """Generate a unique test filename for each test."""
    return f"test_document_{uuid.uuid4().hex[:8]}.pdf"


@pytest.fixture
def test_project_data() -> Dict[str, any]:
    """Generate test project data with unique name."""
    return {
        "name": f"Test Project {uuid.uuid4().hex[:8]}",
        "description": "Integration test project",
        "status": TestConstants.PROJECT_STATUS_ACTIVE
    }


class ProjectManager:
    """
    Context manager for test projects that ensures proper cleanup.
    
    This class handles project creation and automatic cleanup,
    even if tests fail.
    """
    
    def __init__(
        self, 
        service_urls: Dict[str, str], 
        auth_headers: Dict[str, str],
        postgres_config: Dict[str, any],
        neo4j_config: Dict[str, any]
    ):
        self.service_urls = service_urls
        self.auth_headers = auth_headers
        self.postgres_config = postgres_config
        self.neo4j_config = neo4j_config
        self.project_id: Optional[str] = None
    
    def create_project(self, project_data: Dict[str, any]) -> str:
        """
        Create a test project and store its ID for cleanup.
        
        Args:
            project_data: Project creation data
            
        Returns:
            Created project ID
            
        Raises:
            AssertionError: If project creation fails
        """
        response = requests.post(
            f"{self.service_urls['project_management']}/projects",
            json=project_data,
            verify=False,
            headers=self.auth_headers,
            timeout=TestConstants.DEFAULT_TIMEOUT
        )
        
        assert response.status_code == TestConstants.HTTP_CREATED, (
            f"Failed to create project: {response.text}"
        )
        
        project_response = response.json()
        self.project_id = project_response["id"]
        return self.project_id
    
    def cleanup(self) -> None:
        """Clean up test data from all systems."""
        if not self.project_id:
            return

        # Clean up PostgreSQL
        try:
            conn = psycopg2.connect(**self.postgres_config)
            cursor = conn.cursor()
            # Delete only the project created by this test to avoid interfering with other tests
            cursor.execute("DELETE FROM projects WHERE id = %s", (self.project_id,))
            conn.commit()
            cursor.close()
            conn.close()
        except psycopg2.Error as e:
            print(f"Warning: Failed to clean up PostgreSQL data: {e}")

        # Clean up Neo4j
        try:
            driver = GraphDatabase.driver(
                self.neo4j_config["uri"],
                auth=(self.neo4j_config["username"], self.neo4j_config["password"])
            )
            with driver.session() as session:
                session.run(
                    "MATCH (n) WHERE n.project_id = $project_id DETACH DELETE n",
                    project_id=str(self.project_id)
                )
            driver.close()
        except (ConnectionError, RuntimeError) as e:
            print(f"Warning: Failed to clean up Neo4j data: {e}")


@pytest.fixture
def project_manager(
    service_urls: Dict[str, str], 
    auth_headers: Dict[str, str],
    postgres_config: Dict[str, any],
    neo4j_config: Dict[str, any]
) -> Generator[ProjectManager, None, None]:
    """
    Provide a project manager for creating and cleaning up test projects.
    
    This fixture depends on postgres_initialized to ensure the database
    is properly set up before creating projects.
    
    Args:
        service_urls: Service URL configuration
        auth_headers: Authentication headers
        postgres_config: PostgreSQL connection configuration
        neo4j_config: Neo4j connection configuration
        postgres_initialized: Ensures database is initialized
        
    Yields:
        ProjectManager instance
    """
    manager = ProjectManager(service_urls, auth_headers, postgres_config, neo4j_config)
    try:
        yield manager
    finally:
        manager.cleanup()


@pytest.fixture(scope="function")
def celery_task_monitor(redis_config: Dict[str, any]) -> RedisTestMonitor:
    """
    Provide RedisTestMonitor instance for task queue monitoring.
    
    Args:
        redis_config: Redis configuration fixture
        
    Returns:
        RedisTestMonitor instance configured for document_processing queue
    """
    return RedisTestMonitor(redis_config, queue_name='document_processing')


@pytest.fixture(scope="function")
def redis_monitor(redis_config: Dict[str, any]) -> RedisTestMonitor:
    """
    Provide RedisTestMonitor instance for comprehensive Redis monitoring.
    
    This fixture provides the same functionality as celery_task_monitor
    but with a more descriptive name for Redis pub/sub validation.
    
    Args:
        redis_config: Redis configuration fixture
        
    Returns:
        RedisTestMonitor instance configured for document_processing queue
    """
    return RedisTestMonitor(redis_config, queue_name='document_processing')


@pytest.fixture(scope="session")
def blob_storage_config(test_config: TestConfig) -> Dict[str, Any]:
    """
    Provide Azure Blob Storage configuration for tests that need to verify
    ingestion outputs in storage.
    """
    return test_config.get_blob_storage_config()


@pytest.fixture(scope="function")
def wa() -> WorkflowAssertions:
    """Provide a reusable WorkflowAssertions helper instance per test."""
    return WorkflowAssertions()


@pytest.fixture(scope="session")
def target_db_name(neo4j_config) -> str:
    """Standardized Neo4j database name for tests."""
    return neo4j_config.get("database", "neo4j")


@pytest.fixture(scope="session")
def cyphers_path() -> Path:
    """Locate the drift_search_cyphers.txt script across multiple candidate paths."""
    candidates = [
        Path(__file__).parent / ".." / "resources" / "drift_search_cyphers.txt",
        Path("/e2e-tests/resources/drift_search_cyphers.txt"),
        Path("/e2e-tests") / "resources" / "drift_search_cyphers.txt",
        Path.cwd() / "resources" / "drift_search_cyphers.txt",
    ]
    for p in candidates:
        p = p.resolve()
        if p.exists():
            return p
    raise AssertionError(f"Cypher script not found in any known location. Tried: {candidates}")

@pytest.fixture(scope="function", autouse=True)
def ensure_clean_session_setup(neo4j_driver, target_db_name, wa, service_urls):
    """Clean Neo4j database before each test and recreate schema."""
    wa.reset_neo4j_database(neo4j_driver, target_db_name)
    
    # Recreate constraints and indexes dropped by reset
    resp = requests.post(
        f"{service_urls['neo4j_repository']}/v1/maintenance/init-schema",
        headers={"Content-Type": "application/json"},
        json={},
        timeout=TestConstants.DEFAULT_TIMEOUT,
        verify=False
    )
    assert resp.status_code == TestConstants.HTTP_OK, f"Neo4j init failed: {resp.text}"
    yield
    wa.reset_neo4j_database(neo4j_driver, target_db_name)