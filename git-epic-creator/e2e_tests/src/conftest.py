"""
Shared pytest fixtures and utilities for end-to-end tests.

This module provides reusable fixtures for authentication, database connections,
service health checks, and test data management.
"""

import time
import uuid
from typing import Dict, Generator, Optional

import pytest
import requests
import psycopg2
from neo4j import GraphDatabase
import redis

from config import TestConfig, TestConstants


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
    def wait_for_service_health(base_url: str, endpoint: str = TestConstants.HEALTH_ENDPOINT) -> bool:
        """Wait for a single service to become healthy."""
        start_time = time.time()
        while time.time() - start_time < TestConstants.SERVICE_HEALTH_TIMEOUT:
            try:
                response = requests.get(
                    f"{base_url}{endpoint}", 
                    timeout=TestConstants.DEFAULT_TIMEOUT
                )
                if response.status_code == TestConstants.HTTP_OK:
                    return True
            except requests.RequestException:
                pass
            time.sleep(2)
        return False

    # Check all critical services
    services_to_check = [
        (service_urls["project_management"], TestConstants.HEALTH_ENDPOINT),
        (service_urls["document_processing"], TestConstants.HEALTH_ENDPOINT),
        (service_urls["neo4j_ingestion"], TestConstants.HEALTH_ENDPOINT),
        (service_urls["mock_auth"], TestConstants.HEALTH_ENDPOINT),
        (service_urls["init_db_service"], TestConstants.HEALTH_ENDPOINT)
    ]

    failed_services = []
    for base_url, endpoint in services_to_check:
        if not wait_for_service_health(base_url, endpoint):
            failed_services.append(base_url)

    if failed_services:
        pytest.skip(
            f"Skipping tests because the following services are not available: "
            f"{', '.join(failed_services)}. Make sure services are running before running tests."
        )


@pytest.fixture(scope="session")
def postgres_initialized(service_urls: Dict[str, str], services_ready) -> bool:
    """
    Initialize PostgreSQL database once before all tests.
    
    This fixture calls the init_db_service to set up the database schema
    and initial data before any tests run.
    
    Args:
        service_urls: Service URL configuration
        services_ready: Ensures services are healthy before initialization
        
    Returns:
        True if initialization successful
        
    Raises:
        pytest.fail: If initialization fails
    """
    try:
        response = requests.post(
            f"{service_urls['init_db_service']}/db/init",
            timeout=TestConstants.DEFAULT_TIMEOUT
        )

        if response.status_code == TestConstants.HTTP_OK:
            return True

        pytest.fail(
            f"Failed to initialize PostgreSQL database: "
            f"[{service_urls['init_db_service']}/init]"
            f"{response.status_code} - {response.text}"
        )
    except requests.RequestException as e:
        pytest.fail(f"Error initializing PostgreSQL database: {e}")


@pytest.fixture
def auth_headers(service_urls: Dict[str, str], auth_config: Dict[str, str]) -> Dict[str, str]:
    """
    Get authentication headers by requesting a token from the mock auth service.
    
    Returns:
        Dict with Authorization header containing Bearer token
    """
    try:
        token_response = requests.post(
            f"{service_urls['mock_auth']}/{auth_config['tenant_id']}/oauth2/v2.0/token",
            data={
                "grant_type": "client_credentials",
                "client_id": auth_config["client_id"],
                "client_secret": auth_config["client_secret"],
                "scope": auth_config["scope"]
            },
            timeout=TestConstants.DEFAULT_TIMEOUT
        )

        if token_response.status_code == TestConstants.HTTP_OK:
            token_data = token_response.json()
            return {"Authorization": f"Bearer {token_data['access_token']}"}

        pytest.fail(
            f"Failed to get token from mock service: "
            f"{token_response.status_code} - {token_response.text}"
        )
    except requests.RequestException as e:
        pytest.fail(f"Error getting token from mock service: {e}")


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
    neo4j_config: Dict[str, any],
    postgres_initialized: bool
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


def wait_for_document_processing(
    project_id: str,
    service_urls: Dict[str, str],
    auth_headers: Dict[str, str],
    redis_config: Dict[str, any],
    timeout: int = TestConstants.DOCUMENT_PROCESSING_TIMEOUT
) -> None:
    """
    Wait for document processing to complete by monitoring Redis/Celery and project status.
    
    Args:
        project_id: ID of the project being processed
        service_urls: Service URL configuration
        auth_headers: Authentication headers
        redis_config: Redis configuration
        timeout: Maximum time to wait in seconds
        
    Raises:
        AssertionError: If processing doesn't complete within timeout
    """
    redis_client = redis.Redis(**redis_config)
    start_time = time.time()

    while time.time() - start_time < timeout:
        try:
            # Check if there are any active Celery tasks
            active_tasks = redis_client.llen('celery')
            if active_tasks == 0:
                # Also check project status
                response = requests.get(
                    f"{service_urls['project_management']}/projects/{project_id}",
                    headers=auth_headers,
                    timeout=TestConstants.DEFAULT_TIMEOUT
                )
                if response.status_code == TestConstants.HTTP_OK:
                    project = response.json()
                    if project["status"] != TestConstants.PROJECT_STATUS_PROCESSING:
                        return  # Processing completed

        except (redis.RedisError, requests.RequestException) as e:
            print(f"Warning: Error checking processing status: {e}")

        time.sleep(5)

    raise AssertionError(f"Document processing did not complete within {timeout} seconds") 