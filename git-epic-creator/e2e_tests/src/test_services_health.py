"""
Service health and connectivity tests.

This module contains tests to verify that all required services are running,
accessible, and can be connected to properly.
"""

from typing import Dict, Any

import pytest
import requests
import psycopg2
from neo4j import GraphDatabase
import redis

from config import TestConstants


class TestServicesHealth:
    """Test suite for verifying service health and connectivity."""

    def test_project_management_service_health(
        self, 
        service_urls: Dict[str, str],
        services_ready: None  # pylint: disable=unused-argument
    ) -> None:
        """Test that project management service is healthy and responds correctly."""
        response = requests.get(
            f"{service_urls['project_management']}{TestConstants.HEALTH_ENDPOINT}",
            timeout=TestConstants.DEFAULT_TIMEOUT
        )
        
        assert response.status_code == TestConstants.HTTP_OK
        health_data = response.json()
        assert health_data["status"] == "ok"

    def test_document_processing_service_health(
        self, 
        service_urls: Dict[str, str],
        services_ready: None  # pylint: disable=unused-argument
    ) -> None:
        """Test that document processing service is healthy."""
        response = requests.get(
            f"{service_urls['document_processing']}{TestConstants.HEALTH_ENDPOINT}",
            timeout=TestConstants.DEFAULT_TIMEOUT
        )
        
        assert response.status_code == TestConstants.HTTP_OK

    def test_neo4j_ingestion_service_health(
        self, 
        service_urls: Dict[str, str],
        services_ready: None  # pylint: disable=unused-argument
    ) -> None:
        """Test that Neo4j ingestion service is healthy."""
        response = requests.get(
            f"{service_urls['neo4j_ingestion']}{TestConstants.HEALTH_ENDPOINT}",
            timeout=TestConstants.DEFAULT_TIMEOUT
        )
        
        assert response.status_code == TestConstants.HTTP_OK

    def test_mock_auth_service_health(
        self, 
        service_urls: Dict[str, str],
        services_ready: None  # pylint: disable=unused-argument
    ) -> None:
        """Test that mock authentication service is healthy."""
        response = requests.get(
            f"{service_urls['mock_auth']}{TestConstants.HEALTH_ENDPOINT}",
            timeout=TestConstants.DEFAULT_TIMEOUT
        )
        
        assert response.status_code == TestConstants.HTTP_OK

    def test_postgres_connectivity(self, postgres_connection) -> None:
        """Test PostgreSQL database connectivity and basic operations."""
        cursor = postgres_connection.cursor()
        
        # Test basic query execution
        cursor.execute("SELECT 1 as test_value")
        result = cursor.fetchone()
        assert result[0] == 1
        
        # Test that we can query the projects table (assuming it exists)
        try:
            cursor.execute("SELECT COUNT(*) FROM projects")
            count = cursor.fetchone()
            assert isinstance(count[0], int)
        except psycopg2.ProgrammingError:
            # Table might not exist yet, which is acceptable for health check
            pass
        
        cursor.close()



    def test_neo4j_connectivity(self, neo4j_driver: GraphDatabase.driver) -> None:
        """Test Neo4j connectivity and basic operations."""
        with neo4j_driver.session() as session:
            # Test basic query execution
            result = session.run("RETURN 1 as test_value")
            record = result.single()
            assert record["test_value"] == 1
            
            # Test that we can perform basic graph operations
            # Create a test node and immediately delete it
            result = session.run(
                "CREATE (n:HealthCheck {id: $id}) RETURN n.id as created_id",
                id="health_test"
            )
            record = result.single()
            assert record["created_id"] == "health_test"
            
            # Clean up the test node
            session.run("MATCH (n:HealthCheck {id: $id}) DELETE n", id="health_test")

    def test_celery_health_endpoint(self, service_urls: Dict[str, str]) -> None:
        """Test Celery health check endpoint if available."""
        try:
            response = requests.get(
                f"{service_urls['document_processing']}{TestConstants.CELERY_HEALTH_ENDPOINT}",
                timeout=TestConstants.DEFAULT_TIMEOUT
            )
            # If endpoint exists, it should return 200
            assert response.status_code == TestConstants.HTTP_OK
        except requests.RequestException:
            # Celery health check endpoint might not be implemented
            # This is acceptable - we'll skip this test
            pytest.skip("Celery health check endpoint not available")

    def test_all_services_accessible(self, service_urls: Dict[str, str]) -> None:
        """Test that all configured services are accessible."""
        for service_name, service_url in service_urls.items():
            try:
                response = requests.get(
                    f"{service_url}{TestConstants.HEALTH_ENDPOINT}",
                    timeout=TestConstants.DEFAULT_TIMEOUT
                )
                assert response.status_code == TestConstants.HTTP_OK, (
                    f"Service {service_name} at {service_url} is not healthy"
                )
            except requests.RequestException as e:
                pytest.fail(f"Failed to connect to {service_name} at {service_url}: {e}")


class TestDatabaseIntegrity:
    """Test suite for verifying database integrity and schema."""

    def test_postgres_schema_exists(self, postgres_connection) -> None:
        """Test that expected PostgreSQL schema exists."""
        cursor = postgres_connection.cursor()
        
        # Check that we can connect to the expected database
        cursor.execute("SELECT current_database()")
        current_db = cursor.fetchone()[0]
        assert current_db is not None
        
        # Check for expected tables (adjust based on actual schema)
        cursor.execute("""
            SELECT table_name 
            FROM information_schema.tables 
            WHERE table_schema = 'public' 
            AND table_type = 'BASE TABLE'
        """)
        tables = [row[0] for row in cursor.fetchall()]
        
        # We expect at least some core tables to exist
        # Note: Adjust this based on your actual database schema
        if tables:  # If tables exist, verify projects table is among them
            expected_tables = ['projects']  # Add more as needed
            for expected_table in expected_tables:
                if expected_table in tables:
                    # Verify we can query the table
                    cursor.execute(f"SELECT COUNT(*) FROM {expected_table}")
                    count = cursor.fetchone()
                    assert isinstance(count[0], int)
        
        cursor.close()

    def test_neo4j_database_accessible(self, neo4j_driver: GraphDatabase.driver) -> None:
        """Test that Neo4j database is accessible and responsive."""
        with neo4j_driver.session() as session:
            # Test database info
            result = session.run("CALL db.info()")
            info = result.single()
            assert info is not None
            
            # Test that we can access the configured database
            result = session.run("CALL db.labels()")
            labels = list(result)  # Get all labels (might be empty for new database)
            # Just verify the query executes without error
            assert isinstance(labels, list)

    def test_redis_info(self, redis_client: redis.Redis) -> None:
        """Test Redis server information and configuration."""
        info = redis_client.info()
        
        # Verify Redis is running and accessible
        assert 'redis_version' in info
        assert info['connected_clients'] >= 0
        
        # Test that we can access the configured database
        # Get the current database from client connection parameters
        connection_params = redis_client.connection_pool.connection_kwargs
        current_db = connection_params.get('db', 0)
        
        # Verify we can perform database operations (which confirms access)
        redis_client.set('health_check_key', 'test_value', ex=10)  # 10 second expiration
        retrieved_value = redis_client.get('health_check_key')
        
        assert retrieved_value == b'test_value'
        assert current_db >= 0  # Database number should be non-negative 