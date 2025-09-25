"""
Service health and connectivity tests.

This module contains tests to verify that all required services are running,
accessible, and can be connected to properly.
"""

from typing import Dict
import os

import pytest
import requests
from neo4j import GraphDatabase
import redis

from config import TestConstants
from shared_utils import HTTPUtils, ServiceHealthChecker, DatabaseUtils


class TestServicesHealth:
    """Test suite for verifying service health and connectivity."""

    def test_postgres_connectivity(self, postgres_connection) -> None:
        """Test PostgreSQL database connectivity and basic operations."""
        # Test basic query execution
        result = DatabaseUtils.execute_query_with_retry(
            postgres_connection, "SELECT 1 as test_value"
        )
        assert result[0][0] == 1
        
        # Test that we can query the projects table (assuming it exists)
        result = DatabaseUtils.execute_query_with_retry(
            postgres_connection, "SELECT COUNT(*) FROM projects"
        )
        assert isinstance(result[0][0], int)

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

    def test_all_services_accessible(self, service_urls: Dict[str, str]) -> None:
        """Test that all configured services are accessible."""
        health_status = ServiceHealthChecker.check_all_services_health(service_urls)
        
        failed_services = [
            service_name for service_name, is_healthy in health_status.items()
            if not is_healthy
        ]
        
        if failed_services:
            pytest.fail(f"The following services are not healthy: {', '.join(failed_services)}")


class TestDatabaseIntegrity:
    """Test suite for verifying database integrity and schema."""

    def test_postgres_schema_exists(self, postgres_connection) -> None:
        """Test that expected PostgreSQL schema exists."""
        # Check that we can connect to the expected database
        result = DatabaseUtils.execute_query_with_retry(
            postgres_connection, "SELECT current_database()"
        )
        current_db = result[0][0]
        assert current_db is not None
        
        # Check for expected tables (adjust based on actual schema)
        tables_result = DatabaseUtils.execute_query_with_retry(
            postgres_connection, """
                SELECT table_name 
                FROM information_schema.tables 
                WHERE table_schema = 'public' 
                AND table_type = 'BASE TABLE'
            """
        )
        tables = [row[0] for row in tables_result]
        
        # We expect specific core tables to exist after database initialization
        expected_tables = ['projects', 'project_members']
        for expected_table in expected_tables:
            assert expected_table in tables, (
                f"Required table '{expected_table}' not found in database. "
                f"Available tables: {tables}. "
                f"Database initialization may have failed."
            )
            
            # Verify we can query the table using utility
            assert DatabaseUtils.verify_table_exists(postgres_connection, expected_table), (
                f"Failed to query {expected_table} table"
            )

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