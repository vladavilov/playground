"""
Workflow assertions facade for end-to-end tests.

This module provides a simplified facade over the modular service components,
following the Facade pattern to maintain backward compatibility while
delegating to specialized modules.

Design Pattern: Facade
- Simplifies interface to complex subsystem
- Maintains backward compatibility
- Delegates to specialized modules following SOLID principles
"""

from __future__ import annotations

from contextlib import contextmanager
from pathlib import Path
from typing import Any

from services.redis_test_monitor import RedisTestMonitor
from services.neo4j import Neo4jOperations, Neo4jValidators
from services.database import DatabaseValidators
from services.document import DocumentOperations
from services.workflow import StatusMonitor


class WorkflowAssertions:
    """
    Facade for workflow assertion operations.
    
    Provides backward-compatible interface while delegating to specialized modules:
    - Neo4jOperations: Database admin operations (reset, load scripts)
    - Neo4jValidators: Schema and data validation
    - DatabaseValidators: PostgreSQL validation
    - DocumentOperations: Document upload and verification
    - StatusMonitor: Status polling and sequence generation
    
    This class follows the Facade pattern to simplify the interface for tests
    while keeping the underlying implementation modular and maintainable.
    """

    # ----- Context Managers -----
    @contextmanager
    def ui_monitoring(self, project_id: str, redis_monitor: RedisTestMonitor):
        """
        Context manager to ensure UI monitoring is active during critical section.
        
        Args:
            project_id: Project UUID to monitor
            redis_monitor: Redis test monitor instance
            
        Yields:
            None (monitoring active during context)
        """
        redis_monitor.start_ui_progress_monitoring(project_id)
        try:
            yield
        finally:
            redis_monitor.stop_ui_progress_monitoring()

    # ----- Document Operations (delegated to DocumentOperations) -----
    def upload_document(self, project_id: str, fixtures, task_monitor):
        """Upload document and verify task publishing. Delegates to DocumentOperations."""
        return DocumentOperations.upload_document(project_id, fixtures, task_monitor)
    
    def prepare_multiple_files(self, project_id: str, test_pdf_content: bytes):
        """Prepare multiple files for batch upload. Delegates to DocumentOperations."""
        return DocumentOperations.prepare_multiple_files(project_id, test_pdf_content)
    
    def verify_upload_response(self, project_id: str, upload_result, fixtures):
        """Verify single document upload response. Delegates to DocumentOperations."""
        return DocumentOperations.verify_upload_response(project_id, upload_result, fixtures)
    
    def verify_multiple_upload_response(self, upload_result, project_id: str, expected_filenames: list):
        """Verify multiple document upload response. Delegates to DocumentOperations."""
        return DocumentOperations.verify_multiple_upload_response(upload_result, project_id, expected_filenames)
    
    def assert_upload_without_files(self, project_id: str, service_urls, auth_headers):
        """Verify error handling for upload without files. Delegates to DocumentOperations."""
        return DocumentOperations.assert_upload_without_files(project_id, service_urls, auth_headers)
    
    def assert_upload_to_nonexistent_project(self, service_urls, auth_headers):
        """Verify error handling for non-existent project. Delegates to DocumentOperations."""
        return DocumentOperations.assert_upload_to_nonexistent_project(service_urls, auth_headers)

    # ----- Status Monitoring (delegated to StatusMonitor) -----
    def wait_for_api_status(self, project_id: str, fixtures, expected_status: str, **kwargs):
        """Poll API until expected status reached. Delegates to StatusMonitor."""
        return StatusMonitor.wait_for_api_status(project_id, fixtures, expected_status, **kwargs)
    
    def expected_ui_sequence(self):
        """Get standard UI status sequence. Delegates to StatusMonitor."""
        return StatusMonitor.expected_ui_sequence()
    
    # ----- Database Validation (delegated to DatabaseValidators) -----
    def verify_db_status(self, project_id: str, fixtures, expected_status: str, **kwargs):
        """Verify project status in PostgreSQL. Delegates to DatabaseValidators."""
        return DatabaseValidators.verify_status(project_id, fixtures, expected_status, **kwargs)
    
    def verify_project_in_database(self, project_id: str, fixtures):
        """Verify project exists in database. Delegates to DatabaseValidators."""
        return DatabaseValidators.verify_project_exists(project_id, fixtures)

    # ----- Neo4j Validation (delegated to Neo4jValidators) -----
    def verify_neo4j_project_and_documents(self, project_id: str, fixtures, **kwargs):
        """Verify Neo4j graph structure. Delegates to Neo4jValidators."""
        return Neo4jValidators.verify_project_and_documents(project_id, fixtures, **kwargs)
    
    def verify_neo4j_constraints_minimal(self, fixtures):
        """Verify minimal required constraints. Delegates to Neo4jValidators."""
        return Neo4jValidators.verify_constraints_minimal(fixtures)
    
    def verify_neo4j_vector_index(self, fixtures):
        """Verify vector indexes are configured. Delegates to Neo4jValidators."""
        return Neo4jValidators.verify_vector_index(fixtures)
    
    def verify_required_index_names(self, driver: Any, **kwargs):
        """Verify specific indexes by name. Delegates to Neo4jValidators."""
        return Neo4jValidators.verify_required_index_names(driver, **kwargs)

    # ----- Neo4j Operations (delegated to Neo4jOperations) -----
    def reset_neo4j_database(self, driver: Any, database_name: str):
        """Reset Neo4j database to blank state. Delegates to Neo4jOperations."""
        return Neo4jOperations.reset_database(driver, database_name)
    
    def load_cypher_script(self, driver: Any, database_name: str, script_path: Path):
        """Load and execute cypher script. Delegates to Neo4jOperations."""
        return Neo4jOperations.load_cypher_script(driver, database_name, script_path)
    
    def poll_active_queries(self, driver: Any, duration_seconds: float = 2.0):
        """Monitor active queries. Delegates to Neo4jOperations."""
        return Neo4jOperations.poll_active_queries(driver, duration_seconds)
