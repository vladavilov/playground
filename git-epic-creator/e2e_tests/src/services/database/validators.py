"""
PostgreSQL validation operations for e2e tests.

This module provides database validation operations following
Single Responsibility Principle.
"""

from __future__ import annotations

from typing import Dict, Any

from services.workflow_models import WorkflowTestFixtures


class DatabaseValidators:
    """
    Validation operations for PostgreSQL database integrity.
    
    Provides validators for:
    - Project existence and properties
    - Status verification
    - Progress tracking (processed_pct)
    """
    
    @staticmethod
    def verify_status(
        project_id: str,
        fixtures: WorkflowTestFixtures,
        expected_status: str,
        *,
        min_pct: float | None = None,
    ) -> None:
        """
        Verify project status in PostgreSQL database.
        
        Args:
            project_id: Project UUID to validate
            fixtures: Test fixtures with postgres_connection
            expected_status: Expected status value
            min_pct: If provided, also validates processed_pct >= min_pct
            
        Raises:
            AssertionError: If status doesn't match or project not found
        """
        cursor = fixtures.postgres_connection.cursor()
        
        if min_pct is None:
            cursor.execute(
                "SELECT status FROM projects WHERE id = %s",
                (project_id,)
            )
            row = cursor.fetchone()
            cursor.close()
            assert row is not None, "Project not found in database"
            assert row[0] == expected_status, (
                f"Expected DB status '{expected_status}', got '{row[0]}'"
            )
        else:
            cursor.execute(
                "SELECT status, processed_pct FROM projects WHERE id = %s",
                (project_id,)
            )
            row = cursor.fetchone()
            cursor.close()
            assert row is not None, "Project not found in database"
            assert row[0] == expected_status, (
                f"Expected DB status '{expected_status}', got '{row[0]}'"
            )
            assert float(row[1] or 0) >= float(min_pct), (
                f"Expected processed_pct >= {min_pct}, got {row[1]}"
            )
    
    @staticmethod
    def verify_project_exists(project_id: str, fixtures: WorkflowTestFixtures) -> None:
        """
        Verify project exists in database with expected properties.
        
        Args:
            project_id: Project UUID to validate
            fixtures: Test fixtures with postgres_connection and test_project_data
            
        Raises:
            AssertionError: If project not found or properties don't match
        """
        cursor = fixtures.postgres_connection.cursor()
        cursor.execute(
            "SELECT id, name, status FROM projects WHERE id = %s",
            (project_id,)
        )
        db_project = cursor.fetchone()
        cursor.close()
        
        assert db_project is not None, "Project not found in database"
        assert str(db_project[0]) == project_id
        assert db_project[1] == fixtures.test_project_data["name"]
        assert db_project[2] == fixtures.test_project_data["status"]

