"""
Neo4j administrative operations for e2e tests.

This module provides database administration operations following
Single Responsibility Principle - focused solely on Neo4j admin tasks.
"""

from __future__ import annotations

from typing import Any
from pathlib import Path

from .queries import Neo4jQueries
from services.cypher_loader import execute_cypher_script


class Neo4jOperations:
    """
    Administrative operations for Neo4j database management.
    
    Provides operations for:
    - Database reset and cleanup
    - Cypher script loading
    - Transaction monitoring
    """
    
    @staticmethod
    def reset_database(driver: Any, database_name: str) -> None:
        """
        Remove all nodes/relationships and drop all indexes/constraints.
        
        This is a destructive operation that completely clears the database
        and resets its schema to a blank state.
        
        Args:
            driver: Neo4j driver instance
            database_name: Target database name
            
        Note:
            Uses APOC procedures for schema reset. Ensure APOC is installed.
        """
        with driver.session(database=database_name) as session:
            session.run(Neo4jQueries.DELETE_ALL_NODES).consume()
            session.run(Neo4jQueries.RESET_SCHEMA).consume()
    
    @staticmethod
    def load_cypher_script(driver: Any, database_name: str, script_path: Path) -> None:
        """
        Execute a cypher script file with transaction support.
        
        Delegates to cypher_loader for script execution, supporting:
        - :begin/:commit transaction markers
        - Multi-line statements
        - Semicolon-delimited statements
        
        Args:
            driver: Neo4j driver instance
            database_name: Target database name
            script_path: Path to .cypher script file
            
        Raises:
            FileNotFoundError: If script_path doesn't exist
            Neo4jError: If script execution fails
        """
        execute_cypher_script(driver, database_name, script_path)
    
    @staticmethod
    def poll_active_queries(driver: Any, duration_seconds: float = 2.0) -> list[str]:
        """
        Monitor active queries for specified duration.
        
        Useful for debugging and performance analysis - captures all queries
        executed during the polling window.
        
        Args:
            driver: Neo4j driver instance
            duration_seconds: How long to monitor (default: 2.0 seconds)
            
        Returns:
            List of unique query strings observed during polling period
            
        Note:
            Uses SHOW TRANSACTIONS which requires Neo4j 4.4+
        """
        import time as _time
        
        deadline = _time.time() + duration_seconds
        seen: list[str] = []
        
        while _time.time() < deadline:
            try:
                with driver.session() as session:
                    rows = list(session.run(Neo4jQueries.SHOW_TRANSACTIONS))
                    for r in rows:
                        q = r.get("currentQuery") or ""
                        if q and q not in seen:
                            seen.append(q)
            except Exception:
                # Ignore errors during monitoring
                pass
            _time.sleep(0.05)
        
        return seen
    
    @staticmethod
    def cleanup_project_data(driver: Any, project_id: str, database_name: str = "neo4j") -> None:
        """
        Remove all nodes associated with a specific project.
        
        Args:
            driver: Neo4j driver instance
            project_id: Project UUID to clean up
            database_name: Target database name (default: "neo4j")
            
        Note:
            Uses project_id property to scope deletion
        """
        with driver.session(database=database_name) as session:
            session.run(
                Neo4jQueries.DELETE_PROJECT_NODES,
                project_id=str(project_id)
            ).consume()

