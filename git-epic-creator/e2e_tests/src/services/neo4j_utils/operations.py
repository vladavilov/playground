"""
Neo4j operations for e2e tests.

This module provides Neo4j administrative and operational functionality following
Single Responsibility Principle.
"""

from __future__ import annotations

import time as _time
from pathlib import Path
from typing import Any, List

from ..cypher_loader import execute_cypher_script


class Neo4jOperations:
    """
    Neo4j administrative and operational functions.
    
    Provides operations for:
    - Database reset and cleanup
    - Cypher script execution
    - Active query monitoring
    """

    @staticmethod
    def reset_database(driver: Any, database_name: str) -> None:
        """
        Remove all nodes/relationships and drop provided indexes if exist.

        Keeps behaviour identical to existing drift-search setup.
        
        Args:
            driver: Neo4j driver instance
            database_name: Target database name
        """
        with driver.session(database=database_name) as session:
            session.run("MATCH (n) DETACH DELETE n").consume()
            session.run("CALL apoc.schema.assert({}, {})").consume()

    @staticmethod
    def load_cypher_script(driver: Any, database_name: str, script_path: Path) -> None:
        """
        Load and execute a Cypher script file.
        
        Args:
            driver: Neo4j driver instance
            database_name: Target database name
            script_path: Path to the Cypher script file
        """
        execute_cypher_script(driver, database_name, script_path)

    @staticmethod
    def poll_active_queries(driver: Any, duration_seconds: float = 2.0) -> List[str]:
        """
        Monitor active queries for a specified duration.
        
        Args:
            driver: Neo4j driver instance
            duration_seconds: How long to monitor (default: 2.0)
            
        Returns:
            List of unique queries seen during monitoring period
        """
        deadline = _time.time() + duration_seconds
        seen: List[str] = []
        while _time.time() < deadline:
            try:
                with driver.session() as session:
                    rows = list(session.run("SHOW TRANSACTIONS YIELD currentQuery RETURN currentQuery"))
                    for r in rows:
                        q = r.get("currentQuery") or ""
                        if q and q not in seen:
                            seen.append(q)
            except Exception:
                pass
            _time.sleep(0.05)
        return seen
