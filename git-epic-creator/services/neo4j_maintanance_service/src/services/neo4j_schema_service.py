"""Neo4j Schema Service.

Executes schema operations with retry logic and error handling.
Uses the shared Neo4jClient for database operations.
"""

import asyncio
from typing import Any, Dict, List

import structlog
import neo4j
from neo4j.exceptions import Neo4jError

from utils.neo4j_client import Neo4jClient, Neo4jHealthChecker

logger = structlog.get_logger(__name__)


class Neo4jSchemaService:
    """Handles Neo4j schema operations."""

    def __init__(self, neo4j_client: Neo4jClient, query_builder: Any, settings: Any) -> None:
        """Initialize the schema service.

        Args:
            neo4j_client: Neo4j client from shared library
            query_builder: Schema query builder
            settings: Configuration settings
        """
        self.neo4j_client = neo4j_client
        self.query_builder = query_builder
        self.settings = settings
        logger.info("Neo4j schema service initialized")

    async def execute_query(self, query: str) -> Dict[str, Any]:
        """Execute a single query with retry logic.

        Args:
            query: Cypher query to execute

        Returns:
            Dict[str, Any]: Execution result
        """
        for attempt in range(self.settings.RETRY_MAX_ATTEMPTS):
            try:
                with self.neo4j_client.get_session() as session:
                    result = session.run(query)
                    result.consume()

                logger.info("Query executed successfully", query=query, attempt=attempt + 1)
                return {
                    "success": True,
                    "query": query,
                    "attempts": attempt + 1
                }

            except (Neo4jError, OSError, ConnectionError) as e:
                if attempt == self.settings.RETRY_MAX_ATTEMPTS - 1:
                    # Last attempt failed
                    logger.error("Query failed after all retries",
                                query=query, error=str(e), attempts=attempt + 1)
                    return {
                        "success": False,
                        "query": query,
                        "attempts": attempt + 1,
                        "error": str(e)
                    }

                # Retry with delay
                logger.warning("Query failed, retrying",
                              query=query, error=str(e), attempt=attempt + 1)
                await asyncio.sleep(self.settings.RETRY_BACKOFF_BASE_SEC)

    async def execute_queries_batch(self, queries: List[str]) -> Dict[str, Any]:
        """Execute a batch of queries with shared flow."""
        return await self._run_batch(queries)

    async def _run_batch(self, queries: List[str]) -> Dict[str, Any]:
        results: Dict[str, Any] = {
            "executed_queries": [],
            "failed_queries": [],
            "total_queries": len(queries or []),
        }
        for query in (queries or []):
            item = await self.execute_query(query)
            if item.get("success"):
                results["executed_queries"].append(item)
            else:
                results["failed_queries"].append(item)
        executed_count = len(results["executed_queries"])
        total_count = results["total_queries"]
        results["success_rate"] = self._calculate_success_rate(executed_count, total_count)
        logger.info(
            "Batch execution completed",
            total=total_count,
            executed=executed_count,
            failed=len(results["failed_queries"]),
            success_rate=results["success_rate"],
        )
        return results

    async def initialize_schema(self) -> Dict[str, Any]:
        """
        Initializes the Neo4j schema by creating constraints and indexes.
        It is idempotent and will not create duplicates.
        """
        logger.info("Starting Neo4j schema initialization")

        # Check for database connectivity
        health_result = await Neo4jHealthChecker.check_health(self.neo4j_client)
        if not health_result:
            logger.error("Neo4j health check failed. Aborting schema initialization.")
            raise neo4j.exceptions.Neo4jError(
                "Neo4j connection verification failed"
            )

        constraint_queries = self.query_builder.get_constraint_queries()
        index_queries = self.query_builder.get_index_queries()

        # Execute constraint queries
        constraint_results = await self.execute_queries_batch(constraint_queries)

        # Execute index queries
        index_results = await self.execute_queries_batch(index_queries)

        # Execute relationship type queries
        relationship_type_queries = self.query_builder.get_relationship_type_queries()
        relationship_type_results = await self.execute_queries_batch(relationship_type_queries)

        # Calculate overall results
        total_executed = (len(constraint_results["executed_queries"]) +
                        len(index_results["executed_queries"]) +
                        len(relationship_type_results["executed_queries"]))
        total_failed = (len(constraint_results["failed_queries"]) +
                      len(index_results["failed_queries"]) +
                      len(relationship_type_results["failed_queries"]))
        total_queries = (constraint_results["total_queries"] +
                       index_results["total_queries"] +
                       relationship_type_results["total_queries"])

        success_rate = self._calculate_success_rate(total_executed, total_queries)
        success = total_failed == 0

        result = {
            "success": success,
            "constraints": constraint_results,
            "indexes": index_results,
            "relationship_types": relationship_type_results,
            "summary": {
                "total_queries": total_queries,
                "executed_successfully": total_executed,
                "failed": total_failed,
                "success_rate": success_rate
            }
        }

        if success:
            logger.info("Neo4j schema initialization completed successfully",
                       summary=result["summary"])
        else:
            logger.warning("Neo4j schema initialization completed with failures",
                          summary=result["summary"])

        return result

    def _calculate_success_rate(self, executed: int, total: int) -> str:
        """Calculate success rate percentage.

        Args:
            executed: Number of successfully executed queries
            total: Total number of queries

        Returns:
            str: Success rate as percentage string
        """
        if total == 0:
            return "0%"

        rate = (executed / total) * 100
        return f"{rate:.1f}%"
