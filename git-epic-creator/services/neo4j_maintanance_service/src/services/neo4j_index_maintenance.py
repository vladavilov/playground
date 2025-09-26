"""Neo4j Index Maintenance Service.

Handles automatic index maintenance, optimization, and health monitoring.
Uses the shared Neo4jClient for database operations.
"""

from dataclasses import dataclass
from datetime import datetime, timedelta
from typing import Any, Dict, List, Optional

import neo4j
import structlog
from neo4j.exceptions import ClientError, Neo4jError, ServiceUnavailable, TransientError
from .schema_query_builder import SchemaQueryBuilder

logger = structlog.get_logger(__name__)


@dataclass
class IndexHealthStatus:
    """Health status for a vector index."""
    index_name: str
    is_healthy: bool
    last_maintenance: datetime
    next_maintenance: datetime
    issues: List[str]
    recommendations: List[str]


@dataclass
class MaintenanceTask:
    """Maintenance task for an index."""
    task_id: str
    index_name: str
    task_type: str
    scheduled_time: datetime
    status: str
    description: str
    result: Optional[Dict[str, Any]] = None


class Neo4jIndexMaintenance:
    """Handles automatic Neo4j vector index maintenance."""

    def __init__(self, neo4j_client: Any, settings: Any) -> None:
        """Initialize the index maintenance service.

        Args:
            neo4j_client: Neo4j client from shared library
            settings: Configuration settings
        """
        self.neo4j_client = neo4j_client
        self.settings = settings
        self.maintenance_tasks: List[MaintenanceTask] = []
        self.last_maintenance_check = datetime.now()

        # Maintenance configuration
        self.maintenance_interval = timedelta(hours=24)  # Daily maintenance
        # Check health every 30 minutes
        self.health_check_interval = timedelta(minutes=30)
        self.max_maintenance_history = 100  # Keep last 100 maintenance tasks

        logger.info("Neo4j index maintenance service initialized")

    def ensure_community_indexes(self) -> Dict[str, Any]:
        """Ensure non-vector community indexes using SchemaQueryBuilder (DRY)."""
        operations: List[str] = []
        try:
            builder = SchemaQueryBuilder()
            statements = [
                s for s in builder.get_index_queries()
                if "community_" in s and ("INDEX" in s or "FULLTEXT" in s)
            ]
            with self.neo4j_client.get_session() as session:
                for stmt in statements:
                    session.run(stmt)
                    operations.append(stmt)
            logger.info("community_indexes_ensured", count=len(operations))
            return {"success": True, "operations": operations}
        except (Neo4jError, ClientError, ServiceUnavailable, TransientError) as e:
            logger.warning("community_indexes_ensure_failed", error=str(e))
            return {"success": False, "error": str(e), "operations": operations}

    def ensure_ingestion_constraints(self) -> Dict[str, Any]:
        """Ensure idempotent constraints required by ingestion/import flows.

        Creates constraints required for ingestion and removes legacy ones:
        - Create composite unique on `:__Community__(project_id, community)`
        - Unique on `:__Chunk__(id)`
        - Unique on `:__Document__(id)`
        - Unique on `:__Entity__(id)`
        - Unique on relationship `RELATED(id)`
        """
        statements = SchemaQueryBuilder().get_constraint_queries()
        executed: List[str] = []
        try:
            with self.neo4j_client.get_session() as session:
                for stmt in statements:
                    session.run(stmt)
                    executed.append(stmt)
            logger.info("ingestion_constraints_ensured", total=len(executed))
            return {"success": True, "executed": executed}
        except (Neo4jError, ClientError, ServiceUnavailable, TransientError) as e:
            logger.warning("ingestion_constraints_ensure_failed", error=str(e))
            return {"success": False, "error": str(e), "executed": executed}

    async def ensure_all_indexes(self) -> Dict[str, Any]:
        """Ensure all required indexes and constraints exist.

        Ensures: ingestion constraints (including composite community uniqueness),
        vector indexes (chunk/community/entity), and community indexes.
        """
        result: Dict[str, Any] = {"success": True}
        # Ensure constraints first
        constraints = self.ensure_ingestion_constraints()
        result["constraints"] = constraints
        if not constraints.get("success"):
            result["success"] = False
        # Ensure indexes via SchemaQueryBuilder (both vector and BTREE/FTS) in one place
        try:
            builder = SchemaQueryBuilder()
            statements = builder.get_index_queries()
            executed: List[str] = []
            with self.neo4j_client.get_session() as session:
                for stmt in statements:
                    session.run(stmt)
                    executed.append(stmt)
            result["indexes"] = {"success": True, "executed": executed}
        except (Neo4jError, ClientError, ServiceUnavailable, TransientError) as e:
            result["indexes"] = {"success": False, "error": str(e)}
            result["success"] = False
        return result

    async def check_index_health(self) -> List[IndexHealthStatus]:
        """
        Check the health of all relevant indexes (VECTOR + community BTREE/FTS).

        Returns:
            List[IndexHealthStatus]: Health status for each index
        """
        health_statuses: List[IndexHealthStatus] = []

        try:
            with self.neo4j_client.get_session() as session:
                # Get all indexes we care about
                result = session.run("""
                    SHOW INDEXES
                    YIELD name, state, populationPercent, type, entityType,
                          labelsOrTypes, properties
                """)

                for record in result:
                    index_name = record['name']
                    state = record['state']
                    population_percent = record.get('populationPercent') or 100.0
                    idx_type = record.get('type')

                    # Check index health
                    is_healthy = True
                    issues = []
                    recommendations = []

                    # Check if index is online
                    if state != 'ONLINE':
                        is_healthy = False
                        issues.append(f"Index state is {state}, expected ONLINE")
                        recommendations.append(
                            "Check index creation and population status"
                        )

                    # Check population percentage (only applies to VECTOR)
                    if idx_type == 'VECTOR' and population_percent < 100.0:
                        is_healthy = False
                        issues.append(
                            f"Index population at {population_percent}%, "
                            "expected 100%"
                        )
                        recommendations.append(
                            "Wait for index population to complete"
                        )

                    # Check for additional health indicators
                    additional_health = await self._check_additional_health_indicators(session, index_name)
                    if additional_health['issues']:
                        is_healthy = False
                        issues.extend(additional_health['issues'])
                        recommendations.extend(additional_health['recommendations'])

                    health_status = IndexHealthStatus(
                        index_name=index_name,
                        is_healthy=is_healthy,
                        last_maintenance=self._get_last_maintenance_time(index_name),
                        next_maintenance=self._calculate_next_maintenance_time(
                            index_name
                        ),
                        issues=issues,
                        recommendations=recommendations
                    )

                    health_statuses.append(health_status)

                logger.info(
                    "Completed index health check",
                    total_indexes=len(health_statuses),
                    healthy_indexes=len([h for h in health_statuses if h.is_healthy])
                )

        except (Neo4jError, ClientError, ServiceUnavailable) as e:
            logger.error("Failed to check index health", error=str(e))

        return health_statuses

    async def _check_additional_health_indicators(
        self, session: neo4j.Session, index_name: str
    ) -> Dict[str, List[str]]:
        """
        Check additional health indicators for an index.

        Args:
            session: Neo4j session
            index_name: Name of the index

        Returns:
            Dict[str, List[str]]: Issues and recommendations
        """
        result = {'issues': [], 'recommendations': []}

        try:
            # Check if index has been used recently
            index_stats = session.run("""
                SHOW INDEXES
                WHERE name = $indexName
                YIELD name, lastRead, readCount, trackedSince
            """, indexName=index_name)

            for record in index_stats:
                last_read = record['lastRead']
                read_count = record['readCount'] or 0
                tracked_since = record['trackedSince']

                # Check if index has been used
                if read_count == 0 and tracked_since:
                    time_since_tracked = datetime.now() - tracked_since
                    if time_since_tracked > timedelta(days=7):
                        result['issues'].append(
                            "Index has not been used in over 7 days"
                        )
                        result['recommendations'].append(
                            "Consider if this index is needed"
                        )

                # Check if index is stale
                if last_read and (datetime.now() - last_read) > timedelta(days=30):
                    result['issues'].append(
                        "Index has not been read in over 30 days"
                    )
                    result['recommendations'].append(
                        "Consider index cleanup or refresh"
                    )

            # Check for index configuration issues
            config_check = await self._check_index_configuration(index_name)
            result['issues'].extend(config_check['issues'])
            result['recommendations'].extend(config_check['recommendations'])

        except (Neo4jError, ClientError) as e:
            logger.debug(
                "Could not check additional health indicators",
                index_name=index_name,
                error=str(e)
            )

        return result

    async def _check_index_configuration(self, index_name: str) -> Dict[str, List[str]]:
        """Check index configuration for optimal settings.

        Args:
            index_name: Name of the index

        Returns:
            Dict[str, List[str]]: Issues and recommendations
        """
        result = {'issues': [], 'recommendations': []}

        try:
            with self.neo4j_client.get_session() as session:
                index_info = session.run("""
                    SHOW INDEXES
                    WHERE name = $indexName
                    YIELD name, type, entityType, labelsOrTypes, properties,
                          options, failureMessage, populationPercent,
                          bytesAndDocumentCount, valuesSelectivity
                """, indexName=index_name)

                for record in index_info:
                    self._validate_index_record(record, result)

            logger.debug("Index configuration check completed", index_name=index_name)

        except (Neo4jError, ClientError) as e:
            logger.debug(
                "Could not check index configuration",
                index_name=index_name,
                error=str(e)
            )
            result['recommendations'].append(
                "Unable to verify index configuration. "
                "Ensure index has proper vector settings"
            )

        return result

    def _validate_index_record(self, record: Dict[str, Any], result: Dict[str, List[str]]) -> None:
        """Validate a single index record configuration.
        
        Args:
            record: Index record from Neo4j
            result: Result dictionary to update with issues and recommendations
        """
        idx_type = record.get('type')
        if idx_type == 'VECTOR':
            options = record.get('options', {}) or {}
            entity_type = record.get('entityType', '')
            properties = record.get('properties', []) or []
            index_config = options.get('indexConfig', {})

            # Validate vector configuration
            self._check_hnsw_parameters(index_config, result)
            self._check_properties(properties, result)
            return

        # For BTREE/LOOKUP/FTS indexes, perform minimal validation
        entity_type = record.get('entityType', '')
        if entity_type not in ['NODE', 'RELATIONSHIP']:
            result['issues'].append(f"Unexpected entity type: {entity_type}")
        properties = record.get('properties', []) or []
        if not properties and idx_type != 'LOOKUP':
            result['issues'].append("No properties configured for index")

    def _check_hnsw_parameters(
        self, index_config: Dict[str, Any], result: Dict[str, List[str]]
    ) -> None:
        """Check HNSW parameters configuration."""
        # Check M parameter
        m_value = index_config.get('vector.hnsw.m')
        if m_value:
            if m_value < 8:
                result['recommendations'].append(
                    f"Low HNSW M value ({m_value}). "
                    "Consider M=16-32 for better recall"
                )
            elif m_value > 64:
                result['recommendations'].append(
                    f"High HNSW M value ({m_value}). "
                    "Consider M=16-32 for balanced performance"
                )
        else:
            result['recommendations'].append(
                "HNSW M parameter not configured. Consider setting M=16-32"
            )

        # Check ef_construction parameter
        ef_construction = index_config.get('vector.hnsw.ef_construction')
        if ef_construction:
            if ef_construction < 100:
                result['recommendations'].append(
                    f"Low ef_construction value ({ef_construction}). "
                    "Consider 200-400 for better index quality"
                )
            elif ef_construction > 1000:
                result['recommendations'].append(
                    f"High ef_construction value ({ef_construction}). "
                    "This may slow down index creation"
                )
        else:
            result['recommendations'].append(
                "HNSW ef_construction not configured. Consider setting 200-400"
            )


    def _check_properties(self, properties: List[str], result: Dict[str, List[str]]) -> None:
        """Check properties configuration."""
        if not properties:
            result['issues'].append("No properties configured for vector index")
        elif len(properties) > 1:
            result['recommendations'].append(
                f"Multiple properties ({len(properties)}) in vector index. "
                "Consider separate indexes for better performance"
            )
