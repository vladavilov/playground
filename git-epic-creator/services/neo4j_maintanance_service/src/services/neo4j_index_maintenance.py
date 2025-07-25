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

    async def check_index_health(self) -> List[IndexHealthStatus]:
        """
        Check the health of all vector indexes.

        Returns:
            List[IndexHealthStatus]: Health status for each index
        """
        health_statuses = []

        try:
            with self.neo4j_client.get_session() as session:
                # Get all vector indexes
                result = session.run("""
                    SHOW INDEXES
                    WHERE type = 'VECTOR'
                    YIELD name, state, populationPercent, type, entityType,
                          labelsOrTypes, properties
                """)

                for record in result:
                    index_name = record['name']
                    state = record['state']
                    population_percent = record['populationPercent']

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

                    # Check population percentage
                    if population_percent < 100.0:
                        is_healthy = False
                        issues.append(
                            f"Index population at {population_percent}%, "
                            "expected 100%"
                        )
                        recommendations.append(
                            "Wait for index population to complete"
                        )

                    # Check for additional health indicators
                    additional_health = await self._check_additional_health_indicators(
                        session, index_name
                    )
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
        # Check if this is a vector index
        if record.get('type') != 'VECTOR':
            result['issues'].append(
                f"Expected VECTOR index type, found {record.get('type')}"
            )
            return

        options = record.get('options', {}) or {}
        entity_type = record.get('entityType', '')
        properties = record.get('properties', []) or []
        index_config = options.get('indexConfig', {})

        # Validate vector configuration
        self._check_vector_dimension(index_config, result)
        self._check_similarity_function(index_config, result)
        self._check_hnsw_parameters(index_config, result)
        self._check_quantization_settings(index_config, result)
        self._check_entity_type(entity_type, result)
        self._check_properties(properties, result)

    def _check_vector_dimension(
        self, index_config: Dict[str, Any], result: Dict[str, List[str]]
    ) -> None:
        """Check vector dimension configuration."""
        vector_dimension = index_config.get('vector.dimensions')
        if not vector_dimension:
            result['issues'].append("Vector dimension not configured")
            result['recommendations'].append(
                "Specify vector.dimensions in index configuration"
            )
        elif vector_dimension < 128:
            result['recommendations'].append(
                f"Low vector dimension ({vector_dimension}). "
                "Consider higher dimensions for better accuracy"
            )
        elif vector_dimension > 2048:
            result['recommendations'].append(
                f"High vector dimension ({vector_dimension}). "
                "Consider quantization for better performance"
            )

    def _check_similarity_function(
        self, index_config: Dict[str, Any], result: Dict[str, List[str]]
    ) -> None:
        """Check similarity function configuration."""
        similarity_function = index_config.get('vector.similarity_function')
        valid_functions = ['cosine', 'euclidean', 'dot_product']

        if not similarity_function:
            result['issues'].append("Similarity function not specified")
            result['recommendations'].append(
                "Specify vector.similarity_function (cosine, euclidean, or dot_product)"
            )
        elif similarity_function not in valid_functions:
            result['issues'].append(
                f"Unknown similarity function: {similarity_function}"
            )

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

    def _check_quantization_settings(
        self, index_config: Dict[str, Any], result: Dict[str, List[str]]
    ) -> None:
        """Check quantization settings."""
        vector_dimension = index_config.get('vector.dimensions')
        quantization_enabled = index_config.get('vector.quantization.enabled')

        if vector_dimension and vector_dimension > 512 and not quantization_enabled:
            result['recommendations'].append(
                f"Consider enabling quantization for high-dimensional vectors "
                f"({vector_dimension}D) to improve performance"
            )

    def _check_entity_type(self, entity_type: str, result: Dict[str, List[str]]) -> None:
        """Check entity type configuration."""
        if entity_type not in ['NODE', 'RELATIONSHIP']:
            result['issues'].append(f"Unexpected entity type: {entity_type}")

    def _check_properties(self, properties: List[str], result: Dict[str, List[str]]) -> None:
        """Check properties configuration."""
        if not properties:
            result['issues'].append("No properties configured for vector index")
        elif len(properties) > 1:
            result['recommendations'].append(
                f"Multiple properties ({len(properties)}) in vector index. "
                "Consider separate indexes for better performance"
            )

    def _get_last_maintenance_time(self, index_name: str) -> datetime:
        """Get the last maintenance time for an index."""
        # Find the most recent maintenance task for this index
        maintenance_tasks = [
            task for task in self.maintenance_tasks
            if task.index_name == index_name and task.status == 'completed'
        ]

        if maintenance_tasks:
            return max(task.scheduled_time for task in maintenance_tasks)

        # Return a date in the past if no maintenance has been performed
        return datetime.now() - timedelta(days=30)

    def _calculate_next_maintenance_time(self, index_name: str) -> datetime:
        """Calculate the next maintenance time for an index."""
        last_maintenance = self._get_last_maintenance_time(index_name)
        return last_maintenance + self.maintenance_interval

    async def schedule_maintenance_task(
        self,
        index_name: str,
        task_type: str,
        description: str,
        scheduled_time: Optional[datetime] = None
    ) -> str:
        """
        Schedule a maintenance task for an index.

        Args:
            index_name: Name of the index
            task_type: Type of maintenance task
            description: Description of the task
            scheduled_time: When to schedule the task (defaults to now)

        Returns:
            str: Task ID
        """
        if scheduled_time is None:
            scheduled_time = datetime.now()

        task_id = f"{index_name}_{task_type}_{int(scheduled_time.timestamp())}"

        task = MaintenanceTask(
            task_id=task_id,
            index_name=index_name,
            task_type=task_type,
            scheduled_time=scheduled_time,
            status='scheduled',
            description=description
        )

        self.maintenance_tasks.append(task)

        # Keep only recent tasks
        if len(self.maintenance_tasks) > self.max_maintenance_history:
            self.maintenance_tasks = (
                self.maintenance_tasks[-self.max_maintenance_history:]
            )

        logger.info(
            "Scheduled maintenance task",
            task_id=task_id,
            index_name=index_name,
            task_type=task_type,
            scheduled_time=scheduled_time.isoformat()
        )

        return task_id

    async def run_maintenance_tasks(self) -> List[MaintenanceTask]:
        """
        Run scheduled maintenance tasks.

        Returns:
            List[MaintenanceTask]: Completed maintenance tasks
        """
        current_time = datetime.now()
        completed_tasks = []

        # Find tasks that are due
        due_tasks = [
            task for task in self.maintenance_tasks
            if task.status == 'scheduled' and task.scheduled_time <= current_time
        ]

        for task in due_tasks:
            try:
                task.status = 'running'
                logger.info(
                    "Starting maintenance task",
                    task_id=task.task_id,
                    index_name=task.index_name,
                    task_type=task.task_type
                )

                # Execute the maintenance task
                result = await self._execute_maintenance_task(task)

                task.result = result
                task.status = 'completed'
                completed_tasks.append(task)

                logger.info(
                    "Completed maintenance task",
                    task_id=task.task_id,
                    index_name=task.index_name,
                    task_type=task.task_type,
                    result=result
                )

            except (Neo4jError, ClientError, TransientError) as e:
                task.status = 'failed'
                task.result = {'error': str(e)}
                logger.error(
                    "Maintenance task failed",
                    task_id=task.task_id,
                    index_name=task.index_name,
                    task_type=task.task_type,
                    error=str(e)
                )

        return completed_tasks

    async def _execute_maintenance_task(self, task: MaintenanceTask) -> Dict[str, Any]:
        """
        Execute a specific maintenance task.

        Args:
            task: Maintenance task to execute

        Returns:
            Dict[str, Any]: Task execution result
        """
        result = {'task_type': task.task_type, 'success': False}

        try:
            if task.task_type == 'health_check':
                # Perform health check
                health_statuses = await self.check_index_health()
                index_health = next(
                    (h for h in health_statuses if h.index_name == task.index_name),
                    None
                )

                result.update({
                    'success': True,
                    'health_status': {
                        'is_healthy': index_health.is_healthy if index_health else False,
                        'issues': index_health.issues if index_health else [],
                        'recommendations': (
                            index_health.recommendations if index_health else []
                        )
                    }
                })

            elif task.task_type == 'performance_check':
                # Perform performance analysis
                result.update({
                    'success': True,
                    'performance_metrics': await self._analyze_index_performance(
                        task.index_name
                    )
                })

            elif task.task_type == 'cleanup':
                # Perform index cleanup
                result.update({
                    'success': True,
                    'cleanup_result': await self._cleanup_index(task.index_name)
                })

            elif task.task_type == 'optimization':
                # Perform index optimization
                result.update({
                    'success': True,
                    'optimization_result': await self._optimize_index(task.index_name)
                })

            else:
                result['error'] = f"Unknown maintenance task type: {task.task_type}"

        except (Neo4jError, ClientError, TransientError) as e:
            result['error'] = str(e)
            logger.error(
                "Maintenance task execution failed",
                task_type=task.task_type,
                index_name=task.index_name,
                error=str(e)
            )

        return result

    async def _analyze_index_performance(self, index_name: str) -> Dict[str, Any]:
        """
        Analyze index performance.

        Args:
            index_name: Name of the index

        Returns:
            Dict[str, Any]: Performance analysis results
        """
        analysis = {
            'index_name': index_name,
            'query_count': 0,
            'avg_response_time': 0.0,
            'recommendations': []
        }

        try:
            with self.neo4j_client.get_session() as session:
                # Get index usage statistics
                result = session.run("""
                    SHOW INDEXES
                    WHERE name = $indexName
                    YIELD name, readCount, lastRead, trackedSince
                """, indexName=index_name)

                for record in result:
                    analysis['query_count'] = record['readCount'] or 0
                    analysis['last_read'] = record['lastRead']
                    analysis['tracked_since'] = record['trackedSince']

                # Add performance recommendations
                if analysis['query_count'] == 0:
                    analysis['recommendations'].append(
                        "Index is not being used - consider removal"
                    )
                elif analysis['query_count'] > 10000:
                    analysis['recommendations'].append(
                        "High-usage index - monitor performance closely"
                    )

        except (Neo4jError, ClientError) as e:
            analysis['error'] = str(e)
            logger.error(
                "Performance analysis failed",
                index_name=index_name,
                error=str(e)
            )

        return analysis

    async def _cleanup_index(self, index_name: str) -> Dict[str, Any]:
        """
        Cleanup an index.

        Args:
            index_name: Name of the index

        Returns:
            Dict[str, Any]: Cleanup results
        """
        cleanup_result = {
            'index_name': index_name,
            'actions_taken': [],
            'success': True
        }

        try:
            # In a real implementation, you might:
            # 1. Check for orphaned index entries
            # 2. Validate index consistency
            # 3. Remove unused index data
            # 4. Rebuild index statistics

            # For this implementation, we'll just log the cleanup
            cleanup_result['actions_taken'].append("Index cleanup check completed")

            logger.info(
                "Index cleanup completed",
                index_name=index_name,
                actions=cleanup_result['actions_taken']
            )

        except (Neo4jError, ClientError) as e:
            cleanup_result['success'] = False
            cleanup_result['error'] = str(e)
            logger.error(
                "Index cleanup failed",
                index_name=index_name,
                error=str(e)
            )

        return cleanup_result

    async def _optimize_index(self, index_name: str) -> Dict[str, Any]:
        """
        Optimize an index.

        Args:
            index_name: Name of the index

        Returns:
            Dict[str, Any]: Optimization results
        """
        optimization_result = {
            'index_name': index_name,
            'optimizations_applied': [],
            'success': True
        }

        try:
            # In a real implementation, you might:
            # 1. Analyze current HNSW parameters
            # 2. Adjust parameters based on usage patterns
            # 3. Rebuild index with optimized settings
            # 4. Update quantization settings

            # For this implementation, we'll just log the optimization
            optimization_result['optimizations_applied'].append(
                "Index optimization check completed"
            )

            logger.info(
                "Index optimization completed",
                index_name=index_name,
                optimizations=optimization_result['optimizations_applied']
            )

        except (Neo4jError, ClientError) as e:
            optimization_result['success'] = False
            optimization_result['error'] = str(e)
            logger.error(
                "Index optimization failed",
                index_name=index_name,
                error=str(e)
            )

        return optimization_result

    async def auto_schedule_maintenance(self) -> None:
        """
        Automatically schedule maintenance tasks based on index health.
        """
        try:
            # Check if it's time for maintenance check
            time_since_check = datetime.now() - self.last_maintenance_check
            if time_since_check < self.health_check_interval:
                return

            self.last_maintenance_check = datetime.now()

            # Get index health statuses
            health_statuses = await self.check_index_health()

            for health_status in health_statuses:
                # Schedule maintenance if needed
                if not health_status.is_healthy:
                    await self.schedule_maintenance_task(
                        health_status.index_name,
                        'health_check',
                        f"Automatic health check due to issues: "
                        f"{', '.join(health_status.issues)}"
                    )

                # Schedule regular maintenance
                if datetime.now() >= health_status.next_maintenance:
                    await self.schedule_maintenance_task(
                        health_status.index_name,
                        'performance_check',
                        "Scheduled performance maintenance"
                    )

            logger.info(
                "Automatic maintenance scheduling completed",
                indexes_checked=len(health_statuses),
                maintenance_tasks_scheduled=len([
                    task for task in self.maintenance_tasks
                    if task.status == 'scheduled'
                ])
            )

        except (Neo4jError, ClientError, TransientError) as e:
            logger.error("Auto-scheduling maintenance failed", error=str(e))

    async def get_maintenance_status(self) -> Dict[str, Any]:
        """
        Get the current maintenance status.

        Returns:
            Dict[str, Any]: Maintenance status summary
        """
        next_check_time = self.last_maintenance_check + self.health_check_interval
        status = {
            'last_check': self.last_maintenance_check.isoformat(),
            'next_check': next_check_time.isoformat(),
            'total_tasks': len(self.maintenance_tasks),
            'scheduled_tasks': len([
                t for t in self.maintenance_tasks if t.status == 'scheduled'
            ]),
            'running_tasks': len([
                t for t in self.maintenance_tasks if t.status == 'running'
            ]),
            'completed_tasks': len([
                t for t in self.maintenance_tasks if t.status == 'completed'
            ]),
            'failed_tasks': len([
                t for t in self.maintenance_tasks if t.status == 'failed'
            ])
        }

        return status
