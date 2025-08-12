"""
Celery worker application entrypoint for Neo4j Ingestion Service.

Creates the Celery app instance, configures routing/acks, and imports tasks
to ensure registration. Provides a small helper to validate task discovery.
"""

import structlog
logger = structlog.get_logger(__name__)

from utils.celery_factory import get_celery_app

celery_app = get_celery_app("neo4j_ingestion_service")

celery_app.conf.update(
    task_acks_late=True,
    worker_prefetch_multiplier=1,
    task_routes={
        **(celery_app.conf.task_routes or {}),
        "tasks.neo4j_ingestion.*": {"queue": "neo4j_ingestion"},
    },
)


def get_task_validation_status():
    """
    Report discovered vs expected tasks for this service.
    """
    try:
        discovered_tasks = [
            task_name
            for task_name in celery_app.tasks.keys()
            if not task_name.startswith("celery.")
        ]
        expected_tasks = [
            "tasks.neo4j_ingestion.run_graphrag_job",
        ]

        missing_tasks = [t for t in expected_tasks if t not in discovered_tasks]

        logger.info(
            "Task validation status",
            extra={
                "discovered_tasks_count": len(discovered_tasks),
                "discovered_tasks": discovered_tasks,
                "expected_tasks": expected_tasks,
                "missing_tasks": missing_tasks,
                "all_tasks_registered": len(missing_tasks) == 0,
            },
        )

        return {
            "discovered_tasks": discovered_tasks,
            "expected_tasks": expected_tasks,
            "missing_tasks": missing_tasks,
            "all_tasks_registered": len(missing_tasks) == 0,
        }
    except Exception as e:
        logger.error("Failed to get task validation status", extra={"error": str(e)})
        return {
            "discovered_tasks": [],
            "expected_tasks": [
                "tasks.neo4j_ingestion.run_graphrag_job",
            ],
            "missing_tasks": [
                "tasks.neo4j_ingestion.run_graphrag_job",
            ],
            "all_tasks_registered": False,
            "error": str(e),
        }


__all__ = ["celery_app", "get_task_validation_status"]

# Import tasks to ensure registration after celery_app is defined
try:
    from tasks import graphrag as _tasks_graphrag  # noqa: F401
except Exception as _e:
    logger.warning("Tasks import failed during worker init", extra={"error": str(_e)})
