"""
Celery worker application entrypoint for Neo4j Ingestion Service.

Creates the Celery app instance, configures routing/acks, and imports tasks
to ensure registration. Provides a small helper to validate task discovery.
"""

import structlog
logger = structlog.get_logger(__name__)

from utils.celery_factory import get_celery_app
from constants import APP_NAME_NEO4J_INGESTION, EXPECTED_TASKS_INGESTION
from module_utils.asyncio_runner import PersistentEventLoopRunner
from celery import signals

celery_app = get_celery_app(APP_NAME_NEO4J_INGESTION)

@signals.worker_process_init.connect
def _on_worker_process_init(**kwargs):  # noqa: D401
    """Start persistent asyncio event loop when a worker process starts."""
    try:
        runner = PersistentEventLoopRunner.get_instance()
        runner.start()
        logger.info("Persistent asyncio loop started for worker process")
    except Exception as exc:
        logger.error("Failed to start persistent asyncio loop", extra={"error": str(exc)})

@signals.worker_process_shutdown.connect
def _on_worker_process_shutdown(**kwargs):  # noqa: D401
    """Stop persistent asyncio event loop when the worker process shuts down."""
    try:
        runner = PersistentEventLoopRunner.get_instance()
        if runner.is_running():
            runner.stop()
        logger.info("Persistent asyncio loop stopped for worker process")
    except Exception as exc:
        logger.error("Failed to stop persistent asyncio loop", extra={"error": str(exc)})

# Ensure tasks are registered by importing task modules
try:
    from tasks import graphrag as _graphrag_tasks  # noqa: F401
    from tasks import retry as _retry_tasks  # noqa: F401
    logger.info("Neo4j ingestion tasks modules imported for registration")
except Exception as e:
    logger.error("Failed to import neo4j ingestion tasks modules", extra={"error": str(e)})


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
        expected_tasks = EXPECTED_TASKS_INGESTION

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
            "expected_tasks": EXPECTED_TASKS_INGESTION,
            "missing_tasks": EXPECTED_TASKS_INGESTION,
            "all_tasks_registered": False,
            "error": str(e),
        }


__all__ = ["celery_app", "get_task_validation_status"]