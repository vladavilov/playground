"""Celery factory for creating Celery applications."""

from functools import lru_cache
from typing import Dict, Any
import os
import structlog
from celery import Celery
from celery.signals import worker_ready
from configuration.celery_config import get_celery_settings

logger = structlog.get_logger(__name__)

@worker_ready.connect
def _log_worker_ready(sender=None, **kwds):
    """Log worker ready state."""
    logger.info("Celery worker ready",
                worker_hostname=getattr(sender, 'hostname', 'unknown'))

@lru_cache
def get_celery_app(name: str) -> Celery:
    """Get a Celery application, creating it if necessary."""
    settings = get_celery_settings()

    app = Celery(name)

    conf: Dict[str, Any] = {
        "broker_url": settings.CELERY_BROKER_URL,
        "result_backend": settings.CELERY_RESULT_BACKEND,
        "task_serializer": settings.CELERY_TASK_SERIALIZER,
        "result_serializer": settings.CELERY_RESULT_SERIALIZER,
        "accept_content": settings.CELERY_ACCEPT_CONTENT,
        "timezone": settings.CELERY_TIMEZONE,
        "enable_utc": settings.CELERY_ENABLE_UTC,
        "task_track_started": settings.CELERY_TASK_TRACK_STARTED,
        "task_time_limit": settings.CELERY_TASK_TIME_LIMIT,
        "task_acks_late": True,
        "worker_prefetch_multiplier": settings.CELERY_WORKER_PREFETCH_MULTIPLIER,
        "worker_max_tasks_per_child": settings.CELERY_WORKER_MAX_TASKS_PER_CHILD,
        "worker_concurrency": settings.CELERY_WORKER_CONCURRENCY,
        "task_routes": settings.CELERY_TASK_ROUTES,
        # Task protocol version 2 enables access to task headers via self.request.headers
        "task_protocol": 2,
        # Logging controls to preserve structlog config and correct stdout level
        "worker_hijack_root_logger": False,
        "worker_redirect_stdouts": True,
        "worker_redirect_stdouts_level": "INFO",
        # Reliability flags
        "task_reject_on_worker_lost": True,
        "task_acks_on_failure_or_timeout": True,
        "broker_transport_options": {
            "visibility_timeout": getattr(settings, "CELERY_BROKER_VISIBILITY_TIMEOUT", 420),
        },
    }

    if os.name != "nt":
        conf["task_soft_time_limit"] = settings.CELERY_TASK_SOFT_TIME_LIMIT
    else:
        logger.info("Disabling Celery soft timeouts on Windows (SIGUSR1 unsupported)")

    app.conf.update(**conf)

    logger.info(
        "Celery application created",
        name=name,
        broker_url=settings.CELERY_BROKER_URL,
        result_backend=settings.CELERY_RESULT_BACKEND
    )

    return app

class CeleryHealthChecker:
    """
    Health checker for Celery application connections.
    Follows Single Responsibility Principle.
    """

    @staticmethod
    def check_health_with_details(app: Celery) -> Dict[str, Any]:
        """
        Check Celery application health with detailed information.

        Args:
            app: Celery application instance.

        Returns:
            dict: Health check results with details.
        """
        # Safely access configuration with fallback values
        try:
            broker_url = getattr(app.conf, 'broker_url', 'unavailable')
        except AttributeError:
            broker_url = 'unavailable'
        
        try:
            backend_url = getattr(app.conf, 'result_backend', 'unavailable')
        except AttributeError:
            backend_url = 'unavailable'

        result: Dict[str, Any] = {
            "healthy": False,
            "error": None,
            "broker_url": broker_url,
            "backend_url": backend_url,
            "active_workers_count": 0,
            "worker_details": {},
            "active_tasks": {},
            "scheduled_tasks": {},
            "registered_tasks": {},
            "stats": {}
        }
        try:
            # Ping workers to get basic connectivity and worker names
            # Use longer timeout to allow workers time to initialize
            ping_responses = app.control.broadcast('ping', reply=True, timeout=5)
            if not ping_responses:
                result["error"] = "No active workers responded to ping."
                logger.warning(f"Celery detailed health check failed: {result['error']}")
                return result

            result["active_workers_count"] = len(ping_responses)
            result["healthy"] = True

            # Use inspect to gather more detailed information from workers
            inspector = app.control.inspect(timeout=1)

            # Get active tasks
            active = inspector.active()
            if active:
                result["active_tasks"] = active
            else:
                logger.info("No active tasks found.")

            # Get scheduled tasks (tasks waiting to be executed at a specific time)
            scheduled = inspector.scheduled()
            if scheduled:
                result["scheduled_tasks"] = scheduled
            else:
                logger.info("No scheduled tasks found.")

            # Get registered tasks (tasks known by the workers)
            registered = inspector.registered()
            if registered:
                result["registered_tasks"] = registered
                
                # Validate that expected tasks are registered
                expected_tasks = ['tasks.document_tasks.process_project_documents_task']
                missing_tasks = []
                
                for expected_task in expected_tasks:
                    task_found = False
                    for worker_tasks in registered.values():
                        if expected_task in worker_tasks:
                            task_found = True
                            break
                    if not task_found:
                        missing_tasks.append(expected_task)
                
                if missing_tasks:
                    logger.warning("Expected tasks not registered", missing_tasks=missing_tasks)
                    result["missing_tasks"] = missing_tasks
                else:
                    logger.info("All expected tasks are registered")
                    
            else:
                logger.warning("No registered tasks found - this indicates task discovery issues")

            # Get worker stats (e.g., processed tasks, uptime)
            stats = inspector.stats()
            if stats:
                result["stats"] = stats
                # Extract worker details from stats for a cleaner view
                worker_details = {}
                for worker_name, worker_stats in stats.items():
                    worker_details[worker_name] = {
                        "pid": worker_stats.get('pid'),
                        "uptime": worker_stats.get('uptime'),
                        "processed": worker_stats.get('total', 0),
                        "state": "running" # Assuming if stats are available, it's running
                    }
                result["worker_details"] = worker_details
            else:
                logger.info("No worker stats found.")

            logger.debug("Celery detailed health check passed", **{k: v for k, v in result.items() if k != 'error'})
            return result

        except Exception as e:
            result["healthy"] = False
            result["error"] = str(e)
            logger.error(f"Celery detailed health check failed: {e}", exc_info=True)
            return result
