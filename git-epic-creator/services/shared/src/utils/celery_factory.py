"""
Celery factory for creating Celery applications.
"""

from functools import lru_cache
from typing import Dict, Any, List, Optional
import importlib
import structlog
from celery import Celery
from configuration.celery_config import CelerySettings, get_celery_settings

logger = structlog.get_logger(__name__)

class CeleryFactory:
    """
    Factory class for creating Celery applications.
    Follows the Factory pattern and Single Responsibility Principle.
    """

    @staticmethod
    def create_celery_app(name: str, settings: CelerySettings = None) -> Celery:
        """
        Create a Celery application with standard configuration.
        
        Args:
            name: Application name
            settings: Celery configuration settings (optional)
            
        Returns:
            Celery: Configured Celery application
        """
        settings = settings or get_celery_settings()

        app = Celery(name)

        # Configure Celery
        app.conf.update(
            broker_url=settings.CELERY_BROKER_URL,
            result_backend=settings.CELERY_RESULT_BACKEND,
            task_serializer=settings.CELERY_TASK_SERIALIZER,
            result_serializer=settings.CELERY_RESULT_SERIALIZER,
            accept_content=settings.CELERY_ACCEPT_CONTENT,
            timezone=settings.CELERY_TIMEZONE,
            enable_utc=settings.CELERY_ENABLE_UTC,
            task_track_started=settings.CELERY_TASK_TRACK_STARTED,
            task_time_limit=settings.CELERY_TASK_TIME_LIMIT,
            task_soft_time_limit=settings.CELERY_TASK_SOFT_TIME_LIMIT,
            worker_prefetch_multiplier=settings.CELERY_WORKER_PREFETCH_MULTIPLIER,
            worker_max_tasks_per_child=settings.CELERY_WORKER_MAX_TASKS_PER_CHILD,
            worker_concurrency=settings.CELERY_WORKER_CONCURRENCY,
            task_routes=settings.CELERY_TASK_ROUTES
        )

        logger.info(
            "Celery application created",
            name=name,
            broker_url=settings.CELERY_BROKER_URL,
            result_backend=settings.CELERY_RESULT_BACKEND
        )

        return app

    @staticmethod
    def create_celery_worker_app(
        name: str, 
        settings: Optional[CelerySettings] = None,
        task_modules: Optional[List[str]] = None
    ) -> Celery:
        """
        Create a Celery worker application with task module discovery and logging integration.
        
        Args:
            name: Application name
            settings: Celery configuration settings (optional)
            task_modules: List of task module names to import and register (optional)
            
        Returns:
            Celery: Configured Celery worker application with registered tasks
        """
        settings = settings or get_celery_settings()
        task_modules = task_modules or []

        # Create base Celery app using existing method
        app = CeleryFactory.create_celery_app(name, settings)

        # Import and register task modules
        for module_name in task_modules:
            try:
                logger.info(
                    "Importing task module",
                    module_name=module_name,
                    app_name=name
                )
                importlib.import_module(module_name)
                logger.info(
                    "Successfully imported task module",
                    module_name=module_name,
                    app_name=name
                )
            except ImportError as e:
                logger.error(
                    "Failed to import task module",
                    module_name=module_name,
                    app_name=name,
                    error=str(e)
                )
                # Continue with other modules even if one fails

        logger.info(
            "Celery worker application created with task modules",
            name=name,
            task_modules_count=len(task_modules),
            broker_url=settings.CELERY_BROKER_URL,
            result_backend=settings.CELERY_RESULT_BACKEND
        )

        return app

@lru_cache()
def get_celery_app(name: str) -> Celery:
    """Get a Celery application, creating it if necessary."""
    settings = get_celery_settings()
    return CeleryFactory.create_celery_app(name, settings)

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
        result: Dict[str, Any] = {
            "healthy": False,
            "error": None,
            "broker_url": app.broker_url,
            "backend_url": app.backend_url,
            "active_workers_count": 0,
            "worker_details": {},
            "active_tasks": {},
            "scheduled_tasks": {},
            "registered_tasks": {},
            "stats": {}
        }
        try:
            # Ping workers to get basic connectivity and worker names
            ping_responses = app.control.ping(timeout=1, reply=True)
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
            else:
                logger.info("No registered tasks found.")

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


            logger.info("Celery detailed health check passed", **{k: v for k, v in result.items() if k != 'error'})
            return result

        except Exception as e:
            result["healthy"] = False
            result["error"] = str(e)
            logger.error(f"Celery detailed health check failed: {e}", exc_info=True)
            return result
