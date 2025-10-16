"""Celery factory for creating Celery applications."""

from functools import lru_cache
from typing import Dict, Any
import os
import time
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
    Health checker for Celery application connections with busy-worker awareness.
    
    Distinguishes between:
    - Workers that are idle
    - Workers that are busy processing long-running tasks
    - Workers that are unresponsive or crashed
    
    Follows Single Responsibility Principle.
    """
    
    # Configuration constants
    PING_TIMEOUT = 5  # Seconds to wait for worker ping response
    INSPECTOR_TIMEOUT = 5  # Seconds to wait for inspector calls (increased from 1s)
    LONG_RUNNING_THRESHOLD = 60  # Seconds after which a task is considered long-running

    @staticmethod
    def check_health_with_details(app: Celery) -> Dict[str, Any]:
        """
        Check Celery application health with detailed information.
        
        Handles long-running tasks gracefully:
        - If workers respond to ping but inspector times out, marks as "busy" (still healthy)
        - Tracks long-running tasks separately from errors
        - Provides clear distinction between missing and busy workers

        Args:
            app: Celery application instance.

        Returns:
            dict: Health check results with worker state and details.
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
            "worker_state": "unknown",  # NEW: idle, active, busy, busy_unresponsive, unresponsive
            "long_running_tasks": [],    # NEW: tasks running > threshold
            "worker_details": {},
            "active_tasks": {},
            "scheduled_tasks": {},
            "registered_tasks": {},
            "stats": {},
            "inspect_timeout": None      # NEW: capture inspector timeout details
        }
        try:
            # Step 1: Ping workers to verify they're alive
            # Uses longer timeout to allow workers time to respond even when busy
            ping_responses = app.control.broadcast('ping', reply=True, timeout=CeleryHealthChecker.PING_TIMEOUT)
            if not ping_responses:
                result["error"] = "No active workers responded to ping."
                result["worker_state"] = "unresponsive"
                logger.warning("CELERY_HEALTH_NO_WORKERS", error=result['error'])
                return result

            result["active_workers_count"] = len(ping_responses)
            result["healthy"] = True  # At least workers are alive

            # Step 2: Inspect workers with increased timeout for busy workers
            inspector = app.control.inspect(timeout=CeleryHealthChecker.INSPECTOR_TIMEOUT)

            # Step 3: Get active tasks with timeout handling
            try:
                active = inspector.active()
                if active:
                    result["active_tasks"] = active
                    
                    # Analyze task durations to identify long-running tasks
                    long_running = []
                    current_time = time.time()
                    
                    for worker_name, tasks in active.items():
                        for task in tasks:
                            time_start = task.get('time_start')
                            if time_start:
                                elapsed = current_time - time_start
                                if elapsed > CeleryHealthChecker.LONG_RUNNING_THRESHOLD:
                                    long_running.append({
                                        "worker": worker_name,
                                        "task_id": task.get('id'),
                                        "task_name": task.get('name'),
                                        "elapsed_seconds": int(elapsed)
                                    })
                    
                    result["long_running_tasks"] = long_running
                    
                    if long_running:
                        result["worker_state"] = "busy"
                        logger.info("CELERY_WORKERS_BUSY", 
                                   active_task_count=sum(len(t) for t in active.values()),
                                   long_running_count=len(long_running),
                                   long_running_tasks=[t["task_name"] for t in long_running])
                    else:
                        result["worker_state"] = "active"
                        logger.debug("CELERY_WORKERS_ACTIVE", 
                                    active_task_count=sum(len(t) for t in active.values()))
                else:
                    result["worker_state"] = "idle"
                    logger.debug("CELERY_WORKERS_IDLE")
                    
            except Exception as inspect_error:
                # Inspector timed out, but ping succeeded - workers are alive but very busy
                result["worker_state"] = "busy_unresponsive"
                result["inspect_timeout"] = str(inspect_error)
                logger.warning("CELERY_INSPECT_TIMEOUT_DURING_PROCESSING",
                              error=str(inspect_error),
                              note="Workers alive but too busy to respond to inspect - likely processing large documents",
                              still_healthy=True)
                # Still healthy - workers responded to ping, just too busy for inspector

            # Step 4: Get scheduled tasks (non-critical, allow failure)
            try:
                scheduled = inspector.scheduled()
                if scheduled:
                    result["scheduled_tasks"] = scheduled
            except Exception:
                pass  # Not critical for health check

            # Step 5: Get registered tasks with timeout handling
            try:
                registered = inspector.registered()
                if registered:
                    result["registered_tasks"] = registered
                    
                    # Validate that expected tasks are registered
                    expected_tasks = ['tasks.document_tasks.process_project_documents_task']
                    missing_tasks = []
                    
                    for expected_task in expected_tasks:
                        task_found = any(
                            expected_task in worker_tasks 
                            for worker_tasks in registered.values()
                        )
                        if not task_found:
                            missing_tasks.append(expected_task)
                    
                    if missing_tasks:
                        logger.warning("CELERY_TASKS_NOT_REGISTERED", 
                                      missing_tasks=missing_tasks)
                        result["missing_tasks"] = missing_tasks
                        result["healthy"] = False  # Tasks not registered is a real problem
                    else:
                        logger.debug("CELERY_ALL_TASKS_REGISTERED")
                        
            except Exception as reg_error:
                # Inspector timeout for registered tasks - could be busy, don't fail health
                logger.warning("CELERY_INSPECT_REGISTERED_TIMEOUT",
                              error=str(reg_error),
                              note="Could not verify registered tasks - worker may be busy")

            # Step 6: Get worker stats (non-critical)
            try:
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
                            "state": result["worker_state"]
                        }
                    result["worker_details"] = worker_details
            except Exception:
                pass  # Not critical

            logger.debug("CELERY_HEALTH_CHECK_COMPLETED", 
                        healthy=result["healthy"],
                        worker_state=result["worker_state"],
                        active_workers=result["active_workers_count"])
            return result

        except Exception as e:
            result["healthy"] = False
            result["error"] = str(e)
            result["worker_state"] = "error"
            logger.error("CELERY_HEALTH_CHECK_FAILED", error=str(e), exc_info=True)
            return result
