"""
Celery worker entry point for Document Processing Service.

This module configures and starts Celery workers for processing document tasks.
It uses the enhanced CeleryFactory to create a worker application with automatic
task module discovery and proper error handling.

Enhanced with comprehensive logging and monitoring for:
- Worker startup and shutdown events
- Task execution lifecycle (start, success, failure, retry)
- Structured logging with consistent format
- Error handling and monitoring
"""

import structlog
from celery.signals import task_prerun, task_postrun, task_failure, task_retry, worker_ready
from configuration.logging_config import configure_logging
from configuration.celery_config import get_celery_settings
from utils.celery_factory import CeleryFactory

# Module-level variables
logger = None
celery_app = None

# Celery Signal Handlers for Enhanced Logging and Monitoring

@task_prerun.connect
def _log_task_prerun(sender=None, task_id=None, task=None, args=None,
                     kwargs=None, **kwds):
    """
    Log task execution start with structured data.
    
    Args:
        sender: Task class
        task_id: Unique task identifier
        task: Task instance (unused)
        args: Task positional arguments
        kwargs: Task keyword arguments
        **kwds: Additional keyword arguments (unused)
    """
    logger.info(
        "Task started",
        task_name=sender.name if sender else 'unknown',
        task_id=task_id,
        args=args,
        kwargs=kwargs
    )

@task_postrun.connect
def _log_task_postrun(sender=None, task_id=None, task=None, args=None,
                      kwargs=None, retval=None, state=None, **kwds):
    """
    Log task execution completion with result data.
    
    Args:
        sender: Task class
        task_id: Unique task identifier
        task: Task instance (unused)
        args: Task positional arguments (unused)
        kwargs: Task keyword arguments (unused)
        retval: Task return value
        state: Task final state
        **kwds: Additional keyword arguments (unused)
    """
    logger.info(
        "Task completed",
        task_name=sender.name if sender else 'unknown',
        task_id=task_id,
        state=state,
        retval=retval
    )

@task_failure.connect
def _log_task_failure(sender=None, task_id=None, exception=None,
                      traceback=None, einfo=None, **kwds):
    """
    Log task execution failure with error details.
    
    Args:
        sender: Task class
        task_id: Unique task identifier
        exception: Exception that caused the failure
        traceback: Exception traceback
        einfo: Exception info object (unused)
        **kwds: Additional keyword arguments (unused)
    """
    logger.error(
        "Task failed",
        task_name=sender.name if sender else 'unknown',
        task_id=task_id,
        exception=str(exception),
        traceback=traceback
    )

@task_retry.connect
def _log_task_retry(sender=None, task_id=None, reason=None, einfo=None,
                    **kwds):
    """
    Log task retry attempts with retry details.
    
    Args:
        sender: Task class
        task_id: Unique task identifier
        reason: Reason for retry
        einfo: Exception info object
        **kwds: Additional keyword arguments (unused)
    """
    logger.warning(
        "Task retry",
        task_name=sender.name if sender else 'unknown',
        task_id=task_id,
        reason=str(reason),
        traceback=einfo.traceback if einfo else None
    )

@worker_ready.connect
def _log_worker_ready(sender=None, **kwds):
    """
    Log worker ready state with worker details.
    
    Args:
        sender: Worker instance
        **kwds: Additional keyword arguments (unused)
    """
    logger.info(
        "Celery worker ready",
        worker_hostname=sender.hostname if sender else 'unknown'
    )

def _initialize_application():
    """
    Initialize the Celery application with logging and task discovery.
    
    This function must be called before using the celery_app.
    """
    global logger, celery_app

    try:
        # Configure logging at application startup
        configure_logging()
        logger = structlog.get_logger(__name__)

        # Import task modules to register them with the app
        __import__('tasks.document_tasks')

        # Create the Celery app instance
        settings = get_celery_settings()

        celery_app = CeleryFactory.create_celery_app(
            name="document_processing_service",
            settings=settings
        )

        logger.info(
            "Document Processing Service Celery worker initialized successfully"
        )

    except Exception as e:
        error_msg = str(e)
        if logger:
            logger.error(
                "Failed to initialize Celery worker application",
                error=error_msg
            )
        raise

def _discover_task_modules():
    """
    Discover task modules in the tasks directory.
    
    Returns:
        List[str]: List of task module names to import
    """
    # For now, we know we have document_tasks module
    # This could be enhanced to dynamically discover modules in the future
    return ['tasks.document_tasks']

def start_worker():
    """
    Start the Celery worker with enhanced monitoring and error handling.
    
    Raises:
        Exception: If worker startup fails
    """
    global logger, celery_app
    
    # Ensure application is initialized
    if celery_app is None:
        _initialize_application()
    
    try:
        logger.info("Starting Celery worker with enhanced monitoring")
        worker = celery_app.Worker()
        worker.start()
        logger.info("Celery worker started successfully")
    except Exception as e:
        error_msg = str(e)
        logger.error("Failed to start Celery worker", error=error_msg)
        raise

# Initialize the application when the module is imported
_initialize_application()

if __name__ == "__main__":
    start_worker()
