"""
Celery worker application entrypoint.

This module creates the Celery app instance and imports task modules to register tasks.
It follows the architectural pattern of separating the Celery app creation from task definitions
to avoid circular imports.

It also creates singleton processor instances (DoclingProcessor, TikaProcessor) that are
initialized once when the worker starts, ensuring all plugins and models are pre-loaded
and reused across all task executions.
"""

import structlog
from utils.celery_factory import get_celery_app
from constants import APP_NAME_DOCUMENT_PROCESSING, EXPECTED_TASKS_DOCUMENT
from services.docling_processor import DoclingProcessor
from services.tika_processor import TikaProcessor
from utils.asyncio_runner import PersistentEventLoopRunner
from celery import signals

logger = structlog.get_logger(__name__)

celery_app = get_celery_app(APP_NAME_DOCUMENT_PROCESSING)

@signals.worker_process_init.connect
def _on_worker_process_init(**kwargs):
    """Start persistent asyncio event loop when a worker process starts."""
    try:
        runner = PersistentEventLoopRunner.get_instance()
        runner.start()
        logger.info("Persistent asyncio loop started for worker process")
    except Exception as exc:
        logger.error("Failed to start persistent asyncio loop", error=str(exc))

@signals.worker_process_shutdown.connect
def _on_worker_process_shutdown(**kwargs):
    """Stop persistent asyncio event loop when the worker process shuts down."""
    try:
        runner = PersistentEventLoopRunner.get_instance()
        if runner.is_running():
            runner.stop()
        logger.info("Persistent asyncio loop stopped for worker process")
    except Exception as exc:
        logger.error("Failed to stop persistent asyncio loop", error=str(exc))

# Initialize singleton processor instances at worker startup (EAGER initialization)
# These are shared across all task executions to avoid redundant initialization
# and to fail fast if configuration is invalid
_docling_processor = None
_tika_processor = None

def _initialize_processors_eagerly():
    """
    Eagerly initialize both singleton processors at worker startup.
    This ensures configuration errors are caught immediately rather than
    during first task execution, preventing worker crashes.
    
    Raises:
        RuntimeError: If processor initialization fails due to configuration errors
    """
    global _docling_processor, _tika_processor
    
    try:
        _docling_processor = DoclingProcessor()
        logger.info("docling_processor_initialized_successfully",
                   vlm_mode=_docling_processor.settings.DOCLING_VLM_MODE,
                   vlm_provider=_docling_processor.settings.DOCLING_VLM_PROVIDER)
        _tika_processor = TikaProcessor()
        logger.info("tika_processor_initialized_successfully")
        
    except ValueError as ve:
        # Configuration validation errors (e.g., missing Azure OpenAI credentials)
        error_msg = f"Configuration validation failed: {ve}"
        logger.error("FATAL_processor_initialization_failed",
                    error=error_msg,
                    error_type="configuration_error",
                    exc_info=True)
        raise RuntimeError(error_msg) from ve
    except Exception as e:
        # Unexpected initialization errors
        error_msg = f"Processor initialization failed: {e}"
        logger.error("FATAL_processor_initialization_failed",
                    error=error_msg,
                    error_type="unexpected_error",
                    exc_info=True)
        raise RuntimeError(error_msg) from e

def get_docling_processor():
    """
    Get the singleton DoclingProcessor instance.
    
    Returns:
        DoclingProcessor: Singleton processor instance with pre-loaded plugins
        
    Raises:
        RuntimeError: If processor was not initialized at startup
    """
    if _docling_processor is None:
        raise RuntimeError(
            "DoclingProcessor not initialized. This indicates worker startup failed."
        )
    return _docling_processor

def get_tika_processor():
    """
    Get the singleton TikaProcessor instance.
    
    Returns:
        TikaProcessor: Singleton processor instance
        
    Raises:
        RuntimeError: If processor was not initialized at startup
    """
    if _tika_processor is None:
        raise RuntimeError(
            "TikaProcessor not initialized. This indicates worker startup failed."
        )
    return _tika_processor

# Eagerly initialize processors at module load time (worker startup)
# This will fail fast if configuration is invalid, preventing silent worker crashes
_initialize_processors_eagerly()

# Explicitly import task modules to ensure Celery registers them at worker startup
try:
    from tasks import document_tasks as _document_tasks  # noqa: F401
    logger.info("Document tasks module imported for registration")
except Exception as e:
    logger.error("Failed to import document tasks module", error=str(e))

def get_task_validation_status():
    """
    Get the current task validation status.
    
    Returns:
        dict: Task validation information including discovered and expected tasks
    """
    try:
        discovered_tasks = [task for task in celery_app.tasks.keys() if not task.startswith('celery.')]
        expected_tasks = EXPECTED_TASKS_DOCUMENT
        
        missing_tasks = []
        for expected_task in expected_tasks:
            if expected_task not in discovered_tasks:
                missing_tasks.append(expected_task)
        
        logger.info(
            "Task validation status",
            discovered_tasks_count=len(discovered_tasks),
            discovered_tasks=discovered_tasks,
            expected_tasks=expected_tasks,
            missing_tasks=missing_tasks,
            all_tasks_registered=len(missing_tasks) == 0
        )
        
        return {
            'discovered_tasks': discovered_tasks,
            'expected_tasks': expected_tasks,
            'missing_tasks': missing_tasks,
            'all_tasks_registered': len(missing_tasks) == 0
        }
    except Exception as e:
        logger.error("Failed to get task validation status", error=str(e))
        return {
            'discovered_tasks': [],
            'expected_tasks': EXPECTED_TASKS_DOCUMENT,
            'missing_tasks': EXPECTED_TASKS_DOCUMENT,
            'all_tasks_registered': False,
            'error': str(e)
        }

# Export the configured app, validation function, and singleton processor getters
__all__ = ['celery_app', 'get_task_validation_status', 'get_docling_processor', 'get_tika_processor']