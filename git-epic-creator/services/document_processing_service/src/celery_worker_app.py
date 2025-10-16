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
from datetime import datetime, timezone
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

# Initialization status tracking for health checks (separate for each processor)
_initialization_status = {
    "initialized": False,
    "healthy": False,
    "docling": {
        "initialized": False,
        "healthy": False,
        "error": None,
        "error_type": None,
        "vlm_mode": None,
        "ocr_enabled": None
    },
    "tika": {
        "initialized": False,
        "healthy": False,
        "error": None,
        "error_type": None
    },
    "timestamp": None
}

def get_initialization_status():
    """Get the current initialization status for health checks."""
    return _initialization_status.copy()

def get_processors_health():
    """
    Get runtime health status for both Docling and Tika processors.
    
    This function calls check_health() on the singleton processor instances
    and returns their health status. Only checks processors that initialized successfully.
    
    Returns:
        dict: Health status for both processors with keys 'docling' and 'tika'
    """
    health_status = {
        "docling": None,
        "tika": None
    }
    
    # Check Docling processor health
    if _initialization_status["docling"]["healthy"] and _docling_processor is not None:
        try:
            health_status["docling"] = _docling_processor.check_health()
        except Exception as e:
            logger.error("Docling runtime health check failed", error=str(e), exc_info=True)
            health_status["docling"] = {
                "healthy": False,
                "error": f"Docling health check failed: {str(e)}"
            }
    else:
        # Processor not initialized or unhealthy
        health_status["docling"] = {
            "healthy": False,
            "error": "Docling unavailable due to initialization failure",
            "initialization_error": _initialization_status["docling"].get("error")
        }
    
    # Check Tika processor health
    if _initialization_status["tika"]["healthy"] and _tika_processor is not None:
        try:
            health_status["tika"] = _tika_processor.check_health()
        except Exception as e:
            logger.error("Tika runtime health check failed", error=str(e), exc_info=True)
            health_status["tika"] = {
                "healthy": False,
                "error": f"Tika health check failed: {str(e)}"
            }
    else:
        # Processor not initialized or unhealthy
        health_status["tika"] = {
            "healthy": False,
            "error": "Tika unavailable due to initialization failure",
            "initialization_error": _initialization_status["tika"].get("error")
        }
    
    return health_status

def _initialize_processors_eagerly():
    """
    Eagerly initialize both singleton processors at worker startup.
    This ensures configuration errors are caught immediately rather than
    during first task execution.
    
    NOTE: This function does NOT raise exceptions. Instead, it sets the
    initialization status so health checks can report the failure.
    This allows the service to stay running and report unhealthy status.
    """
    global _docling_processor, _tika_processor, _initialization_status
    
    logger.info("PROCESSOR_INITIALIZATION_STARTED",
               message="Starting eager processor initialization")
    
    overall_healthy = True
    timestamp = datetime.now(timezone.utc).isoformat()
    
    # Initialize Docling processor
    try:
        _docling_processor = DoclingProcessor()
        _initialization_status["docling"] = {
            "initialized": True,
            "healthy": True,
            "error": None,
            "error_type": None,
            "vlm_mode": _docling_processor.settings.DOCLING_VLM_MODE,
            "ocr_enabled": _docling_processor.settings.DOCLING_USE_OCR
        }
        logger.info("docling_processor_initialized_successfully",
                   vlm_mode=_docling_processor.settings.DOCLING_VLM_MODE,
                   vlm_provider=_docling_processor.settings.DOCLING_VLM_PROVIDER)
        
    except ValueError as ve:
        # Configuration validation errors (e.g., missing Azure OpenAI credentials)
        error_msg = f"Docling configuration validation failed: {ve}"
        logger.error("FATAL_docling_initialization_failed",
                    error=error_msg,
                    error_type="configuration_error",
                    exc_info=True)
        
        _initialization_status["docling"] = {
            "initialized": True,
            "healthy": False,
            "error": error_msg,
            "error_type": "configuration_error",
            "vlm_mode": None,
            "ocr_enabled": None
        }
        overall_healthy = False
        
    except Exception as e:
        # Unexpected initialization errors
        error_msg = f"Docling initialization failed: {e}"
        logger.error("FATAL_docling_initialization_failed",
                    error=error_msg,
                    error_type="unexpected_error",
                    exc_info=True)
        
        _initialization_status["docling"] = {
            "initialized": True,
            "healthy": False,
            "error": error_msg,
            "error_type": "unexpected_error",
            "vlm_mode": None,
            "ocr_enabled": None
        }
        overall_healthy = False
    
    # Initialize Tika processor
    try:
        _tika_processor = TikaProcessor()
        _initialization_status["tika"] = {
            "initialized": True,
            "healthy": True,
            "error": None,
            "error_type": None
        }
        logger.info("tika_processor_initialized_successfully")
        
    except Exception as e:
        # Tika initialization errors
        error_msg = f"Tika initialization failed: {e}"
        logger.error("FATAL_tika_initialization_failed",
                    error=error_msg,
                    error_type="unexpected_error",
                    exc_info=True)
        
        _initialization_status["tika"] = {
            "initialized": True,
            "healthy": False,
            "error": error_msg,
            "error_type": "unexpected_error"
        }
        overall_healthy = False
    
    # Set overall status
    _initialization_status["initialized"] = True
    _initialization_status["healthy"] = overall_healthy
    _initialization_status["timestamp"] = timestamp
    
    if overall_healthy:
        logger.info("PROCESSOR_INITIALIZATION_SUCCESSFUL",
                   message="All processors initialized successfully")
    else:
        logger.critical("SERVICE_UNHEALTHY",
                       message="Service will report unhealthy status due to initialization failure",
                       docling_healthy=_initialization_status["docling"]["healthy"],
                       tika_healthy=_initialization_status["tika"]["healthy"])

def get_docling_processor():
    """
    Get the singleton DoclingProcessor instance.
    
    Returns:
        DoclingProcessor: Singleton processor instance with pre-loaded plugins
        
    Raises:
        RuntimeError: If processor was not initialized successfully
    """
    if not _initialization_status["healthy"]:
        error_msg = _initialization_status.get("error", "Processor initialization failed")
        raise RuntimeError(
            f"DoclingProcessor not available: {error_msg}"
        )
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
        RuntimeError: If processor was not initialized successfully
    """
    if not _initialization_status["healthy"]:
        error_msg = _initialization_status.get("error", "Processor initialization failed")
        raise RuntimeError(
            f"TikaProcessor not available: {error_msg}"
        )
    if _tika_processor is None:
        raise RuntimeError(
            "TikaProcessor not initialized. This indicates worker startup failed."
        )
    return _tika_processor

# Eagerly initialize processors at module load time (worker startup)
# This captures initialization status for health check reporting
_initialize_processors_eagerly()

# Explicitly import task modules to ensure Celery registers them at worker startup
# Only import tasks if initialization was successful
if _initialization_status["healthy"]:
    try:
        from tasks import document_tasks as _document_tasks  # noqa: F401
        logger.info("Document tasks module imported for registration")
    except Exception as e:
        logger.error("Failed to import document tasks module", error=str(e))
        _initialization_status["healthy"] = False
        _initialization_status["error"] = f"Task import failed: {e}"
        _initialization_status["error_type"] = "task_import_error"
else:
    logger.critical("TASKS_NOT_IMPORTED",
                   message="Skipping task import due to processor initialization failure",
                   error=_initialization_status.get("error"))

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

# Export the configured app, validation function, singleton processor getters, initialization status, and health check
__all__ = ['celery_app', 'get_task_validation_status', 'get_docling_processor', 'get_tika_processor', 'get_initialization_status', 'get_processors_health']