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
from configuration.logging_config import configure_logging
from configuration.celery_config import get_celery_settings
from utils.celery_factory import get_celery_app

# Celery Signal Handlers for Enhanced Logging and Monitoring

# Initialize Celery app
configure_logging()
logger = structlog.get_logger(__name__)

celery_app = get_celery_app(name="document_processing_service")

def start_worker():
    """
    Start the Celery worker with enhanced monitoring and error handling.
    
    Raises:
        Exception: If worker startup fails
    """
    try:
        logger.info("Starting Celery worker with enhanced monitoring")
        worker = celery_app.Worker()
        worker.start()
        logger.info("Celery worker started successfully")
    except Exception as e:
        error_msg = str(e)
        logger.error("Failed to start Celery worker", error=error_msg)
        raise

if __name__ == "__main__":
    start_worker()
