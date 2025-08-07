"""
Celery worker application entrypoint.

This module creates the Celery app instance and imports task modules to register tasks.
It follows the architectural pattern of separating the Celery app creation from task definitions
to avoid circular imports.
"""

import structlog
from utils.celery_factory import get_celery_app

logger = structlog.get_logger(__name__)

celery_app = get_celery_app("document_processing_service")

try:
    from tasks import document_tasks
    logger.info(
        "Task modules imported successfully",
        app_name="document_processing_service"
    )
except Exception as e:
    logger.error(
        "Failed to import task modules",
        app_name="document_processing_service",
        error=str(e)
    )
    raise

def get_task_validation_status():
    """
    Get the current task validation status.
    
    Returns:
        dict: Task validation information including discovered and expected tasks
    """
    try:
        discovered_tasks = [task for task in celery_app.tasks.keys() if not task.startswith('celery.')]
        expected_tasks = ['tasks.document_tasks.process_project_documents_task']
        
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
            'expected_tasks': ['tasks.document_tasks.process_project_documents_task'],
            'missing_tasks': ['tasks.document_tasks.process_project_documents_task'],
            'all_tasks_registered': False,
            'error': str(e)
        }

# Export the configured app and validation function for use by other modules
__all__ = ['celery_app', 'get_task_validation_status']