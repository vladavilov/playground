
"""
Celery tasks for document processing operations.
"""

from typing import Dict, Any
from uuid import UUID
import os
import asyncio
import structlog

from services.tika_processor import TikaProcessor
from utils.blob_storage import BlobStorageClient
from services.project_management_client import ProjectManagementClient
from tasks.document_core import process_project_documents_core

logger = None

# Import the Celery app from the factory to avoid circular imports
from utils.celery_factory import get_celery_app

# Get the Celery app instance
celery_app = get_celery_app("document_processing_service")

async def _update_project_progress_via_http(
    project_id: str, 
    processed_count: int, 
    total_count: int
) -> Dict[str, Any]:
    """
    Update project processing progress via HTTP client.
    
    Args:
        project_id: Project ID
        processed_count: Number of processed documents
        total_count: Total number of documents
        
    Returns:
        Dict[str, Any]: Update result
    """
    logger.info("Updating project progress via HTTP", 
                project_id=project_id, 
                processed_count=processed_count, 
                total_count=total_count)

    try:
        async with ProjectManagementClient() as client:
            result = await client.update_project_status(
                project_id=project_id,
                processed_count=processed_count,
                total_count=total_count
            )

            if result.success:
                logger.info("Project progress updated successfully via HTTP", 
                           project_id=project_id, 
                           processed_count=processed_count,
                           total_count=total_count)
                return {
                    'success': True,
                    'project_id': project_id,
                    'processed_count': processed_count,
                    'total_count': total_count,
                    'status_code': result.status_code
                }

            logger.error("Project progress update failed via HTTP", 
                        project_id=project_id, 
                        error=result.error_message,
                        status_code=result.status_code)
            return {
                'success': False,
                'project_id': project_id,
                'error_message': result.error_message,
                'status_code': result.status_code
            }

    except Exception as e:
        error_msg = str(e)
        logger.error("Project progress update failed with exception", 
                    project_id=project_id, 
                    error=error_msg)
        return {
            'success': False,
            'project_id': project_id,
            'error_message': error_msg
        }


@celery_app.task(bind=True, name='tasks.document_tasks.process_project_documents_task')
def process_project_documents_task(self, project_id: str) -> Dict[str, Any]:
    """
    Process all documents for a project from blob storage.
    
    Args:
        self: Celery task instance (required for bind=True)
        project_id: Project ID
        
    Returns:
        Dict[str, Any]: Processing result
    """
    global logger
    logger= structlog.get_logger(__name__)
    
    logger.info("TASK EXECUTION STARTED - project documents processing", 
                project_id=project_id,
                task_id=self.request.id,
                correlation_id=getattr(self.request, 'correlation_id', 'unknown'),
                worker_pid=os.getpid())

    try:
        blob_client = BlobStorageClient()
        tika_processor = TikaProcessor()

        def _send_progress_update(pid: str, processed: int, total: int) -> Dict[str, Any]:
            return asyncio.run(_update_project_progress_via_http(pid, processed, total))

        result = process_project_documents_core(
            project_id=project_id,
            blob_client=blob_client,
            tika_processor=tika_processor,
            send_progress_update=_send_progress_update,
            logger=logger,
        )

        # Attach task_id for compatibility
        if isinstance(result, dict):
            result.setdefault('project_id', project_id)
            result['task_id'] = self.request.id
        return result

    except Exception as e:
        error_msg = str(e)
        logger.error("TASK EXECUTION FAILED - document collection processing failed",
                    project_id=project_id,
                    task_id=self.request.id,
                    error=error_msg)
        return {
            'success': False,
            'project_id': project_id,
            'task_id': self.request.id,
            'error_message': error_msg
        }

def get_celery_app_with_tasks():
    """
    Get the Celery app instance that has tasks registered.
    This provides access to the configured app with registered tasks.
    """
    return celery_app