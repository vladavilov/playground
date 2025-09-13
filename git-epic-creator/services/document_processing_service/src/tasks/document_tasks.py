
"""Celery tasks for document processing."""

from typing import Dict, Any
import os
import asyncio
import structlog

from services.tika_processor import TikaProcessor
from utils.blob_storage import BlobStorageClient
from clients.project_management_client import ProjectManagementClient
from tasks.document_core import process_project_documents_core
from celery_worker_app import celery_app
from utils.redis_client import get_redis_client, get_sync_redis_client
from utils.ingestion_gating import should_enqueue_sync, post_run_cleanup

logger = structlog.get_logger(__name__)

from constants import (
    TASK_PROCESS_PROJECT_DOCS,
    TASK_RUN_GRAPHRAG_JOB,
    QUEUE_NEO4J_INGESTION,
    QUEUE_DOCUMENT_PROCESSING,
    GATE_NS_DOCS,
    GATE_NS_INGESTION,
    GATE_DEFAULT_RUNNING_TTL,
)

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


@celery_app.task(bind=True, name=TASK_PROCESS_PROJECT_DOCS)
def process_project_documents_task(self, project_id: str) -> Dict[str, Any]:
    """
    Process all documents for a project from blob storage.
    
    Args:
        self: Celery task instance (required for bind=True)
        project_id: Project ID
        
    Returns:
        Dict[str, Any]: Processing result
    """
    logger.info("TASK EXECUTION STARTED - project documents processing", 
                project_id=project_id,
                task_id=self.request.id,
                correlation_id=getattr(self.request, 'correlation_id', 'unknown'),
                worker_pid=os.getpid())

    try:
        # Acquire a per-project distributed lock to avoid concurrent processing (sync lock)
        redis_client = get_redis_client()
        sync_client = get_sync_redis_client()
        lock_key = f"{GATE_NS_DOCS}:lock:{project_id}"
        sync_lock = sync_client.lock(lock_key, timeout=GATE_DEFAULT_RUNNING_TTL, blocking=False)
        if not sync_lock.acquire(blocking=False):
            logger.info("Document processing busy; skipping", project_id=project_id)
            return {
                'success': True,
                'project_id': project_id,
                'skipped': True
            }
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

        if isinstance(result, dict):
            try:
                if should_enqueue_sync(GATE_NS_INGESTION, project_id, client=sync_client):
                    celery_app.send_task(
                        TASK_RUN_GRAPHRAG_JOB,
                        args=[self.request.id, project_id, 0],
                        queue=QUEUE_NEO4J_INGESTION,
                    )
                else:
                    logger.info('Ingestion enqueue suppressed by gating', project_id=project_id)
            except Exception as pub_error:
                logger.error('Failed to enqueue ingestion task', error=str(pub_error))
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
    finally:
        try:
            if 'sync_lock' in locals():
                try:
                    sync_lock.release()
                except Exception:
                    pass
        except Exception:
            pass
        # Post-run cleanup for document processing namespace: clear running and
        # enqueue a single follow-up if a pending marker exists
        try:
            def _enqueue(_job_id: str, _project_id: str, _attempts: int) -> None:
                celery_app.send_task(
                    TASK_PROCESS_PROJECT_DOCS,
                    args=[_project_id],
                    queue=QUEUE_DOCUMENT_PROCESSING,
                )

            post_run_cleanup(
                GATE_NS_DOCS,
                str(self.request.id),
                project_id,
                0,
                client=redis_client,
                enqueue_callable=_enqueue,
            )
        except Exception:
            pass
