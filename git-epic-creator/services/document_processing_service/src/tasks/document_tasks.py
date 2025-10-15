
"""Celery tasks for document processing."""

from __future__ import annotations
from typing import Dict, Any, TYPE_CHECKING
import os
import structlog

from utils.blob_storage import BlobStorageClient
from clients.project_management_client import ProjectManagementClient
from tasks.document_core import process_project_documents_core
from celery_worker_app import celery_app, get_docling_processor, get_tika_processor
from utils.redis_client import get_sync_redis_client
from utils.workflow_gating import gate_and_enqueue_sync, cleanup_after_run_sync
from utils.asyncio_runner import run_async

if TYPE_CHECKING:
    from services.docling_processor import DoclingProcessor
    from services.tika_processor import TikaProcessor

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


class _ProcessorDispatcher:
    """Selects Docling for PDFs/images and Tika for other formats.

    Exposes the same API used by process_project_documents_core: extract_text_with_result(path).
    """

    def __init__(self, docling: DoclingProcessor, tika: TikaProcessor) -> None:
        self._docling = docling
        self._tika = tika

    def extract_text_with_result(self, file_path: str):  # type: ignore[override]
        ext = os.path.splitext(file_path)[1].lower()
        docling_exts = set((self._docling.settings.DOCLING_IMAGE_EXTENSIONS or "").split(","))
        docling_exts = {e.strip().lower() for e in docling_exts if e.strip()}
        if ext == ".pdf" or ext in docling_exts:
            if self._docling.is_supported_format(file_path):
                return self._docling.extract_text_with_result(file_path)
        return self._tika.extract_text_with_result(file_path)


async def _update_project_progress_via_http(
    project_id: str,
    processed_count: int,
    total_count: int,
    authorization_header: str,
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
                total_count=total_count,
                authorization_header=authorization_header,
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
        auth_header = self.request.headers.get('Authentication')
        if not auth_header:
            raise RuntimeError("Missing Authentication for task")
        # Acquire a per-project distributed lock to avoid concurrent processing (sync lock)
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
        # Use singleton processors initialized at worker startup
        # This avoids per-task initialization overhead and reuses pre-loaded plugins/models
        document_processor = _ProcessorDispatcher(get_docling_processor(), get_tika_processor())

        def _send_progress_update(pid: str, processed: int, total: int) -> Dict[str, Any]:
            """Synchronous wrapper for async progress update using persistent event loop."""
            logger.info("PROGRESS_UPDATE_CALL",
                       project_id=pid,
                       processed_count=processed,
                       total_count=total,
                       expected_status="processing" if processed == 0 else ("active" if processed >= total else "processing"))
            
            try:
                # Use persistent event loop runner (consistent with neo4j_ingestion_service)
                result = run_async(_update_project_progress_via_http(pid, processed, total, authorization_header=auth_header))
                
                logger.info("PROGRESS_UPDATE_COMPLETED",
                          project_id=pid,
                          processed=processed,
                          total=total,
                          success=result.get('success'))
                return result
                    
            except Exception as progress_error:
                logger.error("PROGRESS_UPDATE_EXCEPTION",
                           project_id=pid,
                           processed=processed,
                           total=total,
                           error=str(progress_error),
                           error_type=type(progress_error).__name__,
                           exc_info=True)
                return {
                    'success': False,
                    'project_id': pid,
                    'error_message': f"Progress update failed: {str(progress_error)}"
                }

        result = process_project_documents_core(
            project_id=project_id,
            blob_client=blob_client,
            document_processor=document_processor,
            send_progress_update=_send_progress_update,
            logger=logger,
        )

        if isinstance(result, dict):
            # Only trigger ingestion if there are valid documents with content
            processed_count = result.get("processed_documents", 0)
            empty_count = result.get("empty_documents", 0)
            
            if processed_count > 0:
                # We have valid documents with content - trigger ingestion
                try:
                    def _enqueue_ingestion() -> None:
                        logger.info("Enqueueing GraphRAG ingestion task",
                                   project_id=project_id,
                                   task_id=self.request.id,
                                   processed_documents=processed_count,
                                   has_auth_header=bool(auth_header),
                                   auth_header_length=len(auth_header) if auth_header else 0)
                        celery_app.send_task(
                            TASK_RUN_GRAPHRAG_JOB,
                            args=[self.request.id, project_id, 0],
                            queue=QUEUE_NEO4J_INGESTION,
                            headers={"Authentication": auth_header},
                        )

                    gate_and_enqueue_sync(
                        GATE_NS_INGESTION,
                        project_id,
                        client=sync_client,
                        enqueue=_enqueue_ingestion,
                    )
                except Exception as pub_error:
                    logger.error('Failed to enqueue ingestion task', error=str(pub_error))
            else:
                # No valid documents to ingest - update project status but don't trigger ingestion
                logger.warning(
                    "Skipping ingestion trigger - no valid documents with content",
                    project_id=project_id,
                    empty_documents=empty_count,
                    failed_documents=result.get("failed_documents", 0),
                    total_documents=result.get("total_documents", 0)
                )
                
                # Update project status to indicate empty content issue
                try:
                    error_message = f"All documents ({empty_count}) have empty or whitespace-only content"
                    async def _update_status():
                        async with ProjectManagementClient() as client:
                            await client.update_project_status(
                                project_id=project_id,
                                status="rag_failed",
                                error_message=error_message,
                                authorization_header=auth_header,
                            )
                    run_async(_update_status())
                except Exception as status_error:
                    logger.error('Failed to update project status for empty documents', error=str(status_error))
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
        # Post-run cleanup for document processing namespace: enqueue a single follow-up if a pending marker exists
        try:
            def _enqueue_docs_follow_up() -> None:
                celery_app.send_task(
                    TASK_PROCESS_PROJECT_DOCS,
                    args=[project_id],
                    queue=QUEUE_DOCUMENT_PROCESSING,
                    headers={"Authentication": auth_header},
                )

            cleanup_after_run_sync(
                GATE_NS_DOCS,
                project_id,
                client=sync_client,
                enqueue_follow_up=_enqueue_docs_follow_up,
            )
        except Exception:
            pass
