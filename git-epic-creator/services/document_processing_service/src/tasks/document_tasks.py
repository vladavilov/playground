
"""
Celery tasks for document processing operations.
"""

from typing import Dict, Any
from uuid import UUID
import os
import tempfile
import asyncio
import structlog

# Import celery app from main module
from main import celery_app
from services.tika_processor import TikaProcessor
from utils.blob_storage import BlobStorageClient
from services.project_management_client import ProjectManagementClient
from utils.postgres_client import get_postgres_client
from utils.redis_client import get_redis_client
from models.project_rest import ProjectStatus

logger = structlog.get_logger(__name__)

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
        project_id: Project ID
        
    Returns:
        Dict[str, Any]: Processing result
    """
    logger.info("Starting project documents processing", project_id=project_id)

    try:
        # Initialize clients
        blob_client = BlobStorageClient()
        tika_processor = TikaProcessor()

        # Get list of files from blob storage for this project
        list_result = blob_client.list_files(project_id=UUID(project_id))

        if not list_result.success:
            error_msg = f"Failed to list files for project: {list_result.error_message}"
            logger.error("Failed to list project files", project_id=project_id, error=error_msg)
            return {
                'success': False,
                'project_id': project_id,
                'error_message': error_msg
            }

        file_list = list_result.file_list or []
        total_documents = len(file_list)
        processed_documents = 0
        failed_documents = 0
        all_extracted_data = []

        logger.info("Found files to process", project_id=project_id, file_count=total_documents)

        # Process each file
        for blob_name in file_list:
            try:
                # Create temporary file for processing
                with tempfile.NamedTemporaryFile(delete=False, 
                                               suffix=os.path.splitext(blob_name)[1]) as temp_file:
                    temp_file_path = temp_file.name

                try:
                    download_result = blob_client.download_file(blob_name, temp_file_path)

                    if not download_result.success:
                        logger.error("Failed to download file", 
                                   blob_name=blob_name, 
                                   error=download_result.error_message)
                        failed_documents += 1
                        continue

                    processing_result = tika_processor.extract_text_with_result(temp_file_path)

                    if processing_result.success:
                        document_data = {
                            'filename': os.path.basename(blob_name),
                            'blob_name': blob_name,
                            'extracted_text': processing_result.extracted_text,
                            'file_type': processing_result.file_type,
                            'page_count': processing_result.page_count,
                            'metadata': processing_result.metadata,
                            'structured_data': processing_result.to_structured_json()
                        }
                        all_extracted_data.append(document_data)
                        processed_documents += 1

                        logger.info("Document processed successfully", 
                                   blob_name=blob_name,
                                   text_length=len(processing_result.extracted_text or ""))
                    else:
                        failed_documents += 1
                        logger.error("Document processing failed", 
                                   blob_name=blob_name,
                                   error=processing_result.error_message)

                    # Send progress update via HTTP
                    try:
                        progress_result = asyncio.run(_update_project_progress_via_http(
                            project_id,
                            processed_documents + failed_documents,
                            total_documents
                        ))
                        if not progress_result['success']:
                            logger.warning("Progress update failed, continuing processing", 
                                         project_id=project_id,
                                         error=progress_result.get('error_message'))
                    except Exception as progress_error:
                        logger.error("Progress update failed with exception, continuing processing",
                                   project_id=project_id,
                                   error=str(progress_error))

                finally:
                    # Clean up temporary file
                    if os.path.exists(temp_file_path):
                        os.unlink(temp_file_path)

            except Exception as e:
                failed_documents += 1
                logger.error("File processing failed", blob_name=blob_name, error=str(e))

        # Send all collected data to Graph RAG Context Service
        if all_extracted_data:
            try:
                # TODO: Implement Graph RAG Context Service integration
                # graph_rag_result = send_to_graph_rag_service(project_id, all_extracted_data)
                logger.info("Would send data to Graph RAG Context Service", 
                           project_id=project_id, 
                           document_count=len(all_extracted_data))
            except Exception as e:
                logger.error("Failed to send data to Graph RAG Context Service",
                           project_id=project_id,
                           error=str(e))

        # Clean up blob storage files
        for blob_name in file_list:
            try:
                delete_result = blob_client.delete_file(blob_name)
                if delete_result.success:
                    logger.info("File deleted from blob storage", blob_name=blob_name)
                else:
                    logger.warning("Failed to delete file from blob storage",
                                 blob_name=blob_name,
                                 error=delete_result.error_message)
            except Exception as e:
                logger.error("Error deleting file from blob storage",
                           blob_name=blob_name,
                           error=str(e))

        # Final progress update (completion)
        try:
            final_progress_result = asyncio.run(_update_project_progress_via_http(
                project_id,
                total_documents,
                total_documents
            ))
            if not final_progress_result['success']:
                logger.warning("Final progress update failed",
                             project_id=project_id,
                             error=final_progress_result.get('error_message'))
        except Exception as final_progress_error:
            logger.error("Final progress update failed with exception",
                       project_id=project_id,
                       error=str(final_progress_error))

        logger.info("Project documents processing completed",
                   project_id=project_id,
                   processed_documents=processed_documents,
                   failed_documents=failed_documents)

        return {
            'success': True,
            'project_id': project_id,
            'total_documents': total_documents,
            'processed_documents': processed_documents,
            'failed_documents': failed_documents,
            'extracted_data_count': len(all_extracted_data)
        }

    except Exception as e:
        error_msg = str(e)
        logger.error("Document collection processing failed",
                    project_id=project_id,
                    error=error_msg)
        return {
            'success': False,
            'project_id': project_id,
            'error_message': error_msg
        }
