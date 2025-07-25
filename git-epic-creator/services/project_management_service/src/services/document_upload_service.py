"""
Business logic service for document upload operations in project management service.
"""

from datetime import datetime, timezone
import tempfile
import os
from typing import List
from uuid import UUID, uuid4
import structlog
from fastapi import UploadFile

from utils.blob_storage import BlobStorageClient
from models.document_schemas import BulkUploadResponse

# Import the task from document processing service
# This will be available when both services are running
try:
    from tasks.document_tasks import process_project_documents_task
except ImportError:
    # Fallback for testing or when document processing service is not available
    process_project_documents_task = None

logger = structlog.get_logger(__name__)


class DocumentUploadService:
    """
    Service class for document upload operations in project management service.
    Handles business logic for uploading documents to Azure Blob Storage.
    """

    def __init__(self):
        """
        Initialize the document upload service.
        """
        self.blob_storage_client = BlobStorageClient()
        logger.info("DocumentUploadService initialized")

    async def bulk_upload_documents(
        self,
        project_id: UUID,
        files: List[UploadFile]
    ) -> BulkUploadResponse:
        """
        Upload multiple documents and initiate bulk processing.

        Args:
            project_id: Project ID to associate the documents with
            files: List of uploaded files

        Returns:
            BulkUploadResponse: Upload response with processing status
        """
        logger.info("Processing bulk document upload", 
                   project_id=str(project_id), 
                   file_count=len(files))

        uploaded_files = []
        failed_files = []
        successful_uploads = 0
        failed_uploads = 0

        for file in files:
            filename = file.filename or "unknown"
            try:
                # Create temporary file
                with tempfile.NamedTemporaryFile(delete=False,
                                               suffix=os.path.splitext(filename)[1]) as temp_file:
                    # Read file content
                    content = await file.read()
                    temp_file.write(content)
                    temp_file.flush()

                    # Upload to blob storage with project-specific path
                    blob_name = f"projects/{project_id}/documents/{uuid4()}_{filename}"
                    upload_result = self.blob_storage_client.upload_file(
                        temp_file.name,
                        blob_name,
                        project_id=project_id
                    )

                    # Clean up temporary file
                    try:
                        os.unlink(temp_file.name)
                    except OSError as cleanup_error:
                        logger.warning("Failed to cleanup temporary file",
                                     temp_file=temp_file.name,
                                     error=str(cleanup_error))
                        # Don't fail the upload if cleanup fails

                    if upload_result.success:
                        uploaded_files.append(filename)
                        successful_uploads += 1
                        logger.info("File uploaded successfully",
                                   project_id=str(project_id),
                                   filename=filename,
                                   blob_name=blob_name)
                    else:
                        failed_files.append(filename)
                        failed_uploads += 1
                        logger.error("File upload failed",
                                   filename=filename,
                                   error=upload_result.error_message)

            except Exception as e:
                failed_files.append(filename)
                failed_uploads += 1
                logger.error("File processing failed",
                           filename=filename,
                           error=str(e))

        # Create upload response
        upload_response = BulkUploadResponse(
            project_id=project_id,
            total_files=len(files),
            successful_uploads=successful_uploads,
            failed_uploads=failed_uploads,
            upload_time=datetime.now(timezone.utc),
            processing_initiated=successful_uploads > 0,
            uploaded_files=uploaded_files,
            failed_files=failed_files
        )

        # Initiate background processing if any files were uploaded successfully
        if successful_uploads > 0 and process_project_documents_task is not None:
            try:
                process_project_documents_task.delay(str(project_id))
                logger.info("Background processing initiated",
                           project_id=str(project_id),
                           file_count=successful_uploads)
            except Exception as e:
                logger.error("Failed to initiate background processing",
                           project_id=str(project_id),
                           error=str(e))
                # Don't fail the upload response if task submission fails

        logger.info("Bulk document upload completed", 
                   project_id=str(project_id), 
                   successful_uploads=successful_uploads,
                   failed_uploads=failed_uploads)

        return upload_response