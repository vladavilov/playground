"""Document upload service."""

from datetime import datetime, timezone
import tempfile
import os
from typing import List
from uuid import UUID, uuid4
import structlog
from fastapi import UploadFile

from utils.blob_storage import BlobStorageClient
from services.task_publisher import TaskRequestPublisher
from models.document_schemas import BulkUploadResponse

logger = structlog.get_logger(__name__)


class DocumentUploadService:
    """Handles uploading documents to blob storage and triggers processing."""

    def __init__(self, blob_storage_client: BlobStorageClient):
        self.blob_storage_client = blob_storage_client

    async def bulk_upload_documents(
        self,
        project_id: UUID,
        files: List[UploadFile]
    ) -> BulkUploadResponse:
        """Upload multiple documents and request processing if any succeed."""
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
                with tempfile.NamedTemporaryFile(delete=False,
                                               suffix=os.path.splitext(filename)[1]) as temp_file:
                    content = await file.read()
                    temp_file.write(content)
                    temp_file.flush()

                    blob_name = f"input/{uuid4()}_{filename}"
                    upload_result = self.blob_storage_client.upload_file(
                        temp_file.name,
                        blob_name,
                        project_id=project_id
                    )

                    try:
                        os.unlink(temp_file.name)
                    except OSError as cleanup_error:
                        logger.warning("Failed to cleanup temporary file",
                                     temp_file=temp_file.name,
                                     error=str(cleanup_error))

                    if upload_result.success:
                        uploaded_files.append(filename)
                        successful_uploads += 1
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

        if successful_uploads > 0:
            try:
                task_publisher = TaskRequestPublisher()
                task_request_success = await task_publisher.request_document_processing(project_id)
                
                if task_request_success:
                    logger.info("Background processing requested", project_id=str(project_id), file_count=successful_uploads)
                else:
                    logger.error("Processing request failed", project_id=str(project_id))
            except Exception as e:
                logger.error("Processing request error", project_id=str(project_id), error=str(e))

        logger.info("Bulk upload done", project_id=str(project_id), ok=successful_uploads, failed=failed_uploads)

        return upload_response
