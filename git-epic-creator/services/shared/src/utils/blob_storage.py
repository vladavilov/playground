"""
Azure Blob Storage client for temporary file storage.
"""

import os
from typing import Optional, List
from dataclasses import dataclass
from uuid import UUID
import structlog
from azure.storage.blob import BlobServiceClient

logger = structlog.get_logger(__name__)


@dataclass
class BlobStorageResult:
    """
    Result of blob storage operation.
    """
    success: bool = False
    blob_name: Optional[str] = None
    blob_url: Optional[str] = None
    local_path: Optional[str] = None
    file_list: Optional[List[str]] = None
    error_message: Optional[str] = None


class BlobStorageClient:
    """
    Azure Blob Storage client for temporary file storage.
    Handles document upload, download, and cleanup operations.
    """

    def __init__(self, connection_string: Optional[str] = None, container_name: str = "documents"):
        """
        Initialize the Azure Blob Storage client.
        
        Args:
            connection_string: Azure Storage connection string (defaults to environment variable)
            container_name: Name of the blob container
        """
        self.connection_string = connection_string or os.getenv('AZURE_STORAGE_CONNECTION_STRING')
        self.container_name = container_name
        if not self.connection_string:
                raise ValueError("Azure Storage connection string not provided")
        
        self.blob_service_client = None
        logger.info("BlobStorageClient initialized successfully", container_name=container_name)

    def _get_blob_service_client(self):
        """Get or create the blob service client."""
        if self.blob_service_client is None:    
            self.blob_service_client = BlobServiceClient.from_connection_string(self.connection_string)
        return self.blob_service_client

    def upload_file(self, file_path: str, blob_name: str, project_id: Optional[UUID] = None) -> BlobStorageResult:
        """
        Upload a file to Azure Blob Storage.
        
        Args:
            file_path: Local path to the file to upload
            blob_name: Name for the blob in storage
            project_id: Optional project ID for organizing files
            
        Returns:
            BlobStorageResult: Upload operation result
        """
        logger.info("Starting file upload", file_path=file_path, blob_name=blob_name, project_id=str(project_id) if project_id else None)
        
        # Check if file exists
        if not os.path.exists(file_path):
            error_msg = f"File not found: {file_path}"
            logger.error("File not found", file_path=file_path)
            return BlobStorageResult(
                success=False,
                error_message=error_msg
            )

        try:
            full_blob_name = self._generate_blob_name(blob_name, project_id)

            blob_service_client = self._get_blob_service_client()
            container_client = blob_service_client.get_container_client(self.container_name)

            blob_client = container_client.get_blob_client(full_blob_name)

            with open(file_path, 'rb') as data:
                blob_client.upload_blob(data, overwrite=True)

            blob_url = blob_client.url

            logger.info("File upload completed",
                       file_path=file_path,
                       blob_name=full_blob_name,
                       blob_url=blob_url)

            return BlobStorageResult(
                success=True,
                blob_name=full_blob_name,
                blob_url=blob_url
            )

        except Exception as e:
            error_msg = str(e)
            logger.error("File upload failed", file_path=file_path, blob_name=blob_name, error=error_msg)
            return BlobStorageResult(
                success=False,
                error_message=error_msg
            )

    def download_file(self, blob_name: str, local_path: str) -> BlobStorageResult:
        """
        Download a file from Azure Blob Storage.
        
        Args:
            blob_name: Name of the blob to download
            local_path: Local path where to save the file
            
        Returns:
            BlobStorageResult: Download operation result
        """
        logger.info("Starting file download", blob_name=blob_name, local_path=local_path)

        try:
            blob_service_client = self._get_blob_service_client()
            container_client = blob_service_client.get_container_client(self.container_name)

            blob_client = container_client.get_blob_client(blob_name)

            download_stream = blob_client.download_blob()

            with open(local_path, 'wb') as download_file:
                download_file.write(download_stream.readall())

            logger.info("File download completed", blob_name=blob_name, local_path=local_path)

            return BlobStorageResult(
                success=True,
                blob_name=blob_name,
                local_path=local_path
            )

        except Exception as e:
            error_msg = str(e)
            logger.error("File download failed", blob_name=blob_name, local_path=local_path, error=error_msg)
            return BlobStorageResult(
                success=False,
                error_message=error_msg
            )

    def delete_file(self, blob_name: str) -> BlobStorageResult:
        """
        Delete a file from Azure Blob Storage.
        
        Args:
            blob_name: Name of the blob to delete
            
        Returns:
            BlobStorageResult: Delete operation result
        """
        logger.info("Starting file deletion", blob_name=blob_name)

        try:
            blob_service_client = self._get_blob_service_client()
            container_client = blob_service_client.get_container_client(self.container_name)

            blob_client = container_client.get_blob_client(blob_name)

            blob_client.delete_blob()

            logger.info("File deletion completed", blob_name=blob_name)

            return BlobStorageResult(
                success=True,
                blob_name=blob_name
            )

        except Exception as e:
            error_msg = str(e)
            logger.error("File deletion failed", blob_name=blob_name, error=error_msg)
            return BlobStorageResult(
                success=False,
                error_message=error_msg
            )

    def list_files(self, project_id: Optional[UUID] = None, prefix: Optional[str] = None) -> BlobStorageResult:
        """
        List files in Azure Blob Storage.
        
        Args:
            project_id: Optional project ID to filter files
            prefix: Optional prefix to filter files
            
        Returns:
            BlobStorageResult: List operation result with file names
        """
        logger.info("Starting file listing", project_id=str(project_id) if project_id else None, prefix=prefix)

        try:
            blob_service_client = self._get_blob_service_client()
            container_client = blob_service_client.get_container_client(self.container_name)

            list_prefix = prefix
            if project_id and not prefix:
                list_prefix = f"{project_id}/"

            blob_list = []
            for blob in container_client.list_blobs(name_starts_with=list_prefix):
                blob_list.append(blob.name)

            logger.info("File listing completed", file_count=len(blob_list), prefix=list_prefix)

            return BlobStorageResult(
                success=True,
                file_list=blob_list
            )

        except Exception as e:
            error_msg = str(e)
            logger.error("File listing failed", project_id=str(project_id) if project_id else None, prefix=prefix, error=error_msg)
            return BlobStorageResult(
                success=False,
                error_message=error_msg
            )

    def _generate_blob_name(self, filename: str, project_id: Optional[UUID] = None) -> str:
        """
        Generate a blob name with optional project prefix.
        
        Args:
            filename: Original filename
            project_id: Optional project ID for organizing files
            
        Returns:
            str: Generated blob name
        """
        if project_id:
            return f"{project_id}/{filename}"
        return filename

    def cleanup_project_files(self, project_id: UUID) -> BlobStorageResult:
        """
        Clean up all files for a specific project.
        
        Args:
            project_id: Project ID
            
        Returns:
            BlobStorageResult: Cleanup operation result
        """
        logger.info("Starting project file cleanup", project_id=str(project_id))

        try:
            list_result = self.list_files(project_id=project_id)

            if not list_result.success:
                return list_result

            deleted_count = 0
            for blob_name in list_result.file_list or []:
                delete_result = self.delete_file(blob_name)
                if delete_result.success:
                    deleted_count += 1
                else:
                    logger.warning("Failed to delete blob during cleanup", blob_name=blob_name, error=delete_result.error_message)

            logger.info("Project file cleanup completed", project_id=str(project_id), deleted_count=deleted_count)

            return BlobStorageResult(
                success=True,
                file_list=[f"Deleted {deleted_count} files"]
            )

        except Exception as e:
            error_msg = str(e)
            logger.error("Project file cleanup failed", project_id=str(project_id), error=error_msg)
            return BlobStorageResult(
                success=False,
                error_message=error_msg
            )