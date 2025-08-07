"""
Azure Blob Storage client for temporary file storage.
"""

import os
from typing import Optional, List
from dataclasses import dataclass
from uuid import UUID
from functools import lru_cache
import structlog
from azure.storage.blob import BlobServiceClient
from azure.core.exceptions import ResourceExistsError, HttpResponseError
from configuration.blob_storage_config import BlobStorageSettings, get_blob_storage_settings

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

    def __init__(self, settings: Optional[BlobStorageSettings] = None):
        """
        Initialize the Azure Blob Storage client.
        
        Args:
            settings: Blob storage configuration settings
        """
        self.settings = settings or get_blob_storage_settings()
        self.connection_string = self.settings.AZURE_STORAGE_CONNECTION_STRING
        self.base_container_name = self.settings.AZURE_STORAGE_CONTAINER_NAME
        
        if not self.connection_string:
            raise ValueError("Azure Storage connection string not provided")
        
        self.blob_service_client = None
        logger.info("BlobStorageClient initialized successfully", base_container_name=self.base_container_name)

    def _get_blob_service_client(self):
        """Get or create the blob service client."""
        if self.blob_service_client is None:    
            self.blob_service_client = BlobServiceClient.from_connection_string(self.connection_string)
        return self.blob_service_client

    def _get_container_name(self, project_id: Optional[UUID] = None) -> str:
        """
        Generate container name based on project ID.
        
        Args:
            project_id: Optional project ID for project-specific containers
            
        Returns:
            str: Container name
        """
        if project_id:
            return f"{self.base_container_name}-{project_id}"
        return self.base_container_name

    def _ensure_container_exists(self, project_id: Optional[UUID] = None) -> bool:
        """
        Ensure the storage container exists, creating it if necessary.
        
        Args:
            project_id: Optional project ID for project-specific containers
            
        Returns:
            bool: True if container exists or was created successfully, False otherwise
        """
        container_name = self._get_container_name(project_id)
        try:
            blob_service_client = self._get_blob_service_client()
            container_client = blob_service_client.get_container_client(container_name)
            
            # Check if container already exists
            if container_client.exists():
                logger.debug("Container already exists", container_name=container_name)
                return True
            
            # Create the container
            container_client.create_container()
            logger.info("Container created successfully", container_name=container_name)
            return True
            
        except ResourceExistsError:
            # Container was created by another process between our check and create
            logger.debug("Container already exists (created by another process)", container_name=container_name)
            return True
        except (HttpResponseError, Exception) as e:
            error_msg = str(e)
            logger.error("Failed to create container", container_name=container_name, error=error_msg)
            return False

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
            # Ensure container exists before attempting upload
            if not self._ensure_container_exists(project_id):
                container_name = self._get_container_name(project_id)
                error_msg = "Container does not exist and could not be created"
                logger.error("Container creation failed", container_name=container_name)
                return BlobStorageResult(
                    success=False,
                    error_message=error_msg
                )

            # Use blob name as-is (no project prefixing)
            full_blob_name = blob_name
            container_name = self._get_container_name(project_id)

            blob_service_client = self._get_blob_service_client()
            container_client = blob_service_client.get_container_client(container_name)

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

    def download_file(self, blob_name: str, local_path: str, project_id: Optional[UUID] = None) -> BlobStorageResult:
        """
        Download a file from Azure Blob Storage.
        
        Args:
            blob_name: Name of the blob to download
            local_path: Local path where to save the file
            project_id: Optional project ID for project-specific containers
            
        Returns:
            BlobStorageResult: Download operation result
        """
        logger.info("Starting file download", blob_name=blob_name, local_path=local_path)

        try:
            # Ensure container exists before attempting download
            if not self._ensure_container_exists(project_id):
                container_name = self._get_container_name(project_id)
                error_msg = "Container does not exist and could not be created"
                logger.error("Container creation failed", container_name=container_name)
                return BlobStorageResult(
                    success=False,
                    error_message=error_msg
                )

            container_name = self._get_container_name(project_id)
            blob_service_client = self._get_blob_service_client()
            container_client = blob_service_client.get_container_client(container_name)

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

    def delete_file(self, blob_name: str, project_id: Optional[UUID] = None) -> BlobStorageResult:
        """
        Delete a file from Azure Blob Storage.
        
        Args:
            blob_name: Name of the blob to delete
            project_id: Optional project ID for project-specific containers
            
        Returns:
            BlobStorageResult: Delete operation result
        """
        logger.info("Starting file deletion", blob_name=blob_name)

        try:
            # Ensure container exists before attempting deletion
            if not self._ensure_container_exists(project_id):
                container_name = self._get_container_name(project_id)
                error_msg = "Container does not exist and could not be created"
                logger.error("Container creation failed", container_name=container_name)
                return BlobStorageResult(
                    success=False,
                    error_message=error_msg
                )

            container_name = self._get_container_name(project_id)
            blob_service_client = self._get_blob_service_client()
            container_client = blob_service_client.get_container_client(container_name)

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
            # Ensure container exists before attempting to list files
            if not self._ensure_container_exists(project_id):
                container_name = self._get_container_name(project_id)
                error_msg = "Container does not exist and could not be created"
                logger.error("Container creation failed", container_name=container_name)
                return BlobStorageResult(
                    success=False,
                    error_message=error_msg
                )

            container_name = self._get_container_name(project_id)
            blob_service_client = self._get_blob_service_client()
            container_client = blob_service_client.get_container_client(container_name)

            # With project-specific containers, use prefix as-is (no project prefixing needed)
            list_prefix = prefix

            blob_list = []
            for blob in container_client.list_blobs(name_starts_with=list_prefix):
                blob_list.append(blob.name)

            logger.info("File listing completed", file_count=len(blob_list), prefix=list_prefix, container_name=container_name)

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
                delete_result = self.delete_file(blob_name, project_id=project_id)
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


@lru_cache()
def get_blob_storage_client() -> BlobStorageClient:
    """
    Creates a cached instance of BlobStorageClient.
    This ensures that the client is created only once and reused.
    """
    return BlobStorageClient()