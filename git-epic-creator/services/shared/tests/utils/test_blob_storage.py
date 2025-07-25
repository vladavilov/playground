"""
Tests for Azure Blob Storage integration in shared utilities.
"""

import pytest
from unittest.mock import Mock, patch
import tempfile
import os
import sys
from uuid import uuid4

# Add the src directory to the Python path
sys.path.insert(0, os.path.join(os.path.dirname(__file__), '..', '..', 'src'))

from utils.blob_storage import BlobStorageClient, BlobStorageResult


class TestBlobStorageClient:
    """Test cases for Azure Blob Storage client."""

    def setup_method(self):
        """Set up test fixtures."""
        # Provide a mock connection string to avoid Azure Storage dependency
        mock_connection_string = "DefaultEndpointsProtocol=https;AccountName=mockaccount;AccountKey=mockkey;EndpointSuffix=core.windows.net"
        self.client = BlobStorageClient(connection_string=mock_connection_string)

    @patch.object(BlobStorageClient, '_get_blob_service_client')
    def test_upload_file_success(self, mock_get_client):
        """Test successful file upload to blob storage."""
        # Mock Azure Blob Storage client
        mock_blob_service = Mock()
        mock_container_client = Mock()
        mock_blob_client = Mock()
        mock_get_client.return_value = mock_blob_service
        mock_blob_service.get_container_client.return_value = mock_container_client
        mock_container_client.get_blob_client.return_value = mock_blob_client
        mock_blob_client.upload_blob.return_value = None
        mock_blob_client.url = "https://mockaccount.blob.core.windows.net/documents/test-document.pdf"

        # Create a temporary file
        with tempfile.NamedTemporaryFile(suffix='.pdf', delete=False) as temp_file:
            temp_file.write(b'test file content')
            temp_file_path = temp_file.name

        try:
            result = self.client.upload_file(temp_file_path, 'test-document.pdf')
            
            assert isinstance(result, BlobStorageResult)
            assert result.success is True
            assert result.blob_url is not None
            assert result.blob_name == 'test-document.pdf'
            assert result.error_message is None
            
            # Verify Azure SDK was called correctly
            mock_get_client.assert_called_once()
            mock_blob_client.upload_blob.assert_called_once()
            
        finally:
            os.unlink(temp_file_path)

    @patch('utils.blob_storage.BlobServiceClient')
    def test_upload_file_with_project_prefix(self, mock_blob_service):
        """Test file upload with project-specific prefix."""
        # Mock Azure Blob Storage client
        mock_container_client = Mock()
        mock_blob_client = Mock()
        mock_blob_service.from_connection_string.return_value = Mock()
        mock_blob_service.from_connection_string.return_value.get_container_client.return_value = mock_container_client
        mock_container_client.get_blob_client.return_value = mock_blob_client
        mock_blob_client.upload_blob.return_value = None
        mock_blob_client.url = "https://mockaccount.blob.core.windows.net/documents/project-id/document.docx"

        project_id = uuid4()
        
        # Create a temporary file
        with tempfile.NamedTemporaryFile(suffix='.docx', delete=False) as temp_file:
            temp_file.write(b'test docx content')
            temp_file_path = temp_file.name

        try:
            result = self.client.upload_file(temp_file_path, 'document.docx', project_id=project_id)
            
            assert result.success is True
            assert str(project_id) in result.blob_name
            assert result.blob_name.endswith('document.docx')
            
        finally:
            os.unlink(temp_file_path)

    def test_upload_file_handles_missing_file(self):
        """Test that upload handles missing files gracefully."""
        result = self.client.upload_file('/nonexistent/file.pdf', 'test.pdf')
        
        assert isinstance(result, BlobStorageResult)
        assert result.success is False
        assert 'File not found' in result.error_message
        assert result.blob_url is None

    @patch('utils.blob_storage.BlobServiceClient')
    def test_download_file_success(self, mock_blob_service):
        """Test successful file download from blob storage."""
        # Mock Azure Blob Storage client
        mock_container_client = Mock()
        mock_blob_client = Mock()
        mock_blob_service.from_connection_string.return_value = Mock()
        mock_blob_service.from_connection_string.return_value.get_container_client.return_value = mock_container_client
        mock_container_client.get_blob_client.return_value = mock_blob_client
        
        # Mock download response
        mock_download_stream = Mock()
        mock_download_stream.readall.return_value = b'downloaded file content'
        mock_blob_client.download_blob.return_value = mock_download_stream

        # Create temporary download path
        with tempfile.NamedTemporaryFile(delete=False) as temp_file:
            download_path = temp_file.name

        try:
            result = self.client.download_file('test-blob.pdf', download_path)
            
            assert isinstance(result, BlobStorageResult)
            assert result.success is True
            assert result.local_path == download_path
            assert result.error_message is None
            
            # Verify file was written
            with open(download_path, 'rb') as f:
                content = f.read()
                assert content == b'downloaded file content'
            
            # Verify Azure SDK was called correctly
            mock_blob_client.download_blob.assert_called_once()
            
        finally:
            if os.path.exists(download_path):
                os.unlink(download_path)

    @patch('utils.blob_storage.BlobServiceClient')
    def test_delete_file_success(self, mock_blob_service):
        """Test successful file deletion from blob storage."""
        # Mock Azure Blob Storage client
        mock_container_client = Mock()
        mock_blob_client = Mock()
        mock_blob_service.from_connection_string.return_value = Mock()
        mock_blob_service.from_connection_string.return_value.get_container_client.return_value = mock_container_client
        mock_container_client.get_blob_client.return_value = mock_blob_client
        mock_blob_client.delete_blob.return_value = None

        result = self.client.delete_file('test-blob.pdf')
        
        assert isinstance(result, BlobStorageResult)
        assert result.success is True
        assert result.error_message is None
        
        # Verify Azure SDK was called correctly
        mock_blob_client.delete_blob.assert_called_once()

    @patch('utils.blob_storage.BlobServiceClient')
    def test_list_files_success(self, mock_blob_service):
        """Test successful file listing from blob storage."""
        # Mock Azure Blob Storage client
        mock_container_client = Mock()
        mock_blob_service.from_connection_string.return_value = Mock()
        mock_blob_service.from_connection_string.return_value.get_container_client.return_value = mock_container_client
        
        # Mock blob list
        mock_blob1 = Mock()
        mock_blob1.name = 'project1/document1.pdf'
        mock_blob2 = Mock()
        mock_blob2.name = 'project1/document2.docx'
        mock_container_client.list_blobs.return_value = [mock_blob1, mock_blob2]

        project_id = uuid4()
        result = self.client.list_files(project_id=project_id)
        
        assert isinstance(result, BlobStorageResult)
        assert result.success is True
        assert len(result.file_list) == 2
        assert 'project1/document1.pdf' in result.file_list
        assert 'project1/document2.docx' in result.file_list

    def test_generate_blob_name_with_project(self):
        """Test blob name generation with project ID."""
        project_id = uuid4()
        blob_name = self.client._generate_blob_name('document.pdf', project_id=project_id)
        
        assert str(project_id) in blob_name
        assert blob_name.endswith('document.pdf')
        assert '/' in blob_name  # Should have project prefix

    def test_generate_blob_name_without_project(self):
        """Test blob name generation without project ID."""
        blob_name = self.client._generate_blob_name('document.pdf')
        
        assert blob_name == 'document.pdf'

    @patch('utils.blob_storage.BlobServiceClient')
    def test_cleanup_project_files_success(self, mock_blob_service):
        """Test successful cleanup of project files."""
        # Mock Azure Blob Storage client
        mock_container_client = Mock()
        mock_blob_client = Mock()
        mock_blob_service.from_connection_string.return_value = Mock()
        mock_blob_service.from_connection_string.return_value.get_container_client.return_value = mock_container_client
        mock_container_client.get_blob_client.return_value = mock_blob_client
        
        # Mock blob list
        mock_blob1 = Mock()
        mock_blob1.name = 'project1/document1.pdf'
        mock_blob2 = Mock()
        mock_blob2.name = 'project1/document2.docx'
        mock_container_client.list_blobs.return_value = [mock_blob1, mock_blob2]
        
        # Mock delete operations
        mock_blob_client.delete_blob.return_value = None

        project_id = uuid4()
        result = self.client.cleanup_project_files(project_id)
        
        assert isinstance(result, BlobStorageResult)
        assert result.success is True
        assert result.file_list is not None
        assert "Deleted 2 files" in result.file_list[0]
        
        # Verify delete was called for each file
        assert mock_blob_client.delete_blob.call_count == 2


class TestBlobStorageResult:
    """Test cases for BlobStorageResult dataclass."""

    def test_blob_storage_result_creation(self):
        """Test creation of BlobStorageResult."""
        result = BlobStorageResult(
            success=True,
            blob_name="test.pdf",
            blob_url="https://example.com/test.pdf",
            local_path="/tmp/test.pdf"
        )
        
        assert result.success is True
        assert result.blob_name == "test.pdf"
        assert result.blob_url == "https://example.com/test.pdf"
        assert result.local_path == "/tmp/test.pdf"
        assert result.file_list is None
        assert result.error_message is None

    def test_blob_storage_result_defaults(self):
        """Test BlobStorageResult with default values."""
        result = BlobStorageResult()
        
        assert result.success is False
        assert result.blob_name is None
        assert result.blob_url is None
        assert result.local_path is None
        assert result.file_list is None
        assert result.error_message is None