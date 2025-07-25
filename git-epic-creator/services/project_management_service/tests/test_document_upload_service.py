"""
Tests for document upload service in project management service.
"""

import pytest
from unittest.mock import Mock, patch, AsyncMock
import tempfile
import os
import sys
from uuid import uuid4
from datetime import datetime

# Add the src directory to the Python path
sys.path.insert(0, os.path.join(os.path.dirname(__file__), '..', 'src'))
# Add the shared directory to the path
sys.path.insert(0, os.path.join(os.path.dirname(__file__), '..', '..', 'shared', 'src'))

from models.document_schemas import BulkUploadResponse


class TestDocumentUploadService:
    """Test cases for DocumentUploadService."""

    def setup_method(self):
        """Set up test fixtures."""
        self.project_id = uuid4()

    def _create_mock_upload_file(self, filename: str, content: bytes):
        """Create a mock UploadFile for testing."""
        mock_file = Mock()
        mock_file.filename = filename
        mock_file.read = AsyncMock(return_value=content)
        mock_file.size = len(content)
        return mock_file

    @patch('services.document_upload_service.process_project_documents_task')
    @patch('services.document_upload_service.BlobStorageClient')
    async def test_bulk_upload_documents_success(self, mock_blob_client_class, mock_task):
        """Test successful bulk document upload."""
        # Import here to avoid initialization issues
        from services.document_upload_service import DocumentUploadService
        
        # Mock blob storage client instance
        mock_blob_instance = Mock()
        mock_blob_client_class.return_value = mock_blob_instance
        
        # Mock successful upload results
        mock_upload_result = Mock()
        mock_upload_result.success = True
        mock_blob_instance.upload_file.return_value = mock_upload_result
        
        # Mock task submission
        mock_task.delay.return_value = Mock(id='test-task-id')
        
        # Create service instance
        service = DocumentUploadService()
        
        # Create test files
        test_files = [
            self._create_mock_upload_file("test1.pdf", b"PDF content"),
            self._create_mock_upload_file("test2.docx", b"DOCX content")
        ]
        
        # Act
        result = await service.bulk_upload_documents(self.project_id, test_files)
        
        # Assert
        assert isinstance(result, BulkUploadResponse)
        assert result.project_id == self.project_id
        assert result.total_files == 2
        assert result.successful_uploads == 2
        assert result.failed_uploads == 0
        assert result.processing_initiated is True
        assert len(result.uploaded_files) == 2
        assert "test1.pdf" in result.uploaded_files
        assert "test2.docx" in result.uploaded_files
        assert len(result.failed_files) == 0
        
        # Verify blob storage was called
        assert mock_blob_instance.upload_file.call_count == 2
        
        # Verify task was submitted
        mock_task.delay.assert_called_once_with(str(self.project_id))

    @patch('services.document_upload_service.process_project_documents_task')
    @patch('services.document_upload_service.BlobStorageClient')
    async def test_bulk_upload_documents_partial_failure(self, mock_blob_client_class, mock_task):
        """Test bulk document upload with some failures."""
        # Import here to avoid initialization issues
        from services.document_upload_service import DocumentUploadService
        
        # Mock blob storage client instance
        mock_blob_instance = Mock()
        mock_blob_client_class.return_value = mock_blob_instance
        
        # Mock mixed upload results
        def mock_upload_side_effect(*args, **kwargs):
            if "success.pdf" in args[1]:
                result = Mock()
                result.success = True
                return result
            else:
                result = Mock()
                result.success = False
                result.error_message = "Upload failed"
                return result
        
        mock_blob_instance.upload_file.side_effect = mock_upload_side_effect
        
        # Mock task submission
        mock_task.delay.return_value = Mock(id='test-task-id')
        
        # Create service instance
        service = DocumentUploadService()
        
        # Create test files
        test_files = [
            self._create_mock_upload_file("success.pdf", b"PDF content"),
            self._create_mock_upload_file("failure.docx", b"DOCX content")
        ]
        
        # Act
        result = await service.bulk_upload_documents(self.project_id, test_files)
        
        # Assert
        assert isinstance(result, BulkUploadResponse)
        assert result.project_id == self.project_id
        assert result.total_files == 2
        assert result.successful_uploads == 1
        assert result.failed_uploads == 1
        assert result.processing_initiated is True  # Should still initiate if any files succeeded
        assert len(result.uploaded_files) == 1
        assert "success.pdf" in result.uploaded_files
        assert len(result.failed_files) == 1
        assert "failure.docx" in result.failed_files
        
        # Verify task was still submitted (since some files succeeded)
        mock_task.delay.assert_called_once_with(str(self.project_id))

    @patch('services.document_upload_service.process_project_documents_task')
    @patch('services.document_upload_service.BlobStorageClient')
    async def test_bulk_upload_documents_all_failures(self, mock_blob_client_class, mock_task):
        """Test bulk document upload with all failures."""
        # Import here to avoid initialization issues
        from services.document_upload_service import DocumentUploadService
        
        # Mock blob storage client instance
        mock_blob_instance = Mock()
        mock_blob_client_class.return_value = mock_blob_instance
        
        # Mock failed upload results
        mock_upload_result = Mock()
        mock_upload_result.success = False
        mock_upload_result.error_message = "Upload failed"
        mock_blob_instance.upload_file.return_value = mock_upload_result
        
        # Create service instance
        service = DocumentUploadService()
        
        # Create test files
        test_files = [
            self._create_mock_upload_file("fail1.pdf", b"PDF content"),
            self._create_mock_upload_file("fail2.docx", b"DOCX content")
        ]
        
        # Act
        result = await service.bulk_upload_documents(self.project_id, test_files)
        
        # Assert
        assert isinstance(result, BulkUploadResponse)
        assert result.project_id == self.project_id
        assert result.total_files == 2
        assert result.successful_uploads == 0
        assert result.failed_uploads == 2
        assert result.processing_initiated is False  # Should not initiate if no files succeeded
        assert len(result.uploaded_files) == 0
        assert len(result.failed_files) == 2
        
        # Verify task was not submitted (since no files succeeded)
        mock_task.delay.assert_not_called()

    @patch('services.document_upload_service.BlobStorageClient')
    async def test_bulk_upload_documents_empty_file_list(self, mock_blob_client_class):
        """Test bulk document upload with empty file list."""
        # Import here to avoid initialization issues
        from services.document_upload_service import DocumentUploadService
        
        # Mock blob storage client instance
        mock_blob_instance = Mock()
        mock_blob_client_class.return_value = mock_blob_instance
        
        # Create service instance
        service = DocumentUploadService()
        
        # Act
        result = await service.bulk_upload_documents(self.project_id, [])
        
        # Assert
        assert isinstance(result, BulkUploadResponse)
        assert result.project_id == self.project_id
        assert result.total_files == 0
        assert result.successful_uploads == 0
        assert result.failed_uploads == 0
        assert result.processing_initiated is False
        assert len(result.uploaded_files) == 0
        assert len(result.failed_files) == 0