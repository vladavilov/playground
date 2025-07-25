"""
Tests for Azure Blob Storage integration in shared utilities.
"""

import pytest
from unittest.mock import Mock, patch, mock_open
import tempfile
import os
from uuid import uuid4
from utils.blob_storage import BlobStorageClient, BlobStorageResult


class TestBlobStorageClient:
    """Test cases for Azure Blob Storage client."""

    @pytest.fixture
    def mock_connection_string(self):
        """Mock Azure Storage connection string."""
        return "DefaultEndpointsProtocol=https;AccountName=mockaccount;AccountKey=mockkey;EndpointSuffix=core.windows.net"

    @pytest.fixture
    def blob_client(self, mock_connection_string):
        """Create BlobStorageClient instance with mock connection string."""
        return BlobStorageClient(connection_string=mock_connection_string)

    @pytest.fixture
    def mock_blob_service_client(self):
        """Mock Azure BlobServiceClient."""
        return Mock()

    @pytest.fixture
    def mock_container_client(self):
        """Mock Azure container client."""
        return Mock()

    @pytest.fixture
    def mock_blob_client_instance(self):
        """Mock Azure blob client instance."""
        mock_blob = Mock()
        mock_blob.url = "https://mockaccount.blob.core.windows.net/documents/test-document.pdf"
        return mock_blob

    def test_blob_storage_client_initialization(self, mock_connection_string):
        """Test BlobStorageClient initialization."""
        # Act
        client = BlobStorageClient(connection_string=mock_connection_string, container_name="test-container")
        
        # Assert
        assert client.connection_string == mock_connection_string
        assert client.container_name == "test-container"

    def test_blob_storage_client_initialization_no_connection_string(self):
        """Test BlobStorageClient initialization without connection string raises error."""
        # Act & Assert
        with pytest.raises(ValueError, match="Azure Storage connection string not provided"):
            BlobStorageClient(connection_string=None)

    def test_upload_file_success(self, blob_client, mock_blob_service_client, 
                                mock_container_client, mock_blob_client_instance):
        """Test successful file upload to blob storage."""
        # Arrange
        with patch.object(blob_client, '_get_blob_service_client', return_value=mock_blob_service_client):
            mock_blob_service_client.get_container_client.return_value = mock_container_client
            mock_container_client.get_blob_client.return_value = mock_blob_client_instance
            mock_blob_client_instance.upload_blob.return_value = None
            
            with tempfile.NamedTemporaryFile(suffix='.pdf', delete=False) as temp_file:
                temp_file.write(b'test file content')
                temp_file_path = temp_file.name

            try:
                # Act
                result = blob_client.upload_file(temp_file_path, 'test-document.pdf')
                
                # Assert
                assert isinstance(result, BlobStorageResult)
                assert result.success is True
                assert result.blob_url == mock_blob_client_instance.url
                assert result.blob_name == 'test-document.pdf'
                assert result.error_message is None
                
                # Verify Azure SDK was called correctly
                mock_blob_service_client.get_container_client.assert_called_once_with("documents")
                mock_container_client.get_blob_client.assert_called_once_with('test-document.pdf')
                mock_blob_client_instance.upload_blob.assert_called_once()
                
            finally:
                if os.path.exists(temp_file_path):
                    os.unlink(temp_file_path)

    def test_upload_file_with_project_prefix(self, blob_client, mock_blob_service_client,
                                            mock_container_client, mock_blob_client_instance):
        """Test file upload with project-specific prefix."""
        # Arrange
        project_id = uuid4()
        expected_blob_name = f"{project_id}/document.docx"
        mock_blob_client_instance.url = f"https://mockaccount.blob.core.windows.net/documents/{expected_blob_name}"
        
        with patch.object(blob_client, '_get_blob_service_client', return_value=mock_blob_service_client):
            mock_blob_service_client.get_container_client.return_value = mock_container_client
            mock_container_client.get_blob_client.return_value = mock_blob_client_instance
            mock_blob_client_instance.upload_blob.return_value = None
            
            with tempfile.NamedTemporaryFile(suffix='.docx', delete=False) as temp_file:
                temp_file.write(b'test docx content')
                temp_file_path = temp_file.name

            try:
                # Act
                result = blob_client.upload_file(temp_file_path, 'document.docx', project_id=project_id)
                
                # Assert
                assert result.success is True
                assert str(project_id) in result.blob_name
                assert result.blob_name.endswith('document.docx')
                assert result.blob_name == expected_blob_name
                
                # Verify correct blob name was used
                mock_container_client.get_blob_client.assert_called_once_with(expected_blob_name)
                
            finally:
                if os.path.exists(temp_file_path):
                    os.unlink(temp_file_path)

    def test_upload_file_handles_missing_file(self, blob_client):
        """Test that upload handles missing files gracefully."""
        # Arrange
        nonexistent_path = '/nonexistent/file.pdf'
        
        # Act
        result = blob_client.upload_file(nonexistent_path, 'test.pdf')
        
        # Assert
        assert isinstance(result, BlobStorageResult)
        assert result.success is False
        assert 'File not found' in result.error_message
        assert result.blob_url is None
        assert result.blob_name is None

    def test_upload_file_handles_azure_exception(self, blob_client, mock_blob_service_client,
                                                mock_container_client, mock_blob_client_instance):
        """Test that upload handles Azure exceptions gracefully."""
        # Arrange
        with patch.object(blob_client, '_get_blob_service_client', return_value=mock_blob_service_client):
            mock_blob_service_client.get_container_client.return_value = mock_container_client
            mock_container_client.get_blob_client.return_value = mock_blob_client_instance
            mock_blob_client_instance.upload_blob.side_effect = Exception("Azure error")
            
            with tempfile.NamedTemporaryFile(suffix='.pdf', delete=False) as temp_file:
                temp_file.write(b'test file content')
                temp_file_path = temp_file.name

            try:
                # Act
                result = blob_client.upload_file(temp_file_path, 'test-document.pdf')
                
                # Assert
                assert isinstance(result, BlobStorageResult)
                assert result.success is False
                assert "Azure error" in result.error_message
                assert result.blob_url is None
                
            finally:
                if os.path.exists(temp_file_path):
                    os.unlink(temp_file_path)

    def test_download_file_success(self, blob_client, mock_blob_service_client,
                                  mock_container_client, mock_blob_client_instance):
        """Test successful file download from blob storage."""
        # Arrange
        expected_content = b'downloaded file content'
        mock_download_stream = Mock()
        mock_download_stream.readall.return_value = expected_content
        mock_blob_client_instance.download_blob.return_value = mock_download_stream
        
        with patch.object(blob_client, '_get_blob_service_client', return_value=mock_blob_service_client):
            mock_blob_service_client.get_container_client.return_value = mock_container_client
            mock_container_client.get_blob_client.return_value = mock_blob_client_instance
            
            with tempfile.NamedTemporaryFile(delete=False) as temp_file:
                download_path = temp_file.name

            try:
                # Act
                result = blob_client.download_file('test-blob.pdf', download_path)
                
                # Assert
                assert isinstance(result, BlobStorageResult)
                assert result.success is True
                assert result.local_path == download_path
                assert result.blob_name == 'test-blob.pdf'
                assert result.error_message is None
                
                # Verify file was written correctly
                with open(download_path, 'rb') as f:
                    content = f.read()
                    assert content == expected_content
                
                # Verify Azure SDK was called correctly
                mock_container_client.get_blob_client.assert_called_once_with('test-blob.pdf')
                mock_blob_client_instance.download_blob.assert_called_once()
                
            finally:
                if os.path.exists(download_path):
                    os.unlink(download_path)

    def test_delete_file_success(self, blob_client, mock_blob_service_client,
                                mock_container_client, mock_blob_client_instance):
        """Test successful file deletion from blob storage."""
        # Arrange
        with patch.object(blob_client, '_get_blob_service_client', return_value=mock_blob_service_client):
            mock_blob_service_client.get_container_client.return_value = mock_container_client
            mock_container_client.get_blob_client.return_value = mock_blob_client_instance
            mock_blob_client_instance.delete_blob.return_value = None

            # Act
            result = blob_client.delete_file('test-blob.pdf')
            
            # Assert
            assert isinstance(result, BlobStorageResult)
            assert result.success is True
            assert result.blob_name == 'test-blob.pdf'
            assert result.error_message is None
            
            # Verify Azure SDK was called correctly
            mock_container_client.get_blob_client.assert_called_once_with('test-blob.pdf')
            mock_blob_client_instance.delete_blob.assert_called_once()

    def test_list_files_success(self, blob_client, mock_blob_service_client, mock_container_client):
        """Test successful file listing from blob storage."""
        # Arrange
        with patch.object(blob_client, '_get_blob_service_client', return_value=mock_blob_service_client):
            mock_blob_service_client.get_container_client.return_value = mock_container_client
            
            # Mock blob list
            mock_blob1 = Mock()
            mock_blob1.name = 'project1/document1.pdf'
            mock_blob2 = Mock()
            mock_blob2.name = 'project1/document2.docx'
            mock_container_client.list_blobs.return_value = [mock_blob1, mock_blob2]

            project_id = uuid4()
            
            # Act
            result = blob_client.list_files(project_id=project_id)
            
            # Assert
            assert isinstance(result, BlobStorageResult)
            assert result.success is True
            assert len(result.file_list) == 2
            assert 'project1/document1.pdf' in result.file_list
            assert 'project1/document2.docx' in result.file_list
            
            # Verify Azure SDK was called correctly
            mock_container_client.list_blobs.assert_called_once_with(name_starts_with=f"{project_id}/")

    def test_generate_blob_name_with_project(self, blob_client):
        """Test blob name generation with project ID."""
        # Arrange
        project_id = uuid4()
        
        # Act
        blob_name = blob_client._generate_blob_name('document.pdf', project_id=project_id)
        
        # Assert
        assert str(project_id) in blob_name
        assert blob_name.endswith('document.pdf')
        assert '/' in blob_name  # Should have project prefix

    def test_generate_blob_name_without_project(self, blob_client):
        """Test blob name generation without project ID."""
        # Act
        blob_name = blob_client._generate_blob_name('document.pdf')
        
        # Assert
        assert blob_name == 'document.pdf'

    def test_cleanup_project_files_success(self, blob_client, mock_blob_service_client, mock_container_client):
        """Test successful cleanup of project files."""
        # Arrange
        project_id = uuid4()
        
        with patch.object(blob_client, '_get_blob_service_client', return_value=mock_blob_service_client):
            mock_blob_service_client.get_container_client.return_value = mock_container_client
            
            # Mock blob list
            mock_blob1 = Mock()
            mock_blob1.name = f'{project_id}/document1.pdf'
            mock_blob2 = Mock()
            mock_blob2.name = f'{project_id}/document2.docx'
            mock_container_client.list_blobs.return_value = [mock_blob1, mock_blob2]
            
            # Mock successful deletion
            with patch.object(blob_client, 'delete_file') as mock_delete:
                mock_delete.return_value = BlobStorageResult(success=True)

                # Act
                result = blob_client.cleanup_project_files(project_id)
                
                # Assert
                assert isinstance(result, BlobStorageResult)
                assert result.success is True
                assert result.file_list is not None
                assert "Deleted 2 files" in result.file_list[0]
                
                # Verify delete was called for each file
                assert mock_delete.call_count == 2


class TestBlobStorageResult:
    """Test cases for BlobStorageResult dataclass."""

    def test_blob_storage_result_creation(self):
        """Test creation of BlobStorageResult."""
        # Act
        result = BlobStorageResult(
            success=True,
            blob_name="test.pdf",
            blob_url="https://example.com/test.pdf",
            local_path="/tmp/test.pdf"
        )
        
        # Assert
        assert result.success is True
        assert result.blob_name == "test.pdf"
        assert result.blob_url == "https://example.com/test.pdf"
        assert result.local_path == "/tmp/test.pdf"
        assert result.file_list is None
        assert result.error_message is None

    def test_blob_storage_result_defaults(self):
        """Test BlobStorageResult with default values."""
        # Act
        result = BlobStorageResult()
        
        # Assert
        assert result.success is False
        assert result.blob_name is None
        assert result.blob_url is None
        assert result.local_path is None
        assert result.file_list is None
        assert result.error_message is None

    def test_blob_storage_result_error_case(self):
        """Test BlobStorageResult for error scenarios."""
        # Act
        result = BlobStorageResult(
            success=False,
            error_message="Upload failed: Connection timeout"
        )
        
        # Assert
        assert result.success is False
        assert result.error_message == "Upload failed: Connection timeout"
        assert result.blob_name is None
        assert result.blob_url is None