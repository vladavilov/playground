"""
Consolidated tests for Azure Blob Storage integration.
"""

import os
import tempfile
from unittest.mock import Mock, patch, mock_open, MagicMock
from uuid import uuid4

import pytest
from azure.core.exceptions import ResourceNotFoundError, HttpResponseError

from configuration.blob_storage_config import BlobStorageSettings
from utils.blob_storage import BlobStorageClient, BlobStorageResult, get_blob_storage_client


class TestBlobStorageClient:
    """Test cases for Azure Blob Storage client."""

    @pytest.fixture
    def mock_connection_string(self):
        """Mock Azure Storage connection string."""
        return "DefaultEndpointsProtocol=https;AccountName=mockaccount;AccountKey=mockkey;EndpointSuffix=core.windows.net"

    @pytest.fixture
    def mock_settings(self, mock_connection_string):
        """Mock BlobStorageSettings."""
        settings = MagicMock(spec=BlobStorageSettings)
        settings.AZURE_STORAGE_CONNECTION_STRING = mock_connection_string
        settings.AZURE_STORAGE_CONTAINER_NAME = "documents"
        return settings

    @pytest.fixture
    def blob_client(self, mock_settings):
        """Create BlobStorageClient instance with mock settings."""
        return BlobStorageClient(settings=mock_settings)

    @pytest.fixture
    def mock_azure_clients(self):
        """Mock Azure SDK clients."""
        mock_service_client = Mock()
        mock_container_client = Mock()
        mock_blob_client = Mock()
        mock_blob_client.url = "https://mockaccount.blob.core.windows.net/documents/test-document.pdf"
        
        mock_service_client.get_container_client.return_value = mock_container_client
        mock_container_client.get_blob_client.return_value = mock_blob_client
        
        return {
            'service': mock_service_client,
            'container': mock_container_client,
            'blob': mock_blob_client
        }

    def test_initialization_with_settings(self, mock_settings):
        """Test BlobStorageClient initialization with settings."""
        client = BlobStorageClient(settings=mock_settings)
        
        assert client.settings == mock_settings
        assert client.connection_string == mock_settings.AZURE_STORAGE_CONNECTION_STRING
        assert client.container_name == mock_settings.AZURE_STORAGE_CONTAINER_NAME

    def test_initialization_without_settings_uses_default(self):
        """Test that client uses default settings when none provided."""
        with patch('utils.blob_storage.get_blob_storage_settings') as mock_get_settings:
            mock_settings = MagicMock(spec=BlobStorageSettings)
            mock_settings.AZURE_STORAGE_CONNECTION_STRING = "default_connection"
            mock_settings.AZURE_STORAGE_CONTAINER_NAME = "default-container"
            mock_get_settings.return_value = mock_settings
            
            client = BlobStorageClient()
            
            mock_get_settings.assert_called_once()
            assert client.settings == mock_settings

    def test_initialization_validates_connection_string(self):
        """Test that initialization validates connection string."""
        mock_settings = MagicMock(spec=BlobStorageSettings)
        mock_settings.AZURE_STORAGE_CONNECTION_STRING = None
        mock_settings.AZURE_STORAGE_CONTAINER_NAME = "test-container"
        
        with pytest.raises(ValueError, match="Azure Storage connection string not provided"):
            BlobStorageClient(settings=mock_settings)

    @patch('utils.blob_storage.BlobServiceClient')
    def test_get_blob_service_client_lazy_initialization(self, mock_blob_service_client, blob_client):
        """Test lazy initialization of blob service client."""
        mock_service_client = MagicMock()
        mock_blob_service_client.from_connection_string.return_value = mock_service_client
        
        # First call should create the client
        result1 = blob_client._get_blob_service_client()
        mock_blob_service_client.from_connection_string.assert_called_once()
        assert result1 == mock_service_client
        
        # Second call should return the same client (cached)
        result2 = blob_client._get_blob_service_client()
        assert result2 == mock_service_client
        assert mock_blob_service_client.from_connection_string.call_count == 1

    def test_upload_file_success(self, blob_client, mock_azure_clients):
        """Test successful file upload to blob storage."""
        with patch.object(blob_client, '_get_blob_service_client', return_value=mock_azure_clients['service']):
            # Mock container exists check for _ensure_container_exists
            mock_azure_clients['container'].exists.return_value = True
            mock_azure_clients['blob'].upload_blob.return_value = None
            
            with tempfile.NamedTemporaryFile(suffix='.pdf', delete=False) as temp_file:
                temp_file.write(b'test file content')
                temp_file_path = temp_file.name

            try:
                result = blob_client.upload_file(temp_file_path, 'test-document.pdf')
                
                assert isinstance(result, BlobStorageResult)
                assert result.success is True
                assert result.blob_url == mock_azure_clients['blob'].url
                assert result.blob_name == 'test-document.pdf'
                assert result.error_message is None
                
                # Verify Azure SDK was called correctly - now called twice due to container existence check
                assert mock_azure_clients['service'].get_container_client.call_count == 2
                mock_azure_clients['container'].get_blob_client.assert_called_once_with('test-document.pdf')
                mock_azure_clients['blob'].upload_blob.assert_called_once()
                
            finally:
                if os.path.exists(temp_file_path):
                    os.unlink(temp_file_path)

    def test_upload_file_with_project_prefix(self, blob_client, mock_azure_clients):
        """Test file upload with project-specific prefix."""
        project_id = uuid4()
        expected_blob_name = f"{project_id}/document.docx"
        mock_azure_clients['blob'].url = f"https://mockaccount.blob.core.windows.net/documents/{expected_blob_name}"
        
        with patch.object(blob_client, '_get_blob_service_client', return_value=mock_azure_clients['service']):
            mock_azure_clients['blob'].upload_blob.return_value = None
            
            with tempfile.NamedTemporaryFile(suffix='.docx', delete=False) as temp_file:
                temp_file.write(b'test docx content')
                temp_file_path = temp_file.name

            try:
                result = blob_client.upload_file(temp_file_path, 'document.docx', project_id=project_id)
                
                assert result.success is True
                assert str(project_id) in result.blob_name
                assert result.blob_name.endswith('document.docx')
                assert result.blob_name == expected_blob_name
                
                mock_azure_clients['container'].get_blob_client.assert_called_once_with(expected_blob_name)
                
            finally:
                if os.path.exists(temp_file_path):
                    os.unlink(temp_file_path)

    def test_upload_file_handles_missing_file(self, blob_client):
        """Test that upload handles missing files gracefully."""
        nonexistent_path = '/nonexistent/file.pdf'
        
        result = blob_client.upload_file(nonexistent_path, 'test.pdf')
        
        assert isinstance(result, BlobStorageResult)
        assert result.success is False
        assert 'File not found' in result.error_message
        assert result.blob_url is None
        assert result.blob_name is None

    def test_upload_file_handles_azure_exception(self, blob_client, mock_azure_clients):
        """Test that upload handles Azure exceptions gracefully."""
        with patch.object(blob_client, '_get_blob_service_client', return_value=mock_azure_clients['service']):
            mock_azure_clients['blob'].upload_blob.side_effect = Exception("Azure error")
            
            with tempfile.NamedTemporaryFile(suffix='.pdf', delete=False) as temp_file:
                temp_file.write(b'test file content')
                temp_file_path = temp_file.name

            try:
                result = blob_client.upload_file(temp_file_path, 'test-document.pdf')
                
                assert isinstance(result, BlobStorageResult)
                assert result.success is False
                assert "Azure error" in result.error_message
                assert result.blob_url is None
                
            finally:
                if os.path.exists(temp_file_path):
                    os.unlink(temp_file_path)

    def test_download_file_success(self, blob_client, mock_azure_clients):
        """Test successful file download from blob storage."""
        expected_content = b'downloaded file content'
        mock_download_stream = Mock()
        mock_download_stream.readall.return_value = expected_content
        mock_azure_clients['blob'].download_blob.return_value = mock_download_stream
        
        with patch.object(blob_client, '_get_blob_service_client', return_value=mock_azure_clients['service']):
            with tempfile.NamedTemporaryFile(delete=False) as temp_file:
                download_path = temp_file.name

            try:
                result = blob_client.download_file('test-blob.pdf', download_path)
                
                assert isinstance(result, BlobStorageResult)
                assert result.success is True
                assert result.local_path == download_path
                assert result.blob_name == 'test-blob.pdf'
                assert result.error_message is None
                
                # Verify file was written correctly
                with open(download_path, 'rb') as f:
                    content = f.read()
                    assert content == expected_content
                
                mock_azure_clients['container'].get_blob_client.assert_called_once_with('test-blob.pdf')
                mock_azure_clients['blob'].download_blob.assert_called_once()
                
            finally:
                if os.path.exists(download_path):
                    os.unlink(download_path)

    def test_delete_file_success(self, blob_client, mock_azure_clients):
        """Test successful file deletion from blob storage."""
        with patch.object(blob_client, '_get_blob_service_client', return_value=mock_azure_clients['service']):
            mock_azure_clients['blob'].delete_blob.return_value = None

            result = blob_client.delete_file('test-blob.pdf')
            
            assert isinstance(result, BlobStorageResult)
            assert result.success is True
            assert result.blob_name == 'test-blob.pdf'
            assert result.error_message is None
            
            mock_azure_clients['container'].get_blob_client.assert_called_once_with('test-blob.pdf')
            mock_azure_clients['blob'].delete_blob.assert_called_once()

    def test_list_files_success(self, blob_client, mock_azure_clients):
        """Test successful file listing from blob storage."""
        with patch.object(blob_client, '_get_blob_service_client', return_value=mock_azure_clients['service']):
            # Mock blob list
            mock_blob1 = Mock()
            mock_blob1.name = 'project1/document1.pdf'
            mock_blob2 = Mock()
            mock_blob2.name = 'project1/document2.docx'
            mock_azure_clients['container'].list_blobs.return_value = [mock_blob1, mock_blob2]

            project_id = uuid4()
            
            result = blob_client.list_files(project_id=project_id)
            
            assert isinstance(result, BlobStorageResult)
            assert result.success is True
            assert len(result.file_list) == 2
            assert 'project1/document1.pdf' in result.file_list
            assert 'project1/document2.docx' in result.file_list
            
            mock_azure_clients['container'].list_blobs.assert_called_once_with(name_starts_with=f"{project_id}/")

    def test_generate_blob_name_with_project(self, blob_client):
        """Test blob name generation with project ID."""
        project_id = uuid4()
        
        blob_name = blob_client._generate_blob_name('document.pdf', project_id=project_id)
        
        assert str(project_id) in blob_name
        assert blob_name.endswith('document.pdf')
        assert '/' in blob_name  # Should have project prefix

    def test_generate_blob_name_without_project(self, blob_client):
        """Test blob name generation without project ID."""
        blob_name = blob_client._generate_blob_name('document.pdf')
        
        assert blob_name == 'document.pdf'

    def test_cleanup_project_files_success(self, blob_client, mock_azure_clients):
        """Test successful cleanup of project files."""
        project_id = uuid4()
        
        with patch.object(blob_client, '_get_blob_service_client', return_value=mock_azure_clients['service']):
            # Mock blob list
            mock_blob1 = Mock()
            mock_blob1.name = f'{project_id}/document1.pdf'
            mock_blob2 = Mock()
            mock_blob2.name = f'{project_id}/document2.docx'
            mock_azure_clients['container'].list_blobs.return_value = [mock_blob1, mock_blob2]
            
            # Mock successful deletion
            with patch.object(blob_client, 'delete_file') as mock_delete:
                mock_delete.return_value = BlobStorageResult(success=True)

                result = blob_client.cleanup_project_files(project_id)
                
                assert isinstance(result, BlobStorageResult)
                assert result.success is True
                assert result.file_list is not None
                assert "Deleted 2 files" in result.file_list[0]
                
                assert mock_delete.call_count == 2


class TestGetBlobStorageClient:
    """Test cases for get_blob_storage_client function."""

    def test_returns_blob_storage_client_instance(self):
        """Test that function returns BlobStorageClient instance."""
        client = get_blob_storage_client()
        assert isinstance(client, BlobStorageClient)

    def test_caching_behavior(self):
        """Test that function returns the same instance on multiple calls (caching)."""
        client1 = get_blob_storage_client()
        client2 = get_blob_storage_client()
        assert client1 is client2

    @patch('utils.blob_storage.BlobStorageClient')
    def test_client_instantiation_with_default_settings(self, mock_client_class):
        """Test that BlobStorageClient is instantiated with default settings."""
        mock_instance = MagicMock()
        mock_client_class.return_value = mock_instance
        
        # Clear the cache first
        get_blob_storage_client.cache_clear()
        
        result = get_blob_storage_client()
        
        # Should be called without arguments (uses default settings)
        mock_client_class.assert_called_once_with()
        assert result == mock_instance


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

    def test_blob_storage_result_error_case(self):
        """Test BlobStorageResult for error scenarios."""
        result = BlobStorageResult(
            success=False,
            error_message="Upload failed: Connection timeout"
        )
        
        assert result.success is False
        assert result.error_message == "Upload failed: Connection timeout"
        assert result.blob_name is None
        assert result.blob_url is None


class TestBlobStorageClientContainerCreation:
    """Test cases for blob storage container creation functionality."""

    @pytest.fixture
    def mock_connection_string(self):
        """Mock Azure Storage connection string."""
        return "DefaultEndpointsProtocol=https;AccountName=mockaccount;AccountKey=mockkey;EndpointSuffix=core.windows.net"

    @pytest.fixture
    def mock_settings(self, mock_connection_string):
        """Mock BlobStorageSettings."""
        settings = MagicMock(spec=BlobStorageSettings)
        settings.AZURE_STORAGE_CONNECTION_STRING = mock_connection_string
        settings.AZURE_STORAGE_CONTAINER_NAME = "documents"
        return settings

    @pytest.fixture
    def blob_client(self, mock_settings):
        """Create BlobStorageClient instance with mock settings."""
        return BlobStorageClient(settings=mock_settings)

    @pytest.fixture
    def mock_azure_clients(self):
        """Mock Azure SDK clients."""
        mock_service_client = Mock()
        mock_container_client = Mock()
        mock_blob_client = Mock()
        mock_blob_client.url = "https://mockaccount.blob.core.windows.net/documents/test-document.pdf"
        
        mock_service_client.get_container_client.return_value = mock_container_client
        mock_container_client.get_blob_client.return_value = mock_blob_client
        
        return {
            'service': mock_service_client,
            'container': mock_container_client,
            'blob': mock_blob_client
        }

    def test_ensure_container_exists_creates_new_container(self, blob_client, mock_azure_clients):
        """Test that _ensure_container_exists creates container when it doesn't exist."""
        with patch('utils.blob_storage.BlobServiceClient.from_connection_string', return_value=mock_azure_clients['service']):
            # Mock container doesn't exist initially
            mock_azure_clients['service'].get_container_client.return_value.exists.return_value = False
            mock_azure_clients['service'].get_container_client.return_value.create_container = Mock()
            
            # Call the method that should be created
            result = blob_client._ensure_container_exists()
            
            # Verify container creation was attempted
            mock_azure_clients['service'].get_container_client.return_value.create_container.assert_called_once()
            assert result is True

    def test_ensure_container_exists_handles_existing_container(self, blob_client, mock_azure_clients):
        """Test that _ensure_container_exists handles existing container gracefully."""
        with patch('utils.blob_storage.BlobServiceClient.from_connection_string', return_value=mock_azure_clients['service']):
            # Mock container already exists
            mock_azure_clients['service'].get_container_client.return_value.exists.return_value = True
            mock_azure_clients['service'].get_container_client.return_value.create_container = Mock()
            
            # Call the method
            result = blob_client._ensure_container_exists()
            
            # Verify container creation was not attempted since it exists
            mock_azure_clients['service'].get_container_client.return_value.create_container.assert_not_called()
            assert result is True

    def test_ensure_container_exists_handles_creation_failure(self, blob_client, mock_azure_clients):
        """Test that _ensure_container_exists handles container creation failures."""
        with patch('utils.blob_storage.BlobServiceClient.from_connection_string', return_value=mock_azure_clients['service']):
            # Mock container doesn't exist and creation fails
            mock_azure_clients['service'].get_container_client.return_value.exists.return_value = False
            mock_azure_clients['service'].get_container_client.return_value.create_container.side_effect = HttpResponseError("Creation failed")
            
            # Call the method
            result = blob_client._ensure_container_exists()
            
            # Verify creation was attempted but failed
            mock_azure_clients['service'].get_container_client.return_value.create_container.assert_called_once()
            assert result is False

    def test_upload_file_ensures_container_exists(self, blob_client, mock_azure_clients):
        """Test that upload_file ensures container exists before uploading."""
        with patch('utils.blob_storage.BlobServiceClient.from_connection_string', return_value=mock_azure_clients['service']):
            # Mock file exists and container needs to be created
            with patch('os.path.exists', return_value=True):
                with patch('builtins.open', mock_open(read_data=b'test content')):
                    # Mock container creation
                    mock_azure_clients['service'].get_container_client.return_value.exists.return_value = False
                    mock_azure_clients['service'].get_container_client.return_value.create_container = Mock()
                    
                    # Mock _ensure_container_exists method
                    with patch.object(blob_client, '_ensure_container_exists', return_value=True) as mock_ensure:
                        result = blob_client.upload_file('/path/to/test.pdf', 'test.pdf')
                        
                        # Verify container existence was checked
                        mock_ensure.assert_called_once()
                        assert result.success is True

    def test_upload_file_fails_when_container_creation_fails(self, blob_client, mock_azure_clients):
        """Test that upload_file fails when container cannot be created."""
        with patch('utils.blob_storage.BlobServiceClient.from_connection_string', return_value=mock_azure_clients['service']):
            # Mock file exists but container creation fails
            with patch('os.path.exists', return_value=True):
                # Mock _ensure_container_exists to return False (creation failed)
                with patch.object(blob_client, '_ensure_container_exists', return_value=False):
                    result = blob_client.upload_file('/path/to/test.pdf', 'test.pdf')
                    
                    # Verify upload fails when container creation fails
                    assert result.success is False
                    assert "Container does not exist and could not be created" in result.error_message

    def test_download_file_ensures_container_exists(self, blob_client, mock_azure_clients):
        """Test that download_file ensures container exists before downloading."""
        with patch('utils.blob_storage.BlobServiceClient.from_connection_string', return_value=mock_azure_clients['service']):
            # Mock download stream
            mock_download_stream = Mock()
            mock_download_stream.readall.return_value = b'test content'
            mock_azure_clients['blob'].download_blob.return_value = mock_download_stream
            
            # Mock _ensure_container_exists method
            with patch.object(blob_client, '_ensure_container_exists', return_value=True) as mock_ensure:
                with patch('builtins.open', mock_open()) as mock_file:
                    result = blob_client.download_file('test.pdf', '/path/to/download.pdf')
                    
                    # Verify container existence was checked
                    mock_ensure.assert_called_once()
                    assert result.success is True

    def test_list_files_ensures_container_exists(self, blob_client, mock_azure_clients):
        """Test that list_files ensures container exists before listing."""
        with patch('utils.blob_storage.BlobServiceClient.from_connection_string', return_value=mock_azure_clients['service']):
            # Mock blob listing
            mock_blob = Mock()
            mock_blob.name = 'test.pdf'
            mock_azure_clients['container'].list_blobs.return_value = [mock_blob]
            
            # Mock _ensure_container_exists method
            with patch.object(blob_client, '_ensure_container_exists', return_value=True) as mock_ensure:
                result = blob_client.list_files()
                
                # Verify container existence was checked
                mock_ensure.assert_called_once()
                assert result.success is True
                assert result.file_list == ['test.pdf']

    def test_delete_file_ensures_container_exists(self, blob_client, mock_azure_clients):
        """Test that delete_file ensures container exists before deleting."""
        with patch('utils.blob_storage.BlobServiceClient.from_connection_string', return_value=mock_azure_clients['service']):
            # Mock _ensure_container_exists method
            with patch.object(blob_client, '_ensure_container_exists', return_value=True) as mock_ensure:
                result = blob_client.delete_file('test.pdf')
                
                # Verify container existence was checked
                mock_ensure.assert_called_once()
                assert result.success is True