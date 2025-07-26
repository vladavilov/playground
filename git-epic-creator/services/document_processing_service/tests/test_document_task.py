"""
Integration tests for Document Processing Service worker functionality.

These tests verify the document processing task behavior by mocking only the
CeleryFactory and testing the actual business logic and behavior rather than
internal implementation details.
"""
import sys
from uuid import uuid4
from unittest.mock import Mock, patch

class TestDocumentProcessingIntegration:
    """Integration tests for document processing worker functionality."""

    def setup_method(self):
        """Set up test fixtures before each test."""
        self.project_id = str(uuid4())
        # Clean up any imported modules before each test
        modules_to_remove = [
            'main',
            'tasks.document_tasks',
            'celery_app'
        ]
        for module in modules_to_remove:
            if module in sys.modules:
                del sys.modules[module]

    def test_document_processing_task_success(self):
        """Test successful document processing task execution."""
        # Mock the tika module first
        mock_tika_module = Mock()
        mock_tika_parser = Mock()
        mock_tika_module.parser = mock_tika_parser
        
        with patch('utils.blob_storage.BlobStorageClient') as mock_blob_client, \
             patch('utils.celery_factory.get_celery_app') as mock_get_celery_app, \
             patch('asyncio.run') as mock_asyncio_run, \
             patch.dict('sys.modules', {'tika': mock_tika_module, 'tika.parser': mock_tika_parser}):
            
            # Mock Celery app - make task decorator return the original function
            mock_celery_app = Mock()
            mock_celery_app.task.return_value = lambda func: func
            mock_get_celery_app.return_value = mock_celery_app
            
            # Setup blob storage mock
            mock_blob_instance = Mock()
            mock_blob_client.return_value = mock_blob_instance
            
            mock_list_result = Mock()
            mock_list_result.success = True
            mock_list_result.file_list = ["test_file.pdf"]
            mock_blob_instance.list_files.return_value = mock_list_result
            
            mock_download_result = Mock()
            mock_download_result.success = True
            mock_blob_instance.download_file.return_value = mock_download_result
            
            mock_delete_result = Mock()
            mock_delete_result.success = True
            mock_blob_instance.delete_file.return_value = mock_delete_result
            
            # Setup async progress update mock
            mock_asyncio_run.return_value = {"success": True}
            
            # Import and execute the task (after mocking dependencies)
            with patch('services.tika_processor.TikaProcessor') as mock_tika:
                # Setup Tika processor mock
                mock_tika_instance = Mock()
                mock_tika.return_value = mock_tika_instance
                
                mock_processing_result = Mock()
                mock_processing_result.success = True
                mock_processing_result.extracted_text = "Sample extracted text"
                mock_processing_result.file_type = "application/pdf"
                mock_processing_result.page_count = 1
                mock_processing_result.metadata = {"title": "Test Document"}
                mock_processing_result.to_structured_json.return_value = {
                    "text": "Sample extracted text",
                    "metadata": {"title": "Test Document"}
                }
                mock_tika_instance.extract_text_with_result.return_value = mock_processing_result
                
                from tasks.document_tasks import process_project_documents_task
                
                # Call the function directly with proper arguments (including mock self)
                mock_self = Mock()
                result = process_project_documents_task(mock_self, self.project_id)
            
            # Assert behavior - focus on the result, not internal calls
            assert result["success"] is True
            assert result["project_id"] == self.project_id
            assert result["total_documents"] == 1
            assert result["processed_documents"] == 1
            assert result["failed_documents"] == 0

    def test_worker_startup_behavior(self):
        """Test that worker starts correctly using the created Celery app."""
        with patch('utils.celery_factory.get_celery_app') as mock_get_celery_app:
            # Mock Celery app and worker
            mock_celery_app = Mock()
            mock_worker = Mock()
            mock_celery_app.Worker.return_value = mock_worker
            mock_get_celery_app.return_value = mock_celery_app
            
            # Import main to get the start_worker function
            from main import start_worker
            
            # Act
            start_worker()
            
            # Assert behavior - worker should be created and started
            mock_celery_app.Worker.assert_called_once()
            mock_worker.start.assert_called_once()

    def test_document_processing_task_failure_handling(self):
        """Test that task handles failures gracefully and returns appropriate results."""
        # Mock the tika module first
        mock_tika_module = Mock()
        mock_tika_parser = Mock()
        mock_tika_module.parser = mock_tika_parser
        
        with patch('utils.blob_storage.BlobStorageClient') as mock_blob_client, \
             patch('utils.celery_factory.get_celery_app') as mock_get_celery_app, \
             patch.dict('sys.modules', {'tika': mock_tika_module, 'tika.parser': mock_tika_parser}):

            # Mock Celery app - make task decorator return the original function
            mock_celery_app = Mock()
            mock_celery_app.task.return_value = lambda func: func
            mock_get_celery_app.return_value = mock_celery_app
            
            mock_blob_instance = Mock()
            mock_blob_client.return_value = mock_blob_instance
            
            # Mock blob storage failure
            mock_list_result = Mock()
            mock_list_result.success = False
            mock_list_result.error_message = "Storage connection failed"
            mock_blob_instance.list_files.return_value = mock_list_result
            
            # Import and execute the task
            from tasks.document_tasks import process_project_documents_task
            mock_self = Mock()
            result = process_project_documents_task(mock_self, self.project_id)
            
            # Assert failure behavior
            assert result["success"] is False
            assert result["project_id"] == self.project_id
            assert "Failed to list files for project" in result["error_message"]

    def test_document_processing_partial_failure_behavior(self):
        """Test behavior when some documents fail processing but task continues."""
        # Mock the tika module first
        mock_tika_module = Mock()
        mock_tika_parser = Mock()
        mock_tika_module.parser = mock_tika_parser
        
        with patch('utils.blob_storage.BlobStorageClient') as mock_blob_client, \
             patch('utils.celery_factory.get_celery_app') as mock_get_celery_app, \
             patch('asyncio.run') as mock_asyncio_run, \
             patch.dict('sys.modules', {'tika': mock_tika_module, 'tika.parser': mock_tika_parser}):
            
            # Mock Celery app - make task decorator return the original function
            mock_celery_app = Mock()
            mock_celery_app.task.return_value = lambda func: func
            mock_get_celery_app.return_value = mock_celery_app
            
            # Setup blob storage mock
            mock_blob_instance = Mock()
            mock_blob_client.return_value = mock_blob_instance
            
            mock_list_result = Mock()
            mock_list_result.success = True
            mock_list_result.file_list = ["good_file.pdf", "corrupted_file.pdf"]
            mock_blob_instance.list_files.return_value = mock_list_result
            
            mock_download_result = Mock()
            mock_download_result.success = True
            mock_blob_instance.download_file.return_value = mock_download_result
            
            mock_delete_result = Mock()
            mock_delete_result.success = True
            mock_blob_instance.delete_file.return_value = mock_delete_result
            
            # Setup async progress update mock
            mock_asyncio_run.return_value = {"success": True}
            
            # Import and execute the task (after mocking dependencies)
            with patch('services.tika_processor.TikaProcessor') as mock_tika:
                # Setup Tika processor mock - first succeeds, second fails
                mock_tika_instance = Mock()
                mock_tika.return_value = mock_tika_instance
                
                success_result = Mock()
                success_result.success = True
                success_result.extracted_text = "Good content"
                success_result.file_type = "application/pdf"
                success_result.page_count = 1
                success_result.metadata = {}
                success_result.to_structured_json.return_value = {"text": "Good content"}
                
                failure_result = Mock()
                failure_result.success = False
                failure_result.error_message = "Corrupted file"
                
                mock_tika_instance.extract_text_with_result.side_effect = [success_result, failure_result]
                
                from tasks.document_tasks import process_project_documents_task
                mock_self = Mock()
                result = process_project_documents_task(mock_self, self.project_id)
            
            # Assert partial failure behavior
            assert result["success"] is True
            assert result["total_documents"] == 2
            assert result["processed_documents"] == 1
            assert result["failed_documents"] == 1
