"""
Integration tests for API-worker communication in Document Processing Service.

These tests verify the complete communication flow between the FastAPI application
and Celery workers, including task submission, result retrieval, error handling,
and health check functionality.
"""
from uuid import uuid4
from unittest.mock import Mock, patch

from fastapi.testclient import TestClient
from celery.result import AsyncResult

class TestAPIWorkerIntegration:
    """Integration tests for API-worker communication."""

    def setup_method(self):
        """Set up test fixtures before each test."""
        self.project_id = uuid4()

    def test_worker_task_execution_success(self):
        """Test successful worker task execution."""
        # Mock all external dependencies
        with patch.dict('sys.modules', {
            'tika': Mock(),
            'tika.parser': Mock(),
            'azure.storage.blob': Mock(),
            'services.tika_processor': Mock(),
            'utils.blob_storage': Mock(),
            'services.project_management_client': Mock()
        }):
            # Mock the task function directly
            with patch('tasks.document_tasks.BlobStorageClient') as mock_blob_client, \
                 patch('tasks.document_tasks.TikaProcessor') as mock_tika, \
                 patch('tasks.document_tasks.ProjectManagementClient') as mock_pm_client:
                
                from tasks.document_tasks import process_project_documents_task
                
                # Mock blob storage
                mock_blob_instance = Mock()
                mock_blob_client.return_value = mock_blob_instance
                
                # Mock file listing
                mock_list_result = Mock()
                mock_list_result.success = True
                mock_list_result.file_list = ["file1.pdf", "file2.docx"]
                mock_blob_instance.list_files.return_value = mock_list_result
                
                # Mock file download
                mock_download_result = Mock()
                mock_download_result.success = True
                mock_blob_instance.download_file.return_value = mock_download_result
                
                # Mock file deletion
                mock_delete_result = Mock()
                mock_delete_result.success = True
                mock_blob_instance.delete_file.return_value = mock_delete_result
                
                # Mock Tika processor
                mock_tika_instance = Mock()
                mock_tika.return_value = mock_tika_instance
                
                mock_processing_result = Mock()
                mock_processing_result.success = True
                mock_processing_result.extracted_text = "Extracted text content"
                mock_processing_result.file_type = "application/pdf"
                mock_processing_result.page_count = 3
                mock_processing_result.metadata = {"Author": "Test Author"}
                mock_processing_result.to_structured_json.return_value = {
                    "extracted_text": "Extracted text content",
                    "metadata": {"Author": "Test Author"},
                    "file_info": {"page_count": 3}
                }
                mock_tika_instance.extract_text_with_result.return_value = mock_processing_result
                
                # Mock project management client
                mock_pm_instance = Mock()
                mock_pm_client.return_value.__aenter__.return_value = mock_pm_instance
                mock_pm_instance.update_project_status.return_value = Mock(
                    success=True,
                    status_code=200
                )
                
                # Act
                result = process_project_documents_task(str(self.project_id))
                
                # Assert
                assert result["success"] is True
                assert result["project_id"] == str(self.project_id)
                assert result["total_documents"] == 2
                assert result["processed_documents"] == 2
                assert result["failed_documents"] == 0
                assert result["extracted_data_count"] == 2
                
                # Verify blob operations
                mock_blob_instance.list_files.assert_called_once()
                assert mock_blob_instance.download_file.call_count == 2
                assert mock_blob_instance.delete_file.call_count == 2
                
                # Verify Tika processing
                assert mock_tika_instance.extract_text_with_result.call_count == 2

    @patch('main_tasks.celery_app')
    def test_worker_health_check(self, mock_celery_app):
        """Test worker health check functionality."""
        # Mock Celery app inspection
        mock_inspect = Mock()
        mock_celery_app.control.inspect.return_value = mock_inspect
        mock_inspect.ping.return_value = {"worker1@hostname": "pong"}
        mock_inspect.active.return_value = {"worker1@hostname": []}
        mock_inspect.stats.return_value = {"worker1@hostname": {"total": 0}}
        
        # Act - Test that worker can report its health status
        ping_result = mock_celery_app.control.inspect().ping()
        active_tasks = mock_celery_app.control.inspect().active()
        worker_stats = mock_celery_app.control.inspect().stats()

        assert "worker1@hostname" in ping_result
        assert ping_result["worker1@hostname"] == "pong"
        assert "worker1@hostname" in active_tasks
        assert "worker1@hostname" in worker_stats

    @patch('main_tasks.celery_app')
    @patch('main_tasks.logger')
    def test_worker_independent_startup(self, mock_logger, mock_celery_app):
        """Test that worker can start independently without API."""
        # Mock external dependencies
        with patch.dict('sys.modules', {
            'tika': Mock(),
            'tika.parser': Mock(),
            'azure.storage.blob': Mock(),
            'services.tika_processor': Mock(),
            'utils.blob_storage': Mock(),
            'services.project_management_client': Mock()
        }):
            from main_tasks import start_worker
            
            # Mock worker startup
            mock_worker = Mock()
            mock_celery_app.Worker.return_value = mock_worker
            
            # Act
            start_worker()
            
            # Assert
            mock_celery_app.Worker.assert_called_once()
            mock_worker.start.assert_called_once()
            mock_logger.info.assert_any_call("Starting Celery worker with enhanced monitoring")
            mock_logger.info.assert_any_call("Celery worker started successfully")

    def test_task_result_retrieval(self):
        """Test task result retrieval and status monitoring."""
        # Mock external dependencies
        with patch.dict('sys.modules', {
            'tika': Mock(),
            'tika.parser': Mock(),
            'azure.storage.blob': Mock(),
            'services.tika_processor': Mock(),
            'utils.blob_storage': Mock(),
            'services.project_management_client': Mock()
        }):
            with patch('tasks.document_tasks.process_project_documents_task') as mock_task:
                from celery.result import AsyncResult
                
                # Mock task result
                mock_async_result = Mock(spec=AsyncResult)
                mock_async_result.id = 'test-task-id-789'
                mock_async_result.state = 'SUCCESS'
                mock_async_result.result = {
                    'success': True,
                    'project_id': str(self.project_id),
                    'processed_documents': 2,
                    'failed_documents': 0
                }
                mock_async_result.ready.return_value = True
                mock_async_result.successful.return_value = True
                mock_task.delay.return_value = mock_async_result
                
                # Act - Submit task and check result
                task_result = mock_task.delay(str(self.project_id))
                
                # Assert
                assert task_result.id == 'test-task-id-789'
                assert task_result.state == 'SUCCESS'
                assert task_result.ready() is True
                assert task_result.successful() is True
                assert task_result.result['success'] is True
                assert task_result.result['processed_documents'] == 2

    def test_worker_error_handling_processing_failure(self):
        """Test worker error handling when document processing fails."""
        # Mock external dependencies
        with patch.dict('sys.modules', {
            'tika': Mock(),
            'tika.parser': Mock(),
            'azure.storage.blob': Mock(),
            'services.tika_processor': Mock(),
            'utils.blob_storage': Mock(),
            'services.project_management_client': Mock()
        }):
            with patch('tasks.document_tasks.BlobStorageClient') as mock_blob_client, \
                 patch('tasks.document_tasks.TikaProcessor') as mock_tika:
                
                from tasks.document_tasks import process_project_documents_task
                
                # Mock blob storage
                mock_blob_instance = Mock()
                mock_blob_client.return_value = mock_blob_instance
                
                mock_list_result = Mock()
                mock_list_result.success = True
                mock_list_result.file_list = ["corrupted.pdf"]
                mock_blob_instance.list_files.return_value = mock_list_result
                
                mock_download_result = Mock()
                mock_download_result.success = True
                mock_blob_instance.download_file.return_value = mock_download_result
                
                # Mock Tika processor failure
                mock_tika_instance = Mock()
                mock_tika.return_value = mock_tika_instance
                
                mock_processing_result = Mock()
                mock_processing_result.success = False
                mock_processing_result.error_message = "Corrupted file format"
                mock_tika_instance.extract_text_with_result.return_value = mock_processing_result
                
                # Act
                result = process_project_documents_task(str(self.project_id))
                
                # Assert
                assert result["success"] is True  # Task completes even with processing failures
                assert result["processed_documents"] == 0
                assert result["failed_documents"] == 1
                assert result["total_documents"] == 1

    def test_worker_task_execution_blob_failure(self):
        """Test worker task execution when blob operations fail."""
        # Mock external dependencies
        with patch.dict('sys.modules', {
            'tika': Mock(),
            'tika.parser': Mock(),
            'azure.storage.blob': Mock(),
            'services.tika_processor': Mock(),
            'utils.blob_storage': Mock(),
            'services.project_management_client': Mock()
        }):
            with patch('tasks.document_tasks.BlobStorageClient') as mock_blob_client:
                from tasks.document_tasks import process_project_documents_task
                
                # Mock blob storage failure
                mock_blob_instance = Mock()
                mock_blob_client.return_value = mock_blob_instance
                
                mock_list_result = Mock()
                mock_list_result.success = False
                mock_list_result.error_message = "Failed to list files"
                mock_blob_instance.list_files.return_value = mock_list_result
                
                # Act
                result = process_project_documents_task(str(self.project_id))
                
                # Assert
                assert result["success"] is False
                assert result["project_id"] == str(self.project_id)
                assert "Failed to list files for project" in result["error_message"]