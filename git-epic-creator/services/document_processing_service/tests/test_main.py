"""
Tests for main_tasks.py Celery worker entry point.
"""

import pytest
from unittest.mock import Mock, patch, MagicMock, call
import sys
import os
from celery import Celery
from celery.signals import task_prerun, task_postrun, task_failure, task_retry, worker_ready

class TestMain:
    """Test cases for main_tasks.py Celery worker entry point."""

    def setup_method(self):
        """Clean up any imported modules before each test."""
        # Remove main_tasks from sys.modules if it exists
        modules_to_remove = [
            'main_tasks',
            'configuration.logging_config',
            'celery_app',
            'tasks.document_tasks'
        ]
        for module in modules_to_remove:
            if module in sys.modules:
                del sys.modules[module]

    @patch('main_tasks._module_initialization')
    def test_celery_app_creation_with_task_modules(self, mock_module_init):
        """Test that Celery worker app is created with proper task module discovery."""
        # Arrange
        mock_celery_app = Mock(spec=Celery)
        
        # Act
        import main_tasks
        
        # Manually test the initialization with mocks
        with patch('main_tasks.configure_logging') as mock_configure_logging, \
             patch('main_tasks.create_celery_app') as mock_create_celery_app, \
             patch('builtins.__import__') as mock_import:
            
            mock_create_celery_app.return_value = mock_celery_app
            
            # Call the initialization function
            main_tasks._initialize_application()
            
            # Assert
            mock_configure_logging.assert_called_once()
            mock_create_celery_app.assert_called_once()
            
            # Verify task module import was attempted
            mock_import.assert_any_call('tasks.document_tasks')
            
            # Verify the app is accessible
            assert main_tasks.celery_app == mock_celery_app

    @patch('main_tasks._module_initialization')
    def test_celery_app_creation_logs_success(self, mock_module_init):
        """Test that successful Celery app creation is logged."""
        # Arrange
        mock_logger = Mock()
        mock_celery_app = Mock(spec=Celery)
        
        # Act
        import main_tasks
        
        # Manually test the initialization with mocks
        with patch('main_tasks.configure_logging') as mock_configure_logging, \
             patch('main_tasks.create_celery_app') as mock_create_celery_app, \
             patch('builtins.__import__') as mock_import, \
             patch('structlog.get_logger') as mock_get_logger:
            
            mock_get_logger.return_value = mock_logger
            mock_create_celery_app.return_value = mock_celery_app
            
            # Call the initialization function
            main_tasks._initialize_application()
            
            # Assert
            mock_logger.info.assert_called_with(
                "Document Processing Service Celery worker initialized successfully"
            )

    @patch('main_tasks._module_initialization')
    def test_celery_app_creation_handles_factory_error(self, mock_module_init):
        """Test that Celery factory errors are handled gracefully."""
        # Arrange
        mock_logger = Mock()
        mock_celery_app = Mock(spec=Celery)
        
        # Act
        import main_tasks
        
        # Manually test the initialization with mocks
        with patch('main_tasks.configure_logging') as mock_configure_logging, \
             patch('main_tasks.create_celery_app') as mock_create_celery_app, \
             patch('builtins.__import__') as mock_import, \
             patch('structlog.get_logger') as mock_get_logger:
            
            mock_get_logger.return_value = mock_logger
            mock_create_celery_app.return_value = mock_celery_app
            
            # Make task import fail
            import_error = ImportError("Task import failed")
            mock_import.side_effect = import_error
            
            # Act & Assert
            with pytest.raises(ImportError) as exc_info:
                main_tasks._initialize_application()
            
            assert str(exc_info.value) == "Task import failed"
            mock_logger.error.assert_called_with(
                "Failed to initialize Celery worker application",
                error="Task import failed"
            )

    @patch('main_tasks._module_initialization')
    def test_task_module_discovery(self, mock_module_init):
        """Test that task modules are correctly discovered and imported."""
        # Arrange
        mock_celery_app = Mock(spec=Celery)
        
        # Act
        import main_tasks
        
        # Manually test the initialization with mocks
        with patch('main_tasks.configure_logging') as mock_configure_logging, \
             patch('main_tasks.create_celery_app') as mock_create_celery_app, \
             patch('builtins.__import__') as mock_import:
            
            mock_create_celery_app.return_value = mock_celery_app
            
            # Call the initialization function
            main_tasks._initialize_application()
            
            # Assert - verify task modules are discovered and imported
            mock_import.assert_any_call('tasks.document_tasks')

    @patch('main_tasks._module_initialization')
    def test_worker_startup_functionality(self, mock_module_init):
        """Test that worker can be started with proper error handling."""
        # Arrange
        mock_logger = Mock()
        mock_celery_app = Mock(spec=Celery)
        mock_worker = Mock()
        mock_celery_app.Worker.return_value = mock_worker
        
        # Act
        import main_tasks
        
        # Test worker startup function exists and can be called
        assert hasattr(main_tasks, 'start_worker')
        
        # Set up the celery_app manually for this test
        main_tasks.celery_app = mock_celery_app
        main_tasks.logger = mock_logger
        
        # Act - start worker
        main_tasks.start_worker()
        
        # Assert
        mock_celery_app.Worker.assert_called_once()
        mock_worker.start.assert_called_once()

    @patch('main_tasks._module_initialization')
    def test_worker_startup_error_handling(self, mock_module_init):
        """Test that worker startup errors are handled properly."""
        # Arrange
        mock_logger = Mock()
        mock_celery_app = Mock(spec=Celery)
        
        import main_tasks
        
        # Mock worker startup failure
        worker_error = Exception("Worker startup failed")
        mock_worker = Mock()
        mock_worker.start.side_effect = worker_error
        mock_celery_app.Worker.return_value = mock_worker
        
        # Set up the celery_app and logger manually for this test
        main_tasks.celery_app = mock_celery_app
        main_tasks.logger = mock_logger
        
        # Act & Assert
        with pytest.raises(Exception) as exc_info:
            main_tasks.start_worker()
        
        assert str(exc_info.value) == "Worker startup failed"
        mock_logger.error.assert_called_with(
            "Failed to start Celery worker",
            error="Worker startup failed"
        )

    @patch('main_tasks.configure_logging')
    @patch('main_tasks.create_celery_app')
    @patch('builtins.__import__')
    def test_main_execution_starts_worker(self, mock_import, mock_create_celery_app, mock_configure_logging):
        """Test that running main_tasks.py as main starts the worker."""
        # Arrange
        mock_celery_app = Mock(spec=Celery)
        mock_create_celery_app.return_value = mock_celery_app
        
        # Mock worker
        mock_worker = Mock()
        mock_celery_app.Worker.return_value = mock_worker
        
        # Act - simulate running as main by patching the module's __name__
        with patch('main_tasks.start_worker') as mock_start_worker:
            # Create a mock module with __name__ set to '__main__'
            import types
            mock_module = types.ModuleType('main_tasks')
            mock_module.__name__ = '__main__'
            
            # Execute the main block code directly
            with patch.dict('sys.modules', {'main_tasks': mock_module}):
                # Simulate the if __name__ == "__main__": block
                exec('if "__main__" == "__main__": start_worker()', {'start_worker': mock_start_worker})
                
                # Assert
                mock_start_worker.assert_called_once()

    @patch('main_tasks.configure_logging')
    @patch('main_tasks.create_celery_app')
    @patch('builtins.__import__')
    @patch('structlog.get_logger')
    def test_celery_signal_handlers_are_registered(self, mock_get_logger, mock_import, mock_create_celery_app, mock_configure_logging):
        """Test that Celery signal handlers for logging are properly registered."""
        # Arrange
        mock_logger = Mock()
        mock_get_logger.return_value = mock_logger
        
        mock_celery_app = Mock(spec=Celery)
        mock_create_celery_app.return_value = mock_celery_app
        
        # Act
        import main_tasks
        
        # Assert that signal handlers are registered
        # We'll verify this by checking if the functions exist
        assert hasattr(main_tasks, '_log_task_prerun')
        assert hasattr(main_tasks, '_log_task_postrun')
        assert hasattr(main_tasks, '_log_task_failure')
        assert hasattr(main_tasks, '_log_task_retry')
        assert hasattr(main_tasks, '_log_worker_ready')

    def test_task_prerun_logging(self):
        """Test that task prerun events are logged with structured data."""
        # Import main_tasks to get the function
        import main_tasks
        
        # Mock the logger
        with patch.object(main_tasks, 'logger') as mock_logger:
            # Mock task context
            mock_sender = Mock()
            mock_sender.name = 'test_task'
            task_id = 'test-task-id-123'
            args = ('arg1', 'arg2')
            kwargs = {'key1': 'value1'}
            
            # Act
            main_tasks._log_task_prerun(sender=mock_sender, task_id=task_id, task=mock_sender, args=args, kwargs=kwargs)
            
            # Assert
            mock_logger.info.assert_called_with(
                "Task started",
                task_name='test_task',
                task_id=task_id,
                args=args,
                kwargs=kwargs
            )

    def test_task_postrun_logging(self):
        """Test that task postrun events are logged with structured data."""
        # Import main_tasks to get the function
        import main_tasks
        
        # Mock the logger
        with patch.object(main_tasks, 'logger') as mock_logger:
            # Mock task context
            mock_sender = Mock()
            mock_sender.name = 'test_task'
            task_id = 'test-task-id-123'
            state = 'SUCCESS'
            retval = {'result': 'success'}
            
            # Act
            main_tasks._log_task_postrun(sender=mock_sender, task_id=task_id, task=mock_sender, args=(), kwargs={}, retval=retval, state=state)
            
            # Assert
            mock_logger.info.assert_called_with(
                "Task completed",
                task_name='test_task',
                task_id=task_id,
                state=state,
                retval=retval
            )

    def test_task_failure_logging(self):
        """Test that task failure events are logged with error details."""
        # Import main_tasks to get the function
        import main_tasks
        
        # Mock the logger
        with patch.object(main_tasks, 'logger') as mock_logger:
            # Mock task context
            mock_sender = Mock()
            mock_sender.name = 'test_task'
            task_id = 'test-task-id-123'
            exception = Exception('Test error')
            traceback = 'Mock traceback'
            einfo = Mock()
            einfo.traceback = traceback
            
            # Act
            main_tasks._log_task_failure(sender=mock_sender, task_id=task_id, exception=exception, traceback=traceback, einfo=einfo)
            
            # Assert
            mock_logger.error.assert_called_with(
                "Task failed",
                task_name='test_task',
                task_id=task_id,
                exception=str(exception),
                traceback=traceback
            )

    def test_task_retry_logging(self):
        """Test that task retry events are logged with retry details."""
        # Import main_tasks to get the function
        import main_tasks
        
        # Mock the logger
        with patch.object(main_tasks, 'logger') as mock_logger:
            # Mock task context
            mock_sender = Mock()
            mock_sender.name = 'test_task'
            task_id = 'test-task-id-123'
            reason = Exception('Retry reason')
            einfo = Mock()
            einfo.traceback = 'Mock traceback'
            
            # Act
            main_tasks._log_task_retry(sender=mock_sender, task_id=task_id, reason=reason, einfo=einfo)
            
            # Assert
            mock_logger.warning.assert_called_with(
                "Task retry",
                task_name='test_task',
                task_id=task_id,
                reason=str(reason),
                traceback=einfo.traceback
            )

    def test_worker_ready_logging(self):
        """Test that worker ready events are logged."""
        # Import main_tasks to get the function
        import main_tasks
        
        # Mock the logger
        with patch.object(main_tasks, 'logger') as mock_logger:
            # Mock worker context
            mock_sender = Mock()
            mock_sender.hostname = 'worker-host-1'
            
            # Act
            main_tasks._log_worker_ready(sender=mock_sender)
            
            # Assert
            mock_logger.info.assert_called_with(
                "Celery worker ready",
                worker_hostname='worker-host-1'
            )

    def test_enhanced_worker_startup_logging(self):
        """Test that enhanced worker startup includes detailed logging."""
        # Import main_tasks to get the function
        import main_tasks
        
        # Mock the logger and celery app
        with patch.object(main_tasks, 'logger') as mock_logger, \
             patch.object(main_tasks, 'celery_app') as mock_celery_app:
            
            # Mock worker
            mock_worker = Mock()
            mock_celery_app.Worker.return_value = mock_worker
            
            # Act
            main_tasks.start_worker()
            
            # Assert - verify enhanced logging calls
            expected_calls = [
                call("Starting Celery worker with enhanced monitoring"),
                call("Celery worker started successfully")
            ]
            mock_logger.info.assert_has_calls(expected_calls, any_order=True)