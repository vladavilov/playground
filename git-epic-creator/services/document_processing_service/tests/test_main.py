"""
Tests for main.py Celery worker entry point.
"""

import sys
from unittest.mock import Mock, patch
import pytest
from celery import Celery

class TestMain:
    """Test cases for main.py Celery worker entry point."""

    def setup_method(self):
        """Clean up any imported modules before each test."""
        # Remove main from sys.modules if it exists to ensure clean imports
        modules_to_remove = [
            'main',
            'tasks.document_tasks'
        ]
        for module in modules_to_remove:
            if module in sys.modules:
                del sys.modules[module]

    def test_module_initialization_creates_celery_app(self):
        """Test that importing main module initializes Celery app and logger."""
        # Mock the problematic task module in sys.modules
        mock_task_module = Mock()
        
        with patch('utils.celery_factory.CeleryFactory.create_celery_app') as mock_create_celery_app, \
             patch('configuration.logging_config.configure_logging') as mock_configure_logging, \
             patch('structlog.get_logger') as mock_get_logger, \
             patch('configuration.celery_config.get_celery_settings') as mock_get_celery_settings, \
             patch.dict('sys.modules', {'tasks.document_tasks': mock_task_module}):
            
            # Arrange
            mock_logger = Mock()
            mock_celery_app = Mock(spec=Celery)
            mock_settings = Mock()
            
            mock_get_logger.return_value = mock_logger
            mock_create_celery_app.return_value = mock_celery_app
            mock_get_celery_settings.return_value = mock_settings
            
            # Act - Import main module
            import main
            
            # Assert
            mock_configure_logging.assert_called_once()
            mock_get_celery_settings.assert_called_once()
            mock_create_celery_app.assert_called_once_with(
                name="document_processing_service",
                settings=mock_settings
            )
            
            # Verify module has initialized globals
            assert main.logger is not None
            assert main.celery_app is not None

    def test_start_worker_creates_and_starts_celery_worker(self):
        """Test that start_worker creates and starts a Celery worker."""
        mock_task_module = Mock()
        
        with patch('utils.celery_factory.CeleryFactory.create_celery_app') as mock_create_celery_app, \
             patch('configuration.logging_config.configure_logging') as mock_configure_logging, \
             patch('structlog.get_logger') as mock_get_logger, \
             patch.dict('sys.modules', {'tasks.document_tasks': mock_task_module}):
            
            # Arrange
            mock_logger = Mock()
            mock_celery_app = Mock(spec=Celery)
            mock_worker = Mock()
            
            mock_get_logger.return_value = mock_logger
            mock_create_celery_app.return_value = mock_celery_app
            mock_celery_app.Worker.return_value = mock_worker
            
            import main
            
            # Act
            main.start_worker()
            
            # Assert
            mock_celery_app.Worker.assert_called_once()
            mock_worker.start.assert_called_once()
            
            # Verify logging behavior
            assert any("Starting Celery worker" in str(call) for call in mock_logger.info.call_args_list)
            assert any("worker started successfully" in str(call) for call in mock_logger.info.call_args_list)

    def test_start_worker_fails_when_celery_app_is_none(self):
        """Test that start_worker fails gracefully when celery_app is None."""
        mock_task_module = Mock()
        
        with patch('utils.celery_factory.CeleryFactory.create_celery_app') as mock_create_celery_app, \
             patch('configuration.logging_config.configure_logging') as mock_configure_logging, \
             patch('structlog.get_logger') as mock_get_logger, \
             patch('configuration.celery_config.get_celery_settings') as mock_get_celery_settings, \
             patch.dict('sys.modules', {'tasks.document_tasks': mock_task_module}):
            
            # Arrange
            mock_logger = Mock()
            mock_celery_app = Mock(spec=Celery)
            mock_settings = Mock()
            
            mock_get_logger.return_value = mock_logger
            mock_create_celery_app.return_value = mock_celery_app
            mock_get_celery_settings.return_value = mock_settings
            
            import main
            
            # Reset celery_app to None to simulate failure scenario
            main.celery_app = None
            
            # Act & Assert
            with pytest.raises(AttributeError, match="'NoneType' object has no attribute 'Worker'"):
                main.start_worker()
            
            # Verify error was logged
            mock_logger.error.assert_called_with("Failed to start Celery worker", error="'NoneType' object has no attribute 'Worker'")

    def test_signal_handlers_are_defined_and_callable(self):
        """Test that all Celery signal handlers are defined and callable."""
        mock_task_module = Mock()
        
        with patch('utils.celery_factory.CeleryFactory.create_celery_app') as mock_create_celery_app, \
             patch('configuration.logging_config.configure_logging') as mock_configure_logging, \
             patch('structlog.get_logger') as mock_get_logger, \
             patch.dict('sys.modules', {'tasks.document_tasks': mock_task_module}):
            
            # Arrange
            mock_logger = Mock()
            mock_celery_app = Mock(spec=Celery)
            
            mock_get_logger.return_value = mock_logger
            mock_create_celery_app.return_value = mock_celery_app
            
            # Act
            import main
            
            # Assert - Check signal handlers exist and are callable
            signal_handlers = [
                '_log_task_prerun',
                '_log_task_postrun', 
                '_log_task_failure',
                '_log_task_retry',
                '_log_worker_ready'
            ]
            
            for handler_name in signal_handlers:
                assert hasattr(main, handler_name), f"Handler {handler_name} not found"
                assert callable(getattr(main, handler_name)), f"Handler {handler_name} not callable"

    def test_signal_handlers_log_correctly(self):
        """Test that signal handlers log with correct structured data."""
        mock_task_module = Mock()
        
        with patch('utils.celery_factory.CeleryFactory.create_celery_app') as mock_create_celery_app, \
             patch('configuration.logging_config.configure_logging') as mock_configure_logging, \
             patch('structlog.get_logger') as mock_get_logger, \
             patch.dict('sys.modules', {'tasks.document_tasks': mock_task_module}):
            
            # Arrange
            mock_logger = Mock()
            mock_celery_app = Mock(spec=Celery)
            
            mock_get_logger.return_value = mock_logger
            mock_create_celery_app.return_value = mock_celery_app
            
            import main
            
            # Test task_prerun handler
            mock_sender = Mock()
            mock_sender.name = "test_task"
            
            # Act
            main._log_task_prerun(sender=mock_sender, task_id="123", args=["arg1"], kwargs={"key": "value"})
            
            # Assert
            mock_logger.info.assert_called_with(
                "Task started",
                task_name="test_task",
                task_id="123",
                args=["arg1"],
                kwargs={"key": "value"}
            )

    def test_initialization_error_handling(self):
        """Test that initialization errors are properly handled and logged."""
        mock_task_module = Mock()
        
        with patch('utils.celery_factory.CeleryFactory.create_celery_app') as mock_create_celery_app, \
             patch('configuration.logging_config.configure_logging') as mock_configure_logging, \
             patch('structlog.get_logger') as mock_get_logger, \
             patch.dict('sys.modules', {'tasks.document_tasks': mock_task_module}):
            
            # Arrange
            mock_logger = Mock()
            mock_create_celery_app.side_effect = Exception("Celery creation failed")
            mock_get_logger.return_value = mock_logger
            
            # Act & Assert
            with pytest.raises(Exception, match="Celery creation failed"):
                import main

    def test_start_worker_error_handling(self):
        """Test that start_worker handles errors properly."""
        mock_task_module = Mock()
        
        with patch('utils.celery_factory.CeleryFactory.create_celery_app') as mock_create_celery_app, \
             patch('configuration.logging_config.configure_logging') as mock_configure_logging, \
             patch('structlog.get_logger') as mock_get_logger, \
             patch.dict('sys.modules', {'tasks.document_tasks': mock_task_module}):
            
            # Arrange
            mock_logger = Mock()
            mock_celery_app = Mock(spec=Celery)
            mock_worker = Mock()
            
            mock_get_logger.return_value = mock_logger
            mock_create_celery_app.return_value = mock_celery_app
            mock_celery_app.Worker.return_value = mock_worker
            mock_worker.start.side_effect = Exception("Worker start failed")
            
            import main
            
            # Act & Assert
            with pytest.raises(Exception, match="Worker start failed"):
                main.start_worker()
            
            # Verify error was logged
            mock_logger.error.assert_called_with("Failed to start Celery worker", error="Worker start failed")
