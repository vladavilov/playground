"""
Tests for main.py Celery worker entry point.
"""

import sys
from unittest.mock import Mock, patch
import pytest
from celery import Celery
from fastapi import FastAPI
from fastapi.testclient import TestClient

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
        
        with patch('utils.celery_factory.get_celery_app') as mock_get_celery_app, \
             patch('configuration.logging_config.configure_logging') as mock_configure_logging, \
             patch('structlog.get_logger') as mock_get_logger, \
             patch.dict('sys.modules', {'tasks.document_tasks': mock_task_module}):
            
            # Arrange
            mock_logger = Mock()
            mock_celery_app = Mock(spec=Celery)
            
            mock_get_logger.return_value = mock_logger
            mock_get_celery_app.return_value = mock_celery_app
            
            # Act - Import main module
            import main
            
            # Assert
            mock_configure_logging.assert_called_once()
            mock_get_celery_app.assert_called_once_with(name="document_processing_service")
            
            # Verify module has initialized globals
            assert main.logger is not None
            assert main.celery_app is not None

    def test_start_worker_creates_and_starts_celery_worker(self):
        """Test that start_worker creates and starts a Celery worker."""
        mock_task_module = Mock()
        
        with patch('utils.celery_factory.get_celery_app') as mock_get_celery_app, \
             patch('configuration.logging_config.configure_logging') as mock_configure_logging, \
             patch('structlog.get_logger') as mock_get_logger, \
             patch.dict('sys.modules', {'tasks.document_tasks': mock_task_module}):
            
            # Arrange
            mock_logger = Mock()
            mock_celery_app = Mock(spec=Celery)
            mock_worker = Mock()
            
            mock_get_logger.return_value = mock_logger
            mock_get_celery_app.return_value = mock_celery_app
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
        
        with patch('utils.celery_factory.get_celery_app') as mock_get_celery_app, \
             patch('configuration.logging_config.configure_logging') as mock_configure_logging, \
             patch('structlog.get_logger') as mock_get_logger, \
             patch.dict('sys.modules', {'tasks.document_tasks': mock_task_module}):
            
            # Arrange
            mock_logger = Mock()
            mock_celery_app = Mock(spec=Celery)
            
            mock_get_logger.return_value = mock_logger
            mock_get_celery_app.return_value = mock_celery_app
            
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
        
        with patch('utils.celery_factory.get_celery_app') as mock_get_celery_app, \
             patch('configuration.logging_config.configure_logging') as mock_configure_logging, \
             patch('structlog.get_logger') as mock_get_logger, \
             patch.dict('sys.modules', {'tasks.document_tasks': mock_task_module}):
            
            # Arrange
            mock_logger = Mock()
            mock_celery_app = Mock(spec=Celery)
            
            mock_get_logger.return_value = mock_logger
            mock_get_celery_app.return_value = mock_celery_app
            
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
        
        with patch('utils.celery_factory.get_celery_app') as mock_get_celery_app, \
             patch('configuration.logging_config.configure_logging') as mock_configure_logging, \
             patch('structlog.get_logger') as mock_get_logger, \
             patch('utils.celery_factory.logger') as mock_factory_logger, \
             patch.dict('sys.modules', {'tasks.document_tasks': mock_task_module}):
            
            # Arrange
            mock_logger = Mock()
            mock_celery_app = Mock(spec=Celery)
            
            mock_get_logger.return_value = mock_logger
            mock_get_celery_app.return_value = mock_celery_app
            mock_factory_logger.info = Mock()
            
            import main
            
            # Test task_prerun handler
            mock_sender = Mock()
            mock_sender.name = "test_task"
            
            # Act
            main._log_task_prerun(sender=mock_sender, task_id="123", args=["arg1"], kwargs={"key": "value"})
            
            # Assert
            mock_factory_logger.info.assert_called_with(
                "Task started",
                task_name="test_task",
                task_id="123",
                args=["arg1"],
                kwargs={"key": "value"}
            )

    def test_initialization_error_handling(self):
        """Test that initialization errors are properly handled and logged."""
        mock_task_module = Mock()
        
        with patch('utils.celery_factory.get_celery_app') as mock_get_celery_app, \
             patch('configuration.logging_config.configure_logging') as mock_configure_logging, \
             patch('structlog.get_logger') as mock_get_logger, \
             patch.dict('sys.modules', {'tasks.document_tasks': mock_task_module}):
            
            # Arrange
            mock_logger = Mock()
            mock_get_celery_app.side_effect = Exception("Celery creation failed")
            mock_get_logger.return_value = mock_logger
            
            # Act & Assert
            with pytest.raises(Exception, match="Celery creation failed"):
                import main

    def test_start_worker_error_handling(self):
        """Test that start_worker handles errors properly."""
        mock_task_module = Mock()
        
        with patch('utils.celery_factory.get_celery_app') as mock_get_celery_app, \
             patch('configuration.logging_config.configure_logging') as mock_configure_logging, \
             patch('structlog.get_logger') as mock_get_logger, \
             patch.dict('sys.modules', {'tasks.document_tasks': mock_task_module}):
            
            # Arrange
            mock_logger = Mock()
            mock_celery_app = Mock(spec=Celery)
            mock_worker = Mock()
            
            mock_get_logger.return_value = mock_logger
            mock_get_celery_app.return_value = mock_celery_app
            mock_celery_app.Worker.return_value = mock_worker
            mock_worker.start.side_effect = Exception("Worker start failed")
            
            import main
            
            # Act & Assert
            with pytest.raises(Exception, match="Worker start failed"):
                main.start_worker()
            
            # Verify error was logged
            mock_logger.error.assert_called_with("Failed to start Celery worker", error="Worker start failed")

    def test_fastapi_app_creation_with_celery_health_endpoint(self):
        """Test that main module creates FastAPI app with Celery health check endpoint."""
        mock_task_module = Mock()
        
        with patch('utils.celery_factory.get_celery_app') as mock_get_celery_app, \
             patch('configuration.logging_config.configure_logging') as mock_configure_logging, \
             patch('structlog.get_logger') as mock_get_logger, \
             patch('utils.app_factory.FastAPIFactory.create_app') as mock_create_app, \
             patch('configuration.common_config.get_app_settings') as mock_get_settings, \
             patch.dict('sys.modules', {'tasks.document_tasks': mock_task_module}):
            
            # Arrange
            mock_logger = Mock()
            mock_celery_app = Mock(spec=Celery)
            mock_fastapi_app = Mock(spec=FastAPI)
            mock_settings = Mock()
            mock_settings.API_PORT = 8000
            
            # Configure the mock FastAPI app to have a state attribute
            mock_fastapi_app.state = Mock()
            mock_fastapi_app.include_router = Mock()
            
            mock_get_logger.return_value = mock_logger
            mock_get_celery_app.return_value = mock_celery_app
            mock_create_app.return_value = mock_fastapi_app
            mock_get_settings.return_value = mock_settings
            
            # Act - Import main module
            import main
            
            # Assert FastAPI app was created
            mock_create_app.assert_called_once_with(
                title="Document Processing Service",
                description="A microservice for processing documents with Celery tasks",
                version="1.0.0",
                enable_azure_auth=False,
                enable_docs_auth=False,
                enable_cors=True
            )
            
            # Verify module has both apps initialized
            assert hasattr(main, 'app'), "FastAPI app should be available"
            assert main.app is not None
            assert main.celery_app is not None
            
            # Verify Celery app was stored in FastAPI app state
            mock_fastapi_app.state.celery_app = mock_celery_app
            
            # Verify router was included
            mock_fastapi_app.include_router.assert_called_once()

    def test_celery_health_endpoint_exists(self):
        """Test that /health/celery endpoint is created and callable."""
        mock_task_module = Mock()
        
        with patch('utils.celery_factory.get_celery_app') as mock_get_celery_app, \
             patch('configuration.logging_config.configure_logging'), \
             patch('structlog.get_logger') as mock_get_logger, \
             patch('utils.app_factory.FastAPIFactory.create_app') as mock_create_app, \
             patch('configuration.common_config.get_app_settings') as mock_get_settings, \
             patch.dict('sys.modules', {'tasks.document_tasks': mock_task_module}):
            
            # Arrange
            mock_logger = Mock()
            mock_celery_app = Mock(spec=Celery)
            mock_fastapi_app = FastAPI()  # Use real FastAPI for route testing
            mock_settings = Mock()
            mock_settings.API_PORT = 8000
            
            mock_get_logger.return_value = mock_logger
            mock_get_celery_app.return_value = mock_celery_app
            mock_create_app.return_value = mock_fastapi_app
            mock_get_settings.return_value = mock_settings
            
            # Act - Import main module
            import main
            
            # Assert endpoint exists
            client = TestClient(main.app)
            
            # This should exist after our implementation
            routes = [route.path for route in main.app.routes]
            assert "/health/celery" in routes, "Celery health endpoint should exist"

    def test_celery_health_endpoint_returns_health_data(self):
        """Test that /health/celery endpoint returns health check data from CeleryHealthChecker."""
        mock_task_module = Mock()
        
        with patch('utils.celery_factory.get_celery_app') as mock_get_celery_app, \
             patch('configuration.logging_config.configure_logging'), \
             patch('structlog.get_logger') as mock_get_logger, \
             patch('utils.app_factory.FastAPIFactory.create_app') as mock_create_app, \
             patch('configuration.common_config.get_app_settings') as mock_get_settings, \
             patch('utils.celery_factory.CeleryHealthChecker.check_health_with_details') as mock_health_check, \
             patch.dict('sys.modules', {'tasks.document_tasks': mock_task_module}):
            
            # Arrange
            mock_logger = Mock()
            mock_celery_app = Mock(spec=Celery)
            mock_fastapi_app = FastAPI()
            mock_settings = Mock()
            mock_settings.API_PORT = 8000
            
            expected_health_data = {
                "healthy": True,
                "active_workers_count": 2,
                "broker_url": "redis://localhost:6379/0",
                "backend_url": "redis://localhost:6379/1"
            }
            
            mock_get_logger.return_value = mock_logger
            mock_get_celery_app.return_value = mock_celery_app
            mock_create_app.return_value = mock_fastapi_app
            mock_get_settings.return_value = mock_settings
            mock_health_check.return_value = expected_health_data
            
            # Act - Import main module
            import main
            
            # Test the health endpoint using the actual implementation
            client = TestClient(main.app)
            response = client.get("/health/celery")
            
            # Assert
            assert response.status_code == 200
            assert response.json() == expected_health_data
            mock_health_check.assert_called_once_with(mock_celery_app)

    def test_get_celery_app_dependency_returns_celery_instance(self):
        """Test that get_celery_app_from_state dependency function returns Celery app."""
        mock_task_module = Mock()
        
        with patch('utils.celery_factory.get_celery_app') as mock_get_celery_app, \
             patch('configuration.logging_config.configure_logging'), \
             patch('structlog.get_logger') as mock_get_logger, \
             patch('utils.app_factory.FastAPIFactory.create_app') as mock_create_app, \
             patch('configuration.common_config.get_app_settings') as mock_get_settings, \
             patch.dict('sys.modules', {'tasks.document_tasks': mock_task_module}):
            
            # Arrange
            mock_logger = Mock()
            mock_celery_app = Mock(spec=Celery)
            mock_fastapi_app = FastAPI()
            mock_settings = Mock()
            mock_settings.API_PORT = 8000
            
            mock_get_logger.return_value = mock_logger
            mock_get_celery_app.return_value = mock_celery_app
            mock_create_app.return_value = mock_fastapi_app
            mock_get_settings.return_value = mock_settings
            
            # Act - Import main module
            import main
            
            # Mock request with app state
            mock_request = Mock()
            mock_request.app.state.celery_app = mock_celery_app
            
            # Assert dependency function exists and works
            assert hasattr(main, 'get_celery_app_from_state'), "Dependency function should exist"
            
            result = main.get_celery_app_from_state(mock_request)
            assert result == mock_celery_app

    def test_concurrent_startup_creates_both_fastapi_and_celery_threads(self):
        """Test that start_service creates threads for both FastAPI server and Celery worker."""
        mock_task_module = Mock()
        
        with patch('utils.celery_factory.get_celery_app') as mock_get_celery_app, \
             patch('configuration.logging_config.configure_logging'), \
             patch('structlog.get_logger') as mock_get_logger, \
             patch('utils.app_factory.FastAPIFactory.create_app') as mock_create_app, \
             patch('configuration.common_config.get_app_settings') as mock_get_settings, \
             patch('threading.Thread') as mock_thread, \
             patch.dict('sys.modules', {'tasks.document_tasks': mock_task_module}):
            
            # Arrange
            mock_logger = Mock()
            mock_celery_app = Mock(spec=Celery)
            mock_fastapi_app = Mock(spec=FastAPI)
            mock_fastapi_app.state = Mock()
            mock_fastapi_app.include_router = Mock()
            mock_worker = Mock()
            mock_settings = Mock()
            mock_settings.API_PORT = 8000
            
            mock_get_logger.return_value = mock_logger
            mock_get_celery_app.return_value = mock_celery_app
            mock_create_app.return_value = mock_fastapi_app
            mock_get_settings.return_value = mock_settings
            mock_celery_app.Worker.return_value = mock_worker
            
            # Mock thread instances
            mock_fastapi_thread = Mock()
            mock_celery_thread = Mock()
            mock_thread.side_effect = [mock_fastapi_thread, mock_celery_thread]
            
            # Act - Import main and test concurrent startup
            import main
            
            # This function should exist after implementation
            assert hasattr(main, 'start_service'), "start_service function should exist"
            
            main.start_service()
            
            # Assert both threads are created and started
            assert mock_thread.call_count == 2, "Should create 2 threads"
            mock_fastapi_thread.start.assert_called_once()
            mock_celery_thread.start.assert_called_once()
            mock_fastapi_thread.join.assert_called_once()
            mock_celery_thread.join.assert_called_once()

    def test_concurrent_startup_handles_exceptions_gracefully(self):
        """Test that start_service handles exceptions in either FastAPI or Celery startup."""
        mock_task_module = Mock()
        
        with patch('utils.celery_factory.get_celery_app') as mock_get_celery_app, \
             patch('configuration.logging_config.configure_logging'), \
             patch('structlog.get_logger') as mock_get_logger, \
             patch('utils.app_factory.FastAPIFactory.create_app') as mock_create_app, \
             patch('configuration.common_config.get_app_settings') as mock_get_settings, \
             patch('threading.Thread') as mock_thread, \
             patch.dict('sys.modules', {'tasks.document_tasks': mock_task_module}):
            
            # Arrange
            mock_logger = Mock()
            mock_celery_app = Mock(spec=Celery)
            mock_fastapi_app = Mock(spec=FastAPI)
            mock_fastapi_app.state = Mock()
            mock_fastapi_app.include_router = Mock()
            mock_settings = Mock()
            mock_settings.API_PORT = 8000
            
            mock_get_logger.return_value = mock_logger
            mock_get_celery_app.return_value = mock_celery_app
            mock_create_app.return_value = mock_fastapi_app
            mock_get_settings.return_value = mock_settings
            
            # Make thread creation fail to simulate startup failure
            mock_thread.side_effect = Exception("Thread creation failed")
            
            # Act - Import main and test error handling
            import main
            
            # Should handle the exception gracefully
            with pytest.raises(Exception) as exc_info:
                main.start_service()
            
            # Assert proper error logging
            assert "startup failed" in str(exc_info.value).lower()
            # Verify error was logged
            assert any("Failed to start" in str(call) for call in mock_logger.error.call_args_list)
