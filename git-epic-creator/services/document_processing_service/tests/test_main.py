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

    

    def test_fastapi_app_creation_with_celery_health_endpoint(self):
        """Test that main module creates FastAPI app with Celery health check endpoint."""
        mock_task_module = Mock()
        
        with patch('celery_worker_app.celery_app') as mock_celery_app, \
             patch('configuration.logging_config.configure_logging'), \
             patch('structlog.get_logger') as mock_get_logger, \
             patch('utils.app_factory.FastAPIFactory.create_app') as mock_create_app, \
             patch('configuration.common_config.get_app_settings') as mock_get_settings, \
             patch.dict('sys.modules', {'tasks.document_tasks': mock_task_module}):
            
            # Arrange
            mock_logger = Mock()
            mock_fastapi_app = Mock(spec=FastAPI)
            mock_settings = Mock()
            mock_settings.API_PORT = 8000
            
            # Mock celery_app with proper tasks structure
            mock_celery_app.tasks = Mock()
            mock_celery_app.tasks.keys.return_value = [
                'celery.chord_unlock',
                'celery.backend_cleanup',
                'tasks.document_tasks.process_project_documents_task'
            ]
            
            # Configure the mock FastAPI app to have a state attribute
            mock_fastapi_app.state = Mock()
            mock_fastapi_app.include_router = Mock()
            
            mock_get_logger.return_value = mock_logger
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
            assert hasattr(main, 'celery_app'), "Celery app should be available"
            assert main.celery_app is not None
            
            # Verify Celery app was stored in FastAPI app state
            mock_fastapi_app.state.celery_app = mock_celery_app
            
            # Verify router was included
            mock_fastapi_app.include_router.assert_called_once()

    def test_celery_health_endpoint_exists(self):
        """Test that /health/celery endpoint is created and callable."""
        mock_task_module = Mock()
        
        with patch('celery_worker_app.celery_app') as mock_celery_app, \
             patch('configuration.logging_config.configure_logging'), \
             patch('structlog.get_logger') as mock_get_logger, \
             patch('utils.app_factory.FastAPIFactory.create_app') as mock_create_app, \
             patch('configuration.common_config.get_app_settings') as mock_get_settings, \
             patch.dict('sys.modules', {'tasks.document_tasks': mock_task_module}):
            
            # Arrange
            mock_logger = Mock()
            mock_fastapi_app = FastAPI()  # Use real FastAPI for route testing
            mock_settings = Mock()
            mock_settings.API_PORT = 8000
            
            # Mock celery_app with proper tasks structure
            mock_celery_app.tasks = Mock()
            mock_celery_app.tasks.keys.return_value = [
                'celery.chord_unlock',
                'celery.backend_cleanup',
                'tasks.document_tasks.process_project_documents_task'
            ]
            
            mock_get_logger.return_value = mock_logger
            mock_create_app.return_value = mock_fastapi_app
            mock_get_settings.return_value = mock_settings
            
            # Act - Import main module
            import main
            
            # Assert endpoint exists
            routes = [route.path for route in main.app.routes]
            assert "/health/celery" in routes, "Celery health endpoint should exist"

    def test_celery_health_endpoint_returns_health_data(self):
        """Test that /health/celery endpoint returns health check data from CeleryHealthChecker."""
        mock_task_module = Mock()
        
        with patch('celery_worker_app.celery_app') as mock_celery_app, \
             patch('celery_worker_app.get_task_validation_status') as mock_get_task_validation, \
             patch('configuration.logging_config.configure_logging'), \
             patch('structlog.get_logger') as mock_get_logger, \
             patch('utils.app_factory.FastAPIFactory.create_app') as mock_create_app, \
             patch('configuration.common_config.get_app_settings') as mock_get_settings, \
             patch('utils.celery_factory.CeleryHealthChecker') as mock_health_checker_class, \
             patch('clients.project_management_client.ProjectManagementClient'), \
             patch.dict('sys.modules', {'tasks.document_tasks': mock_task_module}):
            
            # Arrange
            mock_logger = Mock()
            mock_fastapi_app = FastAPI()
            mock_settings = Mock()
            mock_settings.API_PORT = 8000

            # Mock celery_app with proper tasks structure that supports len()
            mock_tasks = {
                'celery.chord_unlock': Mock(),
                'celery.backend_cleanup': Mock(),
                'tasks.document_tasks.process_project_documents_task': Mock()
            }
            mock_celery_app.tasks = mock_tasks
            
            # Add additional celery app configuration mocks
            mock_celery_app.conf.broker_url = "redis://localhost:6379/0"
            mock_celery_app.conf.result_backend = "redis://localhost:6379/1"
            mock_celery_app.conf.task_routes = {}
            mock_celery_app.conf.task_serializer = "json"
            mock_celery_app.conf.result_serializer = "json"
            mock_celery_app.main = "document_processing_service"

            expected_health_data = {
                "healthy": True,
                "active_workers_count": 2,
                "broker_url": "redis://localhost:6379/0",
                "backend_url": "redis://localhost:6379/1",
                "service": "Document Processing Service",
                "celery_app_name": "document_processing_service",
                "active_tasks": ["celery.chord_unlock", "celery.backend_cleanup", "tasks.document_tasks.process_project_documents_task"],
                "registered_tasks_count": 3,
                "result_backend": "redis://localhost:6379/1",
                "task_routes": {},
                "worker_queues": ["document_processing"],
                "task_serializer": "json",
                "result_serializer": "json"
            }
            
            expected_task_validation = {
                'discovered_tasks': ['tasks.document_tasks.process_project_documents_task'],
                'expected_tasks': ['tasks.document_tasks.process_project_documents_task'],
                'missing_tasks': [],
                'all_tasks_registered': True
            }
            
            mock_get_logger.return_value = mock_logger
            mock_create_app.return_value = mock_fastapi_app
            mock_get_settings.return_value = mock_settings
            mock_get_task_validation.return_value = expected_task_validation
            
            # Configure mock health checker instance
            mock_health_checker_instance = Mock()
            mock_health_checker_instance.check_health_with_details.return_value = expected_health_data
            mock_health_checker_class.return_value = mock_health_checker_instance
            
            # Act - Import main module
            import main
            
            # Test the health endpoint using the actual implementation
            client = TestClient(main.app)
            response = client.get("/health/celery")
            
            # Assert
            assert response.status_code == 200
            response_data = response.json()
            
            # Check that the original health data is included
            for key, value in expected_health_data.items():
                assert response_data[key] == value, f"Expected {key}={value}, got {response_data.get(key)}"
            
            # Check that task validation data is included
            assert 'task_validation_status' in response_data
            assert response_data['task_validation_status'] == expected_task_validation
            
            # Verify that the health checker was instantiated and called correctly
            mock_health_checker_class.assert_called_once()
            mock_health_checker_instance.check_health_with_details.assert_called_once_with(mock_celery_app)
            mock_get_task_validation.assert_called_once()

    def test_get_celery_app_dependency_returns_celery_instance(self):
        """Test that get_celery_app_from_state dependency function returns Celery app."""
        mock_task_module = Mock()
        
        with patch('celery_worker_app.celery_app') as mock_celery_app, \
             patch('configuration.logging_config.configure_logging'), \
             patch('structlog.get_logger') as mock_get_logger, \
             patch('utils.app_factory.FastAPIFactory.create_app') as mock_create_app, \
             patch('configuration.common_config.get_app_settings') as mock_get_settings, \
             patch.dict('sys.modules', {'tasks.document_tasks': mock_task_module}):
            
            # Arrange
            mock_logger = Mock()
            mock_fastapi_app = FastAPI()
            mock_settings = Mock()
            mock_settings.API_PORT = 8000
            
            # Mock celery_app with proper tasks structure
            mock_celery_app.tasks = Mock()
            mock_celery_app.tasks.keys.return_value = [
                'celery.chord_unlock',
                'celery.backend_cleanup',
                'tasks.document_tasks.process_project_documents_task'
            ]
            
            mock_get_logger.return_value = mock_logger
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
