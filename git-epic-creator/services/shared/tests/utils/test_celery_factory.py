"""
Tests for Celery factory.
"""

import pytest
from unittest.mock import Mock, patch, MagicMock
from celery import Celery
from utils.celery_factory import CeleryFactory
from configuration.celery_config import CelerySettings

@pytest.fixture
def mock_celery_settings():
    """Mock Celery settings."""
    settings = Mock(spec=CelerySettings)
    settings.CELERY_BROKER_URL = "redis://localhost:6379/0"
    settings.CELERY_RESULT_BACKEND = "redis://localhost:6379/0"
    settings.CELERY_TASK_SERIALIZER = "json"
    settings.CELERY_RESULT_SERIALIZER = "json"
    settings.CELERY_ACCEPT_CONTENT = ["json"]
    settings.CELERY_TIMEZONE = "UTC"
    settings.CELERY_ENABLE_UTC = True
    settings.CELERY_TASK_TRACK_STARTED = True
    settings.CELERY_TASK_TIME_LIMIT = 300
    settings.CELERY_TASK_SOFT_TIME_LIMIT = 240
    settings.CELERY_WORKER_PREFETCH_MULTIPLIER = 1
    settings.CELERY_WORKER_MAX_TASKS_PER_CHILD = 1000
    settings.CELERY_WORKER_CONCURRENCY = 4
    settings.CELERY_TASK_ROUTES = {
        'tasks.document_tasks.*': {'queue': 'document_processing'},
        'tasks.project_tasks.*': {'queue': 'project_management'},
    }
    return settings

@patch('utils.celery_factory.Celery')
def test_create_celery_app(mock_celery_class, mock_celery_settings):
    """Test Celery application creation."""
    mock_app = Mock(spec=Celery)
    mock_celery_class.return_value = mock_app
    
    app = CeleryFactory.create_celery_app("test_app", mock_celery_settings)
    
    mock_celery_class.assert_called_once_with("test_app")
    mock_app.conf.update.assert_called_once_with(
        broker_url=mock_celery_settings.CELERY_BROKER_URL,
        result_backend=mock_celery_settings.CELERY_RESULT_BACKEND,
        task_serializer=mock_celery_settings.CELERY_TASK_SERIALIZER,
        result_serializer=mock_celery_settings.CELERY_RESULT_SERIALIZER,
        accept_content=mock_celery_settings.CELERY_ACCEPT_CONTENT,
        timezone=mock_celery_settings.CELERY_TIMEZONE,
        enable_utc=mock_celery_settings.CELERY_ENABLE_UTC,
        task_track_started=mock_celery_settings.CELERY_TASK_TRACK_STARTED,
        task_time_limit=mock_celery_settings.CELERY_TASK_TIME_LIMIT,
        task_soft_time_limit=mock_celery_settings.CELERY_TASK_SOFT_TIME_LIMIT,
        worker_prefetch_multiplier=mock_celery_settings.CELERY_WORKER_PREFETCH_MULTIPLIER,
        worker_max_tasks_per_child=mock_celery_settings.CELERY_WORKER_MAX_TASKS_PER_CHILD,
        worker_concurrency=mock_celery_settings.CELERY_WORKER_CONCURRENCY,
        task_routes=mock_celery_settings.CELERY_TASK_ROUTES
    )
    assert app == mock_app

@patch('utils.celery_factory.Celery')
@patch('utils.celery_factory.get_celery_settings')
def test_create_celery_app_default_settings(mock_get_settings, mock_celery_class, mock_celery_settings):
    """Test Celery application creation with default settings."""
    mock_app = Mock(spec=Celery)
    mock_celery_class.return_value = mock_app
    mock_get_settings.return_value = mock_celery_settings
    
    app = CeleryFactory.create_celery_app("test_app")
    
    mock_get_settings.assert_called_once()
    mock_celery_class.assert_called_once_with("test_app")
    mock_app.conf.update.assert_called_once()
    assert app == mock_app


class TestCeleryWorkerAppCreation:
    """Test cases for create_celery_worker_app method."""

    @patch('utils.celery_factory.CeleryFactory.create_celery_app')
    @patch('utils.celery_factory.get_celery_settings')
    def test_create_celery_worker_app_default_settings(self, mock_get_settings, mock_create_celery_app, mock_celery_settings):
        """Test creating worker app with default settings."""
        # Setup mocks
        mock_app = Mock(spec=Celery)
        mock_create_celery_app.return_value = mock_app
        mock_get_settings.return_value = mock_celery_settings
        
        # Call the method without settings
        app = CeleryFactory.create_celery_worker_app(
            name="test_worker_app",
            task_modules=[]
        )
        
        # Verify default settings were retrieved
        mock_get_settings.assert_called_once()
        mock_create_celery_app.assert_called_once_with("test_worker_app", mock_celery_settings)
        assert app == mock_app

    @patch('utils.celery_factory.CeleryFactory.create_celery_app')
    def test_create_celery_worker_app_empty_task_modules(self, mock_create_celery_app, mock_celery_settings):
        """Test creating worker app with empty task modules list."""
        # Setup mocks
        mock_app = Mock(spec=Celery)
        mock_create_celery_app.return_value = mock_app
        
        # Call the method with empty task modules
        app = CeleryFactory.create_celery_worker_app(
            name="test_worker_app",
            settings=mock_celery_settings,
            task_modules=[]
        )
        
        # Verify app was created without task imports
        mock_create_celery_app.assert_called_once_with("test_worker_app", mock_celery_settings)
        assert app == mock_app

    def test_create_celery_worker_app_method_exists(self):
        """Test that the create_celery_worker_app method exists and is callable."""
        # Verify the method exists
        assert hasattr(CeleryFactory, 'create_celery_worker_app')
        assert callable(getattr(CeleryFactory, 'create_celery_worker_app'))
        
        # Verify method signature
        import inspect
        sig = inspect.signature(CeleryFactory.create_celery_worker_app)
        params = list(sig.parameters.keys())
        
        # Should have name, settings, and task_modules parameters
        assert 'name' in params
        assert 'settings' in params  
        assert 'task_modules' in params