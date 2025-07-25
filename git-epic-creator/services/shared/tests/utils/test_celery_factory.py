"""
Tests for Celery factory and health checker.
"""

from unittest.mock import Mock, patch
import pytest
from celery import Celery
from utils.celery_factory import CeleryFactory, CeleryHealthChecker, get_celery_app
from configuration.celery_config import CelerySettings


class TestCeleryFactory:
    """Test cases for CeleryFactory."""

    @pytest.fixture
    def mock_celery_settings(self):
        """Mock Celery settings with realistic configuration."""
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
    def test_create_celery_app_with_settings(self, mock_celery_class, mock_celery_settings):
        """Test Celery application creation with provided settings."""
        # Arrange
        mock_app = Mock(spec=Celery)
        mock_celery_class.return_value = mock_app
        app_name = "test_app"
        
        # Act
        result = CeleryFactory.create_celery_app(app_name, mock_celery_settings)
        
        # Assert
        mock_celery_class.assert_called_once_with(app_name)
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
        assert result == mock_app

    @patch('utils.celery_factory.Celery')
    @patch('utils.celery_factory.get_celery_settings')
    def test_create_celery_app_with_default_settings(self, mock_get_settings, mock_celery_class, mock_celery_settings):
        """Test Celery application creation with default settings."""
        # Arrange
        mock_app = Mock(spec=Celery)
        mock_celery_class.return_value = mock_app
        mock_get_settings.return_value = mock_celery_settings
        app_name = "test_app"

        # Act
        result = CeleryFactory.create_celery_app(app_name)

        # Assert
        mock_get_settings.assert_called_once()
        mock_celery_class.assert_called_once_with(app_name)
        mock_app.conf.update.assert_called_once()
        assert result == mock_app

    @patch('utils.celery_factory.get_celery_settings')
    @patch('utils.celery_factory.CeleryFactory.create_celery_app')
    def test_get_celery_app_caching(self, mock_create_app, mock_get_settings, mock_celery_settings):
        """Test that get_celery_app caches the result."""
        # Arrange
        mock_app = Mock(spec=Celery)
        mock_create_app.return_value = mock_app
        mock_get_settings.return_value = mock_celery_settings
        app_name = "test_app"
        
        # Clear cache before test
        get_celery_app.cache_clear()

        # Act
        result1 = get_celery_app(app_name)
        result2 = get_celery_app(app_name)

        # Assert
        assert result1 == result2
        mock_create_app.assert_called_once_with(app_name, mock_celery_settings)
        mock_get_settings.assert_called_once()


class TestCeleryHealthChecker:
    """Test cases for CeleryHealthChecker."""

    @pytest.fixture
    def mock_celery_app(self):
        """Mock Celery application."""
        app = Mock(spec=Celery)
        app.broker_url = "redis://localhost:6379/0"
        app.backend_url = "redis://localhost:6379/0"
        return app

    def test_check_health_with_details_success(self, mock_celery_app):
        """Test successful health check with detailed information."""
        # Arrange
        mock_control = Mock()
        mock_inspector = Mock()
        mock_celery_app.control = mock_control
        
        # Mock ping response
        mock_control.ping.return_value = [{'worker1': 'pong'}, {'worker2': 'pong'}]
        mock_control.inspect.return_value = mock_inspector
        
        # Mock inspector responses
        mock_inspector.active.return_value = {'worker1': [], 'worker2': []}
        mock_inspector.scheduled.return_value = {'worker1': [], 'worker2': []}
        mock_inspector.registered.return_value = {
            'worker1': ['task1', 'task2'],
            'worker2': ['task1', 'task2']
        }
        mock_inspector.stats.return_value = {
            'worker1': {'pid': 1234, 'uptime': 3600, 'total': 100},
            'worker2': {'pid': 5678, 'uptime': 3600, 'total': 150}
        }

        # Act
        result = CeleryHealthChecker.check_health_with_details(mock_celery_app)

        # Assert
        assert result["healthy"] is True
        assert result["active_workers_count"] == 2
        assert result["broker_url"] == "redis://localhost:6379/0"
        assert result["backend_url"] == "redis://localhost:6379/0"
        assert "worker_details" in result
        assert len(result["worker_details"]) == 2
        assert result["error"] is None

    def test_check_health_with_details_no_workers(self, mock_celery_app):
        """Test health check when no workers are available."""
        # Arrange
        mock_control = Mock()
        mock_celery_app.control = mock_control
        mock_control.ping.return_value = []

        # Act
        result = CeleryHealthChecker.check_health_with_details(mock_celery_app)

        # Assert
        assert result["healthy"] is False
        assert result["active_workers_count"] == 0
        assert "No active workers responded to ping" in result["error"]

    def test_check_health_with_details_exception(self, mock_celery_app):
        """Test health check when an exception occurs."""
        # Arrange
        mock_control = Mock()
        mock_celery_app.control = mock_control
        mock_control.ping.side_effect = Exception("Connection failed")

        # Act
        result = CeleryHealthChecker.check_health_with_details(mock_celery_app)

        # Assert
        assert result["healthy"] is False
        assert "Connection failed" in result["error"]
        assert result["active_workers_count"] == 0

    def test_check_health_with_details_inspector_failure(self, mock_celery_app):
        """Test health check when inspector methods fail."""
        # Arrange
        mock_control = Mock()
        mock_inspector = Mock()
        mock_celery_app.control = mock_control
        
        mock_control.ping.return_value = [{'worker1': 'pong'}]
        mock_control.inspect.return_value = mock_inspector
        
        # Mock inspector methods to return None (failure case)
        mock_inspector.active.return_value = None
        mock_inspector.scheduled.return_value = None
        mock_inspector.registered.return_value = None
        mock_inspector.stats.return_value = None

        # Act
        result = CeleryHealthChecker.check_health_with_details(mock_celery_app)

        # Assert
        assert result["healthy"] is True  # Still healthy if ping works
        assert result["active_workers_count"] == 1
        assert result["active_tasks"] == {}
        assert result["scheduled_tasks"] == {}
        assert result["registered_tasks"] == {}
        assert result["stats"] == {}
        assert result["worker_details"] == {}
