"""
Tests for Celery factory and health checker.
"""

from unittest.mock import Mock, patch
import pytest
from celery import Celery
from utils.celery_factory import CeleryHealthChecker, get_celery_app
from configuration.celery_config import CelerySettings


@pytest.fixture
def mock_celery_settings():
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
    settings.CELERY_BROKER_VISIBILITY_TIMEOUT = 420
    return settings


@patch('utils.celery_factory.Celery')
@patch('utils.celery_factory.get_celery_settings')
def test_get_celery_app_applies_configuration_posix(mock_get_settings, mock_celery_class, mock_celery_settings):
    # Arrange
    mock_app = Mock(spec=Celery)
    mock_celery_class.return_value = mock_app
    mock_get_settings.return_value = mock_celery_settings
    app_name = "test_app"
    get_celery_app.cache_clear()

    with patch('utils.celery_factory.os.name', 'posix'):
        # Act
        result = get_celery_app(app_name)

    # Assert
    mock_celery_class.assert_called_once_with(app_name)
    mock_get_settings.assert_called_once()
    kwargs = mock_app.conf.update.call_args.kwargs
    assert kwargs.get('broker_url') == mock_celery_settings.CELERY_BROKER_URL
    assert kwargs.get('result_backend') == mock_celery_settings.CELERY_RESULT_BACKEND
    assert kwargs.get('task_serializer') == mock_celery_settings.CELERY_TASK_SERIALIZER
    assert kwargs.get('result_serializer') == mock_celery_settings.CELERY_RESULT_SERIALIZER
    assert kwargs.get('accept_content') == mock_celery_settings.CELERY_ACCEPT_CONTENT
    assert kwargs.get('timezone') == mock_celery_settings.CELERY_TIMEZONE
    assert kwargs.get('enable_utc') == mock_celery_settings.CELERY_ENABLE_UTC
    assert kwargs.get('task_track_started') == mock_celery_settings.CELERY_TASK_TRACK_STARTED
    assert kwargs.get('task_time_limit') == mock_celery_settings.CELERY_TASK_TIME_LIMIT
    assert kwargs.get('task_soft_time_limit') == mock_celery_settings.CELERY_TASK_SOFT_TIME_LIMIT
    assert kwargs.get('worker_prefetch_multiplier') == mock_celery_settings.CELERY_WORKER_PREFETCH_MULTIPLIER
    assert kwargs.get('worker_max_tasks_per_child') == mock_celery_settings.CELERY_WORKER_MAX_TASKS_PER_CHILD
    assert kwargs.get('worker_concurrency') == mock_celery_settings.CELERY_WORKER_CONCURRENCY
    assert kwargs.get('task_routes') == mock_celery_settings.CELERY_TASK_ROUTES
    # Task protocol version 2 enables access to task headers
    assert kwargs.get('task_protocol') == 2
    # Reliability flags
    assert kwargs.get('task_reject_on_worker_lost') is True
    assert kwargs.get('task_acks_on_failure_or_timeout') is True
    # Broker transport options
    bto = kwargs.get('broker_transport_options')
    assert isinstance(bto, dict)
    assert bto.get('visibility_timeout') == mock_celery_settings.CELERY_BROKER_VISIBILITY_TIMEOUT
    assert result == mock_app


@patch('utils.celery_factory.Celery')
@patch('utils.celery_factory.get_celery_settings')
def test_get_celery_app_applies_configuration_windows_excludes_soft_timeout(mock_get_settings, mock_celery_class, mock_celery_settings):
    # Arrange
    mock_app = Mock(spec=Celery)
    mock_celery_class.return_value = mock_app
    mock_get_settings.return_value = mock_celery_settings
    app_name = "test_app_win"
    get_celery_app.cache_clear()

    with patch('utils.celery_factory.os.name', 'nt'):
        # Act
        result = get_celery_app(app_name)

    # Assert
    kwargs = mock_app.conf.update.call_args.kwargs
    assert 'task_soft_time_limit' not in kwargs
    assert result == mock_app


@patch('utils.celery_factory.Celery')
@patch('utils.celery_factory.get_celery_settings')
def test_get_celery_app_caching_returns_same_instance(mock_get_settings, mock_celery_class, mock_celery_settings):
    # Arrange
    mock_app = Mock(spec=Celery)
    mock_celery_class.return_value = mock_app
    mock_get_settings.return_value = mock_celery_settings
    app_name = "cache_app"
    get_celery_app.cache_clear()

    # Act
    result1 = get_celery_app(app_name)
    result2 = get_celery_app(app_name)

    # Assert
    assert result1 is result2
    mock_celery_class.assert_called_once_with(app_name)
    mock_get_settings.assert_called_once()


@patch('utils.celery_factory.Celery')
@patch('utils.celery_factory.get_celery_settings')
def test_get_celery_app_sets_reliability_and_visibility_timeout(mock_get_settings, mock_celery_class, mock_celery_settings):
    # Arrange
    mock_app = Mock(spec=Celery)
    mock_celery_class.return_value = mock_app
    mock_get_settings.return_value = mock_celery_settings
    get_celery_app.cache_clear()

    with patch('utils.celery_factory.os.name', 'posix'):
        get_celery_app("x")

    # Assert reliability flags included in conf.update
    kwargs = mock_app.conf.update.call_args.kwargs
    assert kwargs.get("task_acks_on_failure_or_timeout") is True
    assert kwargs.get("task_reject_on_worker_lost") is True
    # Broker visibility timeout propagated
    bto = kwargs.get("broker_transport_options")
    assert isinstance(bto, dict)
    assert bto.get("visibility_timeout") == mock_celery_settings.CELERY_BROKER_VISIBILITY_TIMEOUT

class TestCeleryHealthChecker:
    """Test cases for CeleryHealthChecker."""

    @pytest.fixture
    def mock_celery_app(self):
        """Mock Celery application."""
        app = Mock(spec=Celery)
        # Mock the conf attribute with broker_url and result_backend
        app.conf = Mock()
        app.conf.broker_url = "redis://localhost:6379/0"
        app.conf.result_backend = "redis://localhost:6379/0"
        return app

    def test_check_health_with_details_success(self, mock_celery_app):
        """Test successful health check with detailed information."""
        # Arrange
        mock_control = Mock()
        mock_inspector = Mock()
        mock_celery_app.control = mock_control
        
        # Mock broadcast response for ping command
        mock_control.broadcast.return_value = [{'worker1': 'pong'}, {'worker2': 'pong'}]
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
        mock_control.broadcast.return_value = []

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
        mock_control.broadcast.side_effect = Exception("Connection failed")

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
        
        mock_control.broadcast.return_value = [{'worker1': 'pong'}]
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

    def test_check_health_with_details_missing_conf_attributes(self):
        """Test health check when conf attributes are missing."""
        # Arrange
        app = Mock(spec=Celery)
        app.conf = Mock()
        # Simulate missing broker_url and result_backend attributes
        del app.conf.broker_url
        del app.conf.result_backend
        
        mock_control = Mock()
        mock_inspector = Mock()
        app.control = mock_control
        mock_control.broadcast.return_value = [{'worker1': 'pong'}]
        mock_control.inspect.return_value = mock_inspector
        
        # Mock inspector responses
        mock_inspector.active.return_value = {'worker1': []}
        mock_inspector.scheduled.return_value = {'worker1': []}
        mock_inspector.registered.return_value = {'worker1': ['task1']}
        mock_inspector.stats.return_value = {'worker1': {'pid': 1234, 'uptime': 3600, 'total': 100}}
        
        # Act
        result = CeleryHealthChecker.check_health_with_details(app)
        
        # Assert
        assert result["healthy"] is True
        assert result["broker_url"] == "unavailable"
        assert result["backend_url"] == "unavailable"
        assert result["registered_tasks"] == {'worker1': ['task1']}

    def test_check_health_with_details_marks_missing_expected_tasks(self, mock_celery_app):
        """Ensures missing expected tasks are reported in the result."""
        # Arrange
        mock_control = Mock()
        mock_inspector = Mock()
        mock_celery_app.control = mock_control
        
        mock_control.broadcast.return_value = [{'worker1': 'pong'}]
        mock_control.inspect.return_value = mock_inspector
        
        # Registered tasks do NOT include the expected
        mock_inspector.active.return_value = {'worker1': []}
        mock_inspector.scheduled.return_value = {'worker1': []}
        mock_inspector.registered.return_value = {
            'worker1': ['some.other_task']
        }
        mock_inspector.stats.return_value = {
            'worker1': {'pid': 1234, 'uptime': 3600, 'total': 10}
        }

        # Act
        result = CeleryHealthChecker.check_health_with_details(mock_celery_app)

        # Assert
        assert result["healthy"] is True
        assert result.get("missing_tasks") == [
            'tasks.document_tasks.process_project_documents_task'
        ]

    def test_check_health_with_details_missing_conf_object(self):
        """Test health check when conf object is missing entirely."""
        # Arrange
        app = Mock(spec=Celery)
        # Simulate missing conf attribute
        del app.conf
        
        mock_control = Mock()
        mock_inspector = Mock()
        app.control = mock_control
        mock_control.broadcast.return_value = [{'worker1': 'pong'}]
        mock_control.inspect.return_value = mock_inspector
        
        # Mock inspector responses
        mock_inspector.active.return_value = {'worker1': []}
        mock_inspector.scheduled.return_value = {'worker1': []}
        mock_inspector.registered.return_value = {'worker1': ['task1']}
        mock_inspector.stats.return_value = {'worker1': {'pid': 1234, 'uptime': 3600, 'total': 100}}
        
        # Act
        result = CeleryHealthChecker.check_health_with_details(app)
        
        # Assert
        assert result["healthy"] is True
        assert result["broker_url"] == "unavailable"
        assert result["backend_url"] == "unavailable"
        assert result["registered_tasks"] == {'worker1': ['task1']}
