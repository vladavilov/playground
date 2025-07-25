"""
Tests for Celery configuration settings.
"""

import pytest
import os
from unittest.mock import patch
from configuration.celery_config import CelerySettings, get_celery_settings


class TestCelerySettings:
    """Test Celery configuration settings."""

    @pytest.fixture(autouse=True)
    def reset_cache_and_env(self):
        """Reset cache and clean environment before each test."""
        get_celery_settings.cache_clear()
        # Store original env vars to restore later
        original_env = {}
        celery_env_vars = [
            'CELERY_BROKER_URL', 'CELERY_RESULT_BACKEND', 'CELERY_TASK_SERIALIZER',
            'CELERY_RESULT_SERIALIZER', 'CELERY_TIMEZONE', 'CELERY_ENABLE_UTC',
            'CELERY_TASK_TRACK_STARTED', 'CELERY_TASK_TIME_LIMIT', 'CELERY_TASK_SOFT_TIME_LIMIT',
            'CELERY_WORKER_PREFETCH_MULTIPLIER', 'CELERY_WORKER_MAX_TASKS_PER_CHILD',
            'CELERY_WORKER_CONCURRENCY', 'REDIS_URL'
        ]
        
        for var in celery_env_vars:
            if var in os.environ:
                original_env[var] = os.environ[var]
                del os.environ[var]
        
        yield
        
        # Restore original environment
        for var, value in original_env.items():
            os.environ[var] = value
    
    def test_celery_settings_default_values(self):
        """Test that Celery settings have proper default values."""
        # Act
        settings = CelerySettings()
        
        # Assert
        assert settings.CELERY_BROKER_URL == "redis://localhost:6379/0"
        assert settings.CELERY_RESULT_BACKEND == "redis://localhost:6379/0"
        assert settings.CELERY_TASK_SERIALIZER == "json"
        assert settings.CELERY_RESULT_SERIALIZER == "json"
        assert settings.CELERY_ACCEPT_CONTENT == ["json"]
        assert settings.CELERY_TIMEZONE == "UTC"
        assert settings.CELERY_ENABLE_UTC is True
        assert settings.CELERY_TASK_TRACK_STARTED is True
        assert settings.CELERY_TASK_TIME_LIMIT == 300
        assert settings.CELERY_TASK_SOFT_TIME_LIMIT == 240
        assert settings.CELERY_WORKER_PREFETCH_MULTIPLIER == 1
        assert settings.CELERY_WORKER_MAX_TASKS_PER_CHILD == 1000
        assert settings.CELERY_WORKER_CONCURRENCY == 4
        assert settings.CELERY_TASK_ROUTES == {
            'tasks.document_tasks.*': {'queue': 'document_processing'},
            'tasks.project_tasks.*': {'queue': 'project_management'},
        }
    
    def test_celery_settings_from_environment_variables(self):
        """Test that Celery settings load from environment variables."""
        # Arrange
        env_vars = {
            "CELERY_BROKER_URL": "redis://test-redis:6379/1",
            "CELERY_RESULT_BACKEND": "redis://test-redis:6379/2",
            "CELERY_TASK_SERIALIZER": "pickle",
            "CELERY_WORKER_CONCURRENCY": "8"
        }
        
        with patch.dict(os.environ, env_vars):
            # Act
            settings = CelerySettings()
            
            # Assert
            assert settings.CELERY_BROKER_URL == "redis://test-redis:6379/1"
            assert settings.CELERY_RESULT_BACKEND == "redis://test-redis:6379/2"
            assert settings.CELERY_TASK_SERIALIZER == "pickle"
            assert settings.CELERY_WORKER_CONCURRENCY == 8
            # Verify defaults are still used for non-overridden values
            assert settings.CELERY_TIMEZONE == "UTC"
        
    def test_celery_settings_type_coercion(self):
        """Test that Celery settings correctly coerce types from environment variables."""
        # Arrange
        env_vars = {
            "CELERY_WORKER_CONCURRENCY": "16",
            "CELERY_TASK_TIME_LIMIT": "7200",
            "CELERY_ENABLE_UTC": "False",
            "CELERY_TASK_TRACK_STARTED": "false"
        }
        
        with patch.dict(os.environ, env_vars):
            # Act
            settings = CelerySettings()
            
            # Assert
            assert isinstance(settings.CELERY_WORKER_CONCURRENCY, int)
            assert settings.CELERY_WORKER_CONCURRENCY == 16
            assert isinstance(settings.CELERY_TASK_TIME_LIMIT, int)
            assert settings.CELERY_TASK_TIME_LIMIT == 7200
            assert isinstance(settings.CELERY_ENABLE_UTC, bool)
            assert settings.CELERY_ENABLE_UTC is False
            assert isinstance(settings.CELERY_TASK_TRACK_STARTED, bool)
            assert settings.CELERY_TASK_TRACK_STARTED is False
        
    def test_celery_settings_explicit_broker_url(self):
        """Test that explicit CELERY_BROKER_URL is used when provided."""
        # Arrange
        env_vars = {
            "CELERY_BROKER_URL": "redis://explicit-broker:6379/1"
        }
        
        with patch.dict(os.environ, env_vars):
            # Act
            settings = CelerySettings()
            
            # Assert
            assert settings.CELERY_BROKER_URL == "redis://explicit-broker:6379/1"

    def test_celery_settings_explicit_result_backend(self):
        """Test that explicit CELERY_RESULT_BACKEND is used when provided."""
        # Arrange
        env_vars = {
            "CELERY_RESULT_BACKEND": "redis://explicit-backend:6379/2"
        }
        
        with patch.dict(os.environ, env_vars):
            # Act
            settings = CelerySettings()
            
            # Assert
            assert settings.CELERY_RESULT_BACKEND == "redis://explicit-backend:6379/2"

    def test_celery_settings_accept_content_list_handling(self):
        """Test that CELERY_ACCEPT_CONTENT properly handles list values."""
        # Arrange
        with patch.dict(os.environ, {"CELERY_ACCEPT_CONTENT": '["pickle", "json", "yaml"]'}):
            # Act
            settings = CelerySettings()
            
            # Assert
            assert isinstance(settings.CELERY_ACCEPT_CONTENT, list)
            assert settings.CELERY_ACCEPT_CONTENT == ["pickle", "json", "yaml"]

    def test_get_celery_settings_caching(self):
        """Test that get_celery_settings returns cached instance."""
        # Act
        settings1 = get_celery_settings()
        settings2 = get_celery_settings()
        
        # Assert
        assert settings1 is settings2
        assert isinstance(settings1, CelerySettings)

    def test_get_celery_settings_cache_clear(self):
        """Test that cache can be cleared and new instance is created."""
        # Arrange
        settings1 = get_celery_settings()
        
        # Act
        get_celery_settings.cache_clear()
        settings2 = get_celery_settings()
        
        # Assert
        assert settings1 is not settings2
        assert isinstance(settings2, CelerySettings)

    @pytest.mark.parametrize("timezone,expected", [
        ("UTC", "UTC"),
        ("America/New_York", "America/New_York"),
        ("Europe/London", "Europe/London"),
        ("Asia/Tokyo", "Asia/Tokyo"),
    ])
    def test_celery_settings_timezone_values(self, timezone, expected):
        """Test various timezone configurations."""
        # Arrange
        with patch.dict(os.environ, {"CELERY_TIMEZONE": timezone}):
            # Act
            settings = CelerySettings()
            
            # Assert
            assert settings.CELERY_TIMEZONE == expected

    def test_celery_settings_task_routes_structure(self):
        """Test that task routes have the correct structure."""
        # Act
        settings = CelerySettings()
        
        # Assert
        assert isinstance(settings.CELERY_TASK_ROUTES, dict)
        assert 'tasks.document_tasks.*' in settings.CELERY_TASK_ROUTES
        assert 'tasks.project_tasks.*' in settings.CELERY_TASK_ROUTES
        assert settings.CELERY_TASK_ROUTES['tasks.document_tasks.*']['queue'] == 'document_processing'
        assert settings.CELERY_TASK_ROUTES['tasks.project_tasks.*']['queue'] == 'project_management'