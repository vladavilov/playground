"""
Tests for Celery configuration settings.
"""

import pytest
from unittest.mock import Mock
from configuration.celery_config import CelerySettings, get_celery_settings


@pytest.fixture(autouse=True)
def reset_get_celery_settings_cache():
    """Reset the cache for get_celery_settings before each test."""
    get_celery_settings.cache_clear()

class TestCelerySettings:
    """Test Celery configuration settings."""
    
    def test_celery_settings_default_values(self):
        """Test that Celery settings have proper default values."""
        settings = CelerySettings()
        
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
    
    def test_celery_settings_from_env_with_defaults(self, monkeypatch):
        """Test that Celery settings load from environment variables and defaults."""
        monkeypatch.setenv("CELERY_BROKER_URL", "redis://test-redis:6379/1")
        
        settings = CelerySettings()
        
        assert settings.CELERY_BROKER_URL == "redis://test-redis:6379/1"
        assert settings.CELERY_RESULT_BACKEND == "redis://localhost:6379/0"  # Should be default
        assert settings.CELERY_WORKER_CONCURRENCY == 4  # Should be default
        
    def test_celery_settings_type_coercion(self, monkeypatch):
        """Test that Celery settings correctly coerce types from environment variables."""
        monkeypatch.setenv("CELERY_WORKER_CONCURRENCY", "16")
        monkeypatch.setenv("CELERY_TASK_TIME_LIMIT", "7200")
        
        settings = CelerySettings()
        
        assert isinstance(settings.CELERY_WORKER_CONCURRENCY, int)
        assert settings.CELERY_WORKER_CONCURRENCY == 16
        assert isinstance(settings.CELERY_TASK_TIME_LIMIT, int)
        assert settings.CELERY_TASK_TIME_LIMIT == 7200
        
    def test_celery_settings_with_full_env_configuration(self, monkeypatch):
        """Test that Celery settings are fully configurable from environment variables."""
        env_vars = {
            "CELERY_BROKER_URL": "amqp://guest:guest@localhost:5672//",
            "CELERY_RESULT_BACKEND": "db+sqlite:///results.db",
            "CELERY_TASK_SERIALIZER": "pickle",
            "CELERY_ACCEPT_CONTENT": '["pickle", "json"]',  # Note: Pydantic handles JSON string
            "CELERY_TIMEZONE": "America/New_York",
            "CELERY_ENABLE_UTC": "False",
            "CELERY_WORKER_CONCURRENCY": "1"
        }
        for key, value in env_vars.items():
            monkeypatch.setenv(key, value)
            
        settings = CelerySettings()
        
        assert settings.CELERY_BROKER_URL == "amqp://guest:guest@localhost:5672//"
        assert settings.CELERY_RESULT_BACKEND == "db+sqlite:///results.db"
        assert settings.CELERY_TASK_SERIALIZER == "pickle"
        assert settings.CELERY_ACCEPT_CONTENT == ["pickle", "json"]
        assert settings.CELERY_TIMEZONE == "America/New_York"
        assert settings.CELERY_ENABLE_UTC is False
        assert settings.CELERY_WORKER_CONCURRENCY == 1