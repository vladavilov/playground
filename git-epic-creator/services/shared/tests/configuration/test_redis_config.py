"""
Tests for Redis configuration settings only.
Client utilities are tested separately in test_redis_client.py
"""

import pytest
from unittest.mock import Mock
from configuration.redis_config import RedisSettings, get_redis_settings


@pytest.fixture(autouse=True)
def reset_get_redis_settings_cache():
    """Reset the cache for get_redis_settings before each test."""
    get_redis_settings.cache_clear()

class TestRedisSettings:
    """Test Redis configuration settings."""
    
    def test_redis_settings_default_values(self):
        """Test that Redis settings have proper default values."""
        settings = RedisSettings()
        
        assert settings.REDIS_URL == "redis://localhost:6379"
        assert settings.REDIS_PASSWORD is None
        assert settings.REDIS_DB == 0
        assert settings.REDIS_MAX_CONNECTIONS == 10
        assert settings.REDIS_RETRY_ON_TIMEOUT is True
        assert settings.REDIS_SOCKET_CONNECT_TIMEOUT == 5.0
        assert settings.REDIS_SOCKET_TIMEOUT == 5.0
    
    def test_redis_settings_from_env(self, monkeypatch):
        """Test that Redis settings load from environment variables."""
        monkeypatch.setenv("REDIS_URL", "redis://test-redis:6379")
        monkeypatch.setenv("REDIS_PASSWORD", "test-password")
        monkeypatch.setenv("REDIS_DB", "1")
        monkeypatch.setenv("REDIS_MAX_CONNECTIONS", "20")
        
        settings = RedisSettings()
        
        assert settings.REDIS_URL == "redis://test-redis:6379"
        assert settings.REDIS_PASSWORD == "test-password"
        assert settings.REDIS_DB == 1
        assert settings.REDIS_MAX_CONNECTIONS == 20
    
    def test_get_redis_settings_cached(self):
        """Test that get_redis_settings returns cached instance."""
        settings1 = get_redis_settings()
        settings2 = get_redis_settings()
        
        assert settings1 is settings2
    
    def test_redis_settings_with_custom_values(self, monkeypatch):
        """Test Redis settings with custom configuration values."""
        monkeypatch.setenv("REDIS_URL", "redis://custom-redis:6380")
        monkeypatch.setenv("REDIS_PASSWORD", "custom-password")
        monkeypatch.setenv("REDIS_DB", "2")
        monkeypatch.setenv("REDIS_MAX_CONNECTIONS", "50")
        monkeypatch.setenv("REDIS_RETRY_ON_TIMEOUT", "false")
        monkeypatch.setenv("REDIS_SOCKET_CONNECT_TIMEOUT", "10.0")
        monkeypatch.setenv("REDIS_SOCKET_TIMEOUT", "15.0")
        
        settings = RedisSettings()
        
        assert settings.REDIS_URL == "redis://custom-redis:6380"
        assert settings.REDIS_PASSWORD == "custom-password"
        assert settings.REDIS_DB == 2
        assert settings.REDIS_MAX_CONNECTIONS == 50
        assert settings.REDIS_RETRY_ON_TIMEOUT is False
        assert settings.REDIS_SOCKET_CONNECT_TIMEOUT == 10.0
        assert settings.REDIS_SOCKET_TIMEOUT == 15.0