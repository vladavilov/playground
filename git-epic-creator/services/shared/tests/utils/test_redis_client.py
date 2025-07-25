"""
Tests for Redis client utilities following SOLID principles.
"""

import pytest
from unittest.mock import Mock, patch, AsyncMock, MagicMock
import redis.asyncio as redis
from utils.redis_client import (
    RedisHealthChecker, 
    get_redis_client, 
    check_redis_health,
    RedisClient
)
from configuration.common_config import AppSettings
from configuration.redis_config import RedisSettings

@pytest.fixture
def mock_app_settings():
    """Mock application settings."""
    settings = Mock(spec=AppSettings)
    settings.redis = Mock(spec=RedisSettings)
    settings.redis.REDIS_URL = "redis://localhost:6379"
    settings.redis.REDIS_PASSWORD = None
    settings.redis.REDIS_DB = 0
    settings.redis.REDIS_MAX_CONNECTIONS = 10
    settings.redis.REDIS_RETRY_ON_TIMEOUT = True
    settings.redis.REDIS_SOCKET_CONNECT_TIMEOUT = 5.0
    settings.redis.REDIS_SOCKET_TIMEOUT = 5.0
    return settings

@patch('utils.redis_client.RedisClient')
@patch('utils.redis_client.get_app_settings')
def test_get_redis_client_uses_factory(mock_get_settings, mock_redis_client_class, mock_app_settings):
    """Test that get_redis_client uses the factory pattern."""
    mock_client_instance = Mock(spec=RedisClient)
    mock_redis_client_class.return_value = mock_client_instance
    mock_get_settings.return_value = mock_app_settings
    
    get_redis_client.cache_clear()
    
    client = get_redis_client()
    
    mock_get_settings.assert_called_once()
    mock_redis_client_class.assert_called_once_with(mock_app_settings.redis)
    assert client == mock_client_instance.get_client()

@patch('utils.redis_client.RedisClient')
@patch('utils.redis_client.get_app_settings')
def test_get_redis_client_cached(mock_get_settings, mock_redis_client_class, mock_app_settings):
    """Test that get_redis_client returns cached instance."""
    mock_client_instance = Mock(spec=RedisClient)
    mock_redis_client_class.return_value = mock_client_instance
    mock_get_settings.return_value = mock_app_settings
    
    get_redis_client.cache_clear()
    
    client1 = get_redis_client()
    client2 = get_redis_client()
    
    mock_redis_client_class.assert_called_once_with(mock_app_settings.redis)
    assert client1 is client2

@pytest.mark.asyncio
async def test_check_health_success():
    """Test successful Redis health check."""
    mock_client = MagicMock()
    mock_client.ping = AsyncMock(return_value=True)

    result = await RedisHealthChecker.check_health(mock_client)

    assert result["healthy"] is True
    mock_client.ping.assert_awaited_once()

@pytest.mark.asyncio
async def test_check_health_failure():
    """Test Redis health check failure."""
    mock_client = MagicMock()
    mock_client.ping = AsyncMock(side_effect=Exception("Connection failed"))

    result = await RedisHealthChecker.check_health(mock_client)

    assert result["healthy"] is False
    mock_client.ping.assert_awaited_once()

@pytest.mark.asyncio
async def test_check_health_with_details_success():
    """Test detailed Redis health check success."""
    mock_client = MagicMock()
    mock_client.ping = AsyncMock(return_value=True)
    mock_client.info = AsyncMock(return_value={"redis_version": "6.2.5"})

    result = await RedisHealthChecker.check_health_with_details(mock_client)

    assert result["healthy"] is True
    mock_client.ping.assert_awaited_once()
    mock_client.info.assert_awaited_once()

@pytest.mark.asyncio
async def test_check_health_with_details_failure():
    """Test detailed Redis health check failure."""
    mock_client = MagicMock()
    mock_client.ping = AsyncMock(side_effect=Exception("Connection failed"))

    result = await RedisHealthChecker.check_health_with_details(mock_client)

    assert result["healthy"] is False
    assert "error" in result
    mock_client.ping.assert_awaited_once()

@patch('utils.redis_client.get_app_settings')
@patch('utils.redis_client.RedisClient')
def test_full_client_creation_flow(mock_redis_client_class, mock_get_settings, mock_app_settings):
    """Test the complete client creation flow."""
    mock_get_settings.return_value = mock_app_settings
    mock_client_instance = Mock(spec=RedisClient)
    mock_redis_client_class.return_value = mock_client_instance
    
    get_redis_client.cache_clear()
    
    client = get_redis_client()
    
    mock_get_settings.assert_called_once()
    mock_redis_client_class.assert_called_once_with(mock_app_settings.redis)
    assert client == mock_client_instance.get_client()

@pytest.mark.asyncio
async def test_health_check_convenience_function():
    """Test the health check convenience function."""
    mock_client = AsyncMock(spec=redis.Redis)

    with patch('utils.redis_client.RedisHealthChecker.check_health', new_callable=AsyncMock) as mock_check_health:
        mock_check_health.return_value = {"healthy": True}

        result = await check_redis_health(mock_client)

        assert result is True
        mock_check_health.assert_called_once_with(mock_client)

@patch('utils.redis_client.redis.from_url')
async def test_client_closure(mock_from_url):
    """Test that the Redis client's close method is called."""
    mock_redis_client = AsyncMock(spec=redis.Redis)
    mock_from_url.return_value = mock_redis_client

    settings = Mock(spec=RedisSettings)
    settings.REDIS_URL = "redis://localhost:6379"
    settings.REDIS_PASSWORD = None
    settings.REDIS_DB = 0
    settings.REDIS_MAX_CONNECTIONS = 10
    settings.REDIS_RETRY_ON_TIMEOUT = True
    settings.REDIS_SOCKET_CONNECT_TIMEOUT = 5.0
    settings.REDIS_SOCKET_TIMEOUT = 5.0
    
    client = RedisClient(settings)
    await client.close()

    mock_redis_client.close.assert_awaited()