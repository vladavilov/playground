"""
Tests for Redis client utilities following SOLID principles.
"""

from unittest.mock import Mock, patch, AsyncMock
import pytest
import redis.asyncio as redis
from utils.redis_client import (
    RedisClientFactory,
    RedisHealthChecker, 
    get_redis_client, 
    check_redis_health,
    RedisClient
)
from configuration.common_config import AppSettings
from configuration.redis_config import RedisSettings


class TestRedisClientFactory:
    """Test cases for RedisClientFactory."""

    @pytest.fixture
    def mock_redis_settings(self):
        """Mock Redis settings."""
        settings = Mock(spec=RedisSettings)
        settings.REDIS_URL = "redis://localhost:6379"
        settings.REDIS_PASSWORD = None
        settings.REDIS_DB = 0
        settings.REDIS_MAX_CONNECTIONS = 10
        settings.REDIS_RETRY_ON_TIMEOUT = True
        settings.REDIS_SOCKET_CONNECT_TIMEOUT = 5.0
        settings.REDIS_SOCKET_TIMEOUT = 5.0
        return settings

    @patch('utils.redis_client.redis.from_url')
    def test_create_client(self, mock_from_url, mock_redis_settings):
        """Test Redis client creation from settings."""
        # Arrange
        mock_redis_client = Mock(spec=redis.Redis)
        mock_from_url.return_value = mock_redis_client
        
        # Act
        result = RedisClientFactory.create_client(mock_redis_settings)
        
        # Assert
        mock_from_url.assert_called_once_with(
            mock_redis_settings.REDIS_URL,
            password=mock_redis_settings.REDIS_PASSWORD,
            db=mock_redis_settings.REDIS_DB,
            max_connections=mock_redis_settings.REDIS_MAX_CONNECTIONS,
            retry_on_timeout=mock_redis_settings.REDIS_RETRY_ON_TIMEOUT,
            socket_connect_timeout=mock_redis_settings.REDIS_SOCKET_CONNECT_TIMEOUT,
            socket_timeout=mock_redis_settings.REDIS_SOCKET_TIMEOUT,
            decode_responses=True
        )
        assert result == mock_redis_client

    @patch('utils.redis_client.redis.from_url')
    def test_create_client_with_password(self, mock_from_url, mock_redis_settings):
        """Test Redis client creation with password."""
        # Arrange
        mock_redis_settings.REDIS_PASSWORD = "test-password"
        mock_redis_client = Mock(spec=redis.Redis)
        mock_from_url.return_value = mock_redis_client
        
        # Act
        result = RedisClientFactory.create_client(mock_redis_settings)
        
        # Assert
        mock_from_url.assert_called_once_with(
            mock_redis_settings.REDIS_URL,
            password="test-password",
            db=mock_redis_settings.REDIS_DB,
            max_connections=mock_redis_settings.REDIS_MAX_CONNECTIONS,
            retry_on_timeout=mock_redis_settings.REDIS_RETRY_ON_TIMEOUT,
            socket_connect_timeout=mock_redis_settings.REDIS_SOCKET_CONNECT_TIMEOUT,
            socket_timeout=mock_redis_settings.REDIS_SOCKET_TIMEOUT,
            decode_responses=True
        )
        assert result == mock_redis_client


class TestRedisClient:
    """Test cases for RedisClient."""

    @pytest.fixture
    def mock_redis_settings(self):
        """Mock Redis settings."""
        settings = Mock(spec=RedisSettings)
        settings.REDIS_URL = "redis://localhost:6379"
        settings.REDIS_PASSWORD = None
        settings.REDIS_DB = 0
        settings.REDIS_MAX_CONNECTIONS = 10
        settings.REDIS_RETRY_ON_TIMEOUT = True
        settings.REDIS_SOCKET_CONNECT_TIMEOUT = 5.0
        settings.REDIS_SOCKET_TIMEOUT = 5.0
        return settings

    @patch('utils.redis_client.RedisClientFactory.create_client')
    def test_redis_client_initialization(self, mock_create_client, mock_redis_settings):
        """Test RedisClient initialization."""
        # Arrange
        mock_redis_instance = Mock(spec=redis.Redis)
        mock_create_client.return_value = mock_redis_instance
        
        # Act
        client = RedisClient(mock_redis_settings)
        
        # Assert
        assert client.settings == mock_redis_settings
        assert client.client == mock_redis_instance
        mock_create_client.assert_called_once_with(mock_redis_settings)

    @patch('utils.redis_client.RedisClientFactory.create_client')
    def test_get_client(self, mock_create_client, mock_redis_settings):
        """Test get_client method returns the Redis client instance."""
        # Arrange
        mock_redis_instance = Mock(spec=redis.Redis)
        mock_create_client.return_value = mock_redis_instance
        client = RedisClient(mock_redis_settings)
        
        # Act
        result = client.get_client()
        
        # Assert
        assert result == mock_redis_instance

    @patch('utils.redis_client.RedisClientFactory.create_client')
    @pytest.mark.asyncio
    async def test_close(self, mock_create_client, mock_redis_settings):
        """Test close method calls Redis client close."""
        # Arrange
        mock_redis_instance = AsyncMock(spec=redis.Redis)
        mock_create_client.return_value = mock_redis_instance
        client = RedisClient(mock_redis_settings)
        
        # Act
        await client.close()
        
        # Assert
        mock_redis_instance.close.assert_awaited_once()

    @patch('utils.redis_client.RedisClientFactory.create_client')
    @pytest.mark.asyncio
    async def test_async_context_manager(self, mock_create_client, mock_redis_settings):
        """Test RedisClient as async context manager."""
        # Arrange
        mock_redis_instance = AsyncMock(spec=redis.Redis)
        mock_create_client.return_value = mock_redis_instance
        client = RedisClient(mock_redis_settings)
        
        # Act
        async with client as redis_client:
            result = redis_client
        
        # Assert
        assert result == mock_redis_instance
        mock_redis_instance.close.assert_awaited_once()


class TestRedisHealthChecker:
    """Test cases for RedisHealthChecker."""

    @pytest.fixture
    def mock_app_settings(self):
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

    @pytest.mark.asyncio
    async def test_check_health_success(self):
        """Test successful Redis health check."""
        # Arrange
        mock_client = AsyncMock(spec=redis.Redis)
        mock_client.ping = AsyncMock(return_value=True)

        # Act
        result = await RedisHealthChecker.check_health(mock_client)

        # Assert
        assert result["healthy"] is True
        mock_client.ping.assert_called_once()

    @pytest.mark.asyncio
    async def test_check_health_failure(self):
        """Test Redis health check failure."""
        # Arrange
        mock_client = AsyncMock(spec=redis.Redis)
        mock_client.ping = AsyncMock(side_effect=Exception("Connection failed"))

        # Act
        result = await RedisHealthChecker.check_health(mock_client)

        # Assert
        assert result["healthy"] is False
        assert "Connection failed" in result["error"]
        mock_client.ping.assert_called_once()

    @pytest.mark.asyncio
    async def test_check_health_with_details_success(self):
        """Test detailed Redis health check success."""
        # Arrange
        mock_client = AsyncMock(spec=redis.Redis)
        mock_client.ping = AsyncMock(return_value=True)
        mock_info = {
            "redis_version": "6.2.5",
            "connected_clients": 5,
            "used_memory": 1024000,
            "uptime_in_seconds": 3600
        }
        mock_client.info = AsyncMock(return_value=mock_info)

        # Act
        result = await RedisHealthChecker.check_health_with_details(mock_client)

        # Assert
        assert result["healthy"] is True
        assert result["version"] == "6.2.5"
        assert result["connected_clients"] == 5
        assert result["used_memory"] == 1024000
        assert result["uptime_in_seconds"] == 3600
        mock_client.ping.assert_called_once()
        mock_client.info.assert_called_once()

    @pytest.mark.asyncio
    async def test_check_health_with_details_failure(self):
        """Test detailed Redis health check failure."""
        # Arrange
        mock_client = AsyncMock(spec=redis.Redis)
        mock_client.ping = AsyncMock(side_effect=Exception("Connection failed"))

        # Act
        result = await RedisHealthChecker.check_health_with_details(mock_client)

        # Assert
        assert result["healthy"] is False
        assert "Connection failed" in result["error"]
        assert result["ping"] is False
        mock_client.ping.assert_called_once()

    @pytest.mark.asyncio
    async def test_check_health_with_details_info_failure(self):
        """Test detailed Redis health check when info() fails but ping succeeds."""
        # Arrange
        mock_client = AsyncMock(spec=redis.Redis)
        mock_client.ping = AsyncMock(return_value=True)
        mock_client.info = AsyncMock(side_effect=Exception("Info command failed"))

        # Act
        result = await RedisHealthChecker.check_health_with_details(mock_client)

        # Assert
        assert result["healthy"] is False
        assert "Info command failed" in result["error"]
        mock_client.ping.assert_called_once()
        mock_client.info.assert_called_once()


class TestRedisClientIntegration:
    """Integration tests for Redis client functions."""

    @patch('utils.redis_client.get_app_settings')
    @patch('utils.redis_client.RedisClient')
    def test_get_redis_client_uses_factory(self, mock_redis_client_class, mock_get_settings):
        """Test that get_redis_client uses the factory pattern."""
        # Arrange
        mock_app_settings = Mock()
        mock_app_settings.redis = Mock()
        mock_client_instance = Mock(spec=RedisClient)
        mock_redis_instance = Mock(spec=redis.Redis)
        mock_client_instance.get_client.return_value = mock_redis_instance
        mock_redis_client_class.return_value = mock_client_instance
        mock_get_settings.return_value = mock_app_settings
        
        get_redis_client.cache_clear()
        
        # Act
        client = get_redis_client()
        
        # Assert
        mock_get_settings.assert_called_once()
        mock_redis_client_class.assert_called_once_with(mock_app_settings.redis)
        assert client == mock_redis_instance

    @patch('utils.redis_client.get_app_settings')
    @patch('utils.redis_client.RedisClient')
    def test_get_redis_client_cached(self, mock_redis_client_class, mock_get_settings):
        """Test that get_redis_client returns cached instance."""
        # Arrange
        mock_app_settings = Mock()
        mock_app_settings.redis = Mock()
        mock_client_instance = Mock(spec=RedisClient)
        mock_redis_instance = Mock(spec=redis.Redis)
        mock_client_instance.get_client.return_value = mock_redis_instance
        mock_redis_client_class.return_value = mock_client_instance
        mock_get_settings.return_value = mock_app_settings
        
        get_redis_client.cache_clear()
        
        # Act
        client1 = get_redis_client()
        client2 = get_redis_client()
        
        # Assert
        mock_redis_client_class.assert_called_once_with(mock_app_settings.redis)
        assert client1 is client2

    @pytest.mark.asyncio
    async def test_check_redis_health_convenience_function(self):
        """Test the health check convenience function."""
        # Arrange
        mock_client = AsyncMock(spec=redis.Redis)

        with patch('utils.redis_client.RedisHealthChecker.check_health', new_callable=AsyncMock) as mock_check_health:
            mock_check_health.return_value = {"healthy": True}

            # Act
            result = await check_redis_health(mock_client)

            # Assert
            assert result is True
            mock_check_health.assert_called_once_with(mock_client)

    @pytest.mark.asyncio
    async def test_check_redis_health_convenience_function_failure(self):
        """Test the health check convenience function with failure."""
        # Arrange
        mock_client = AsyncMock(spec=redis.Redis)

        with patch('utils.redis_client.RedisHealthChecker.check_health', new_callable=AsyncMock) as mock_check_health:
            mock_check_health.return_value = {"healthy": False, "error": "Connection failed"}

            # Act
            result = await check_redis_health(mock_client)

            # Assert
            assert result is False
            mock_check_health.assert_called_once_with(mock_client)