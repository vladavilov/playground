"""
Tests for Redis abstraction classes that unify common functionality.
Tests for BaseRedisPublisher, BaseRedisSubscriber, RedisMessage, and mixins.
"""

import pytest
import json
from unittest.mock import AsyncMock, Mock, patch
from uuid import UUID, uuid4
from datetime import datetime
from dataclasses import dataclass
from typing import Dict, Any

from utils.redis_abstractions import (
    RedisMessage,
    BaseRedisPublisher, 
    BaseRedisSubscriber,
    RedisHealthMixin,
    JSONSerializationMixin,
    RedisChannelConfig,
    MessageTypeRegistry
)


class TestRedisMessage:
    """Test the unified RedisMessage base class."""
    
    def test_redis_message_creation(self):
        """Test creating a RedisMessage with required fields."""
        message_id = uuid4()
        timestamp = datetime.now()
        
        @dataclass
        class TestMessage(RedisMessage):
            content: str
            
        message = TestMessage(
            message_id=message_id,
            timestamp=timestamp,
            content="test content"
        )
        
        assert message.message_id == message_id
        assert message.timestamp == timestamp
        assert message.content == "test content"
    
    def test_redis_message_to_dict(self):
        """Test serializing RedisMessage to dictionary."""
        message_id = uuid4()
        timestamp = datetime.now()
        
        @dataclass
        class TestMessage(RedisMessage):
            content: str
            
        message = TestMessage(
            message_id=message_id,
            timestamp=timestamp,
            content="test content"
        )
        
        result = message.to_dict()
        
        assert result["message_id"] == str(message_id)
        assert result["timestamp"] == timestamp.isoformat()
        assert result["content"] == "test content"
    
    def test_redis_message_from_dict(self):
        """Test deserializing RedisMessage from dictionary."""
        message_id = uuid4()
        timestamp = datetime.now()
        
        @dataclass
        class TestMessage(RedisMessage):
            content: str
            
        data = {
            "message_id": str(message_id),
            "timestamp": timestamp.isoformat(),
            "content": "test content"
        }
        
        message = TestMessage.from_dict(data)
        
        assert message.message_id == message_id
        assert message.timestamp == timestamp
        assert message.content == "test content"


class TestRedisHealthMixin:
    """Test the RedisHealthMixin for connection health checking."""
    
    @pytest.fixture
    def mock_redis_client(self):
        """Mock Redis client for testing."""
        return AsyncMock()
    
    @pytest.fixture
    def health_checker(self, mock_redis_client):
        """Create health checker with mocked Redis client."""
        class TestHealthChecker(RedisHealthMixin):
            def __init__(self, redis_client):
                self.redis_client = redis_client
        
        return TestHealthChecker(mock_redis_client)
    
    @pytest.mark.asyncio
    async def test_check_connection_success(self, health_checker, mock_redis_client):
        """Test successful Redis connection check."""
        mock_redis_client.ping.return_value = True
        
        result = await health_checker.check_connection()
        
        assert result is True
        mock_redis_client.ping.assert_called_once()
    
    @pytest.mark.asyncio
    async def test_check_connection_failure(self, health_checker, mock_redis_client):
        """Test failed Redis connection check."""
        mock_redis_client.ping.side_effect = Exception("Connection failed")
        
        result = await health_checker.check_connection()
        
        assert result is False
        mock_redis_client.ping.assert_called_once()

    @pytest.mark.asyncio
    async def test_check_health_success(self, health_checker, mock_redis_client):
        """Test successful Redis health check returning dict."""
        mock_redis_client.ping.return_value = True
        
        result = await health_checker.check_health()
        
        assert result["healthy"] is True
        mock_redis_client.ping.assert_called_once()

    @pytest.mark.asyncio
    async def test_check_health_failure(self, health_checker, mock_redis_client):
        """Test failed Redis health check returning dict."""
        mock_redis_client.ping.side_effect = Exception("Connection failed")
        
        result = await health_checker.check_health()
        
        assert result["healthy"] is False
        assert "Connection failed" in result["error"]
        mock_redis_client.ping.assert_called_once()

    @pytest.mark.asyncio
    async def test_check_health_with_details_success(self, health_checker, mock_redis_client):
        """Test detailed Redis health check success."""
        mock_redis_client.ping.return_value = True
        mock_info = {
            "redis_version": "6.2.5",
            "connected_clients": 5,
            "used_memory": 1024000,
            "uptime_in_seconds": 3600
        }
        mock_redis_client.info.return_value = mock_info
        
        result = await health_checker.check_health_with_details()
        
        assert result["healthy"] is True
        assert result["ping"] is True
        assert result["version"] == "6.2.5"
        assert result["connected_clients"] == 5
        assert result["used_memory"] == 1024000
        assert result["uptime_in_seconds"] == 3600
        mock_redis_client.ping.assert_called_once()
        mock_redis_client.info.assert_called_once()

    @pytest.mark.asyncio
    async def test_check_health_with_details_failure(self, health_checker, mock_redis_client):
        """Test detailed Redis health check failure."""
        mock_redis_client.ping.side_effect = Exception("Connection failed")
        
        result = await health_checker.check_health_with_details()
        
        assert result["healthy"] is False
        assert "Connection failed" in result["error"]
        assert result["ping"] is False
        mock_redis_client.ping.assert_called_once()

    @pytest.mark.asyncio
    async def test_check_health_with_details_info_failure(self, health_checker, mock_redis_client):
        """Test detailed Redis health check when info() fails but ping succeeds."""
        mock_redis_client.ping.return_value = True
        mock_redis_client.info.side_effect = Exception("Info command failed")
        
        result = await health_checker.check_health_with_details()
        
        assert result["healthy"] is False
        assert "Info command failed" in result["error"]
        assert result["ping"] is True
        mock_redis_client.ping.assert_called_once()
        mock_redis_client.info.assert_called_once()


class TestJSONSerializationMixin:
    """Test the JSONSerializationMixin for message serialization."""
    
    @pytest.fixture
    def serializer(self):
        """Create serializer instance."""
        class TestSerializer(JSONSerializationMixin):
            pass
        
        return TestSerializer()
    
    def test_serialize_message_success(self, serializer):
        """Test successful message serialization."""
        @dataclass
        class TestMessage(RedisMessage):
            content: str
            
        message = TestMessage(
            message_id=uuid4(),
            timestamp=datetime.now(),
            content="test"
        )
        
        result = serializer.serialize_message(message)
        
        assert isinstance(result, str)
        data = json.loads(result)
        assert data["content"] == "test"
    
    def test_deserialize_message_success(self, serializer):
        """Test successful message deserialization."""
        message_id = uuid4()
        timestamp = datetime.now()
        
        @dataclass
        class TestMessage(RedisMessage):
            content: str
        
        data = {
            "message_id": str(message_id),
            "timestamp": timestamp.isoformat(),
            "content": "test"
        }
        
        json_data = json.dumps(data)
        result = serializer.deserialize_message(json_data, TestMessage)
        
        assert isinstance(result, TestMessage)
        assert result.content == "test"
        assert result.message_id == message_id


class TestRedisChannelConfig:
    """Test the RedisChannelConfig for channel naming strategies."""
    
    def test_get_channel_name_with_prefix(self):
        """Test channel name generation with prefix."""
        config = RedisChannelConfig(
            channel_prefix="test_prefix",
            channel_separator=":"
        )
        
        result = config.get_channel_name("my_channel")
        
        assert result == "test_prefix:my_channel"
    
    def test_get_project_channel_name(self):
        """Test project-specific channel name generation."""
        config = RedisChannelConfig(
            channel_prefix="project_updates",
            channel_separator=":"
        )
        
        project_id = uuid4()
        result = config.get_project_channel_name(project_id)
        
        assert result == f"project_updates:{project_id}"


class TestBaseRedisPublisher:
    """Test the BaseRedisPublisher abstract class."""
    
    @pytest.fixture
    def mock_redis_client(self):
        """Mock Redis client for testing."""
        return AsyncMock()
    
    @pytest.fixture
    def mock_channel_config(self):
        """Mock channel configuration."""
        config = Mock()
        config.get_channel_name.return_value = "test_channel"
        return config
    
    @pytest.fixture
    def publisher(self, mock_redis_client, mock_channel_config):
        """Create publisher with mocked dependencies."""
        class TestPublisher(BaseRedisPublisher):
            def get_default_channel(self) -> str:
                return "test_channel"
        
        return TestPublisher(
            redis_client=mock_redis_client,
            channel_config=mock_channel_config
        )
    
    @pytest.mark.asyncio
    async def test_publish_message_success(self, publisher, mock_redis_client):
        """Test successful message publishing."""
        @dataclass
        class TestMessage(RedisMessage):
            content: str
            
        message = TestMessage(
            message_id=uuid4(),
            timestamp=datetime.now(),
            content="test"
        )
        
        mock_redis_client.publish.return_value = 1
        
        result = await publisher.publish_message(message, "test_channel")
        
        assert result is True
        mock_redis_client.publish.assert_called_once()
    
    @pytest.mark.asyncio
    async def test_publish_message_failure(self, publisher, mock_redis_client):
        """Test failed message publishing."""
        @dataclass
        class TestMessage(RedisMessage):
            content: str
            
        message = TestMessage(
            message_id=uuid4(),
            timestamp=datetime.now(),
            content="test"
        )
        
        mock_redis_client.publish.side_effect = Exception("Publish failed")
        
        result = await publisher.publish_message(message, "test_channel")
        
        assert result is False


class TestMessageTypeRegistry:
    """Test the MessageTypeRegistry for message routing."""
    
    def test_register_message_type(self):
        """Test registering a message type."""
        registry = MessageTypeRegistry()
        
        @dataclass
        class TestMessage(RedisMessage):
            content: str
        
        registry.register_message_type("test_message", TestMessage)
        
        assert registry.get_message_class("test_message") == TestMessage
    
    def test_get_unknown_message_type(self):
        """Test getting unknown message type raises error."""
        registry = MessageTypeRegistry()
        
        with pytest.raises(ValueError, match="Unknown message type"):
            registry.get_message_class("unknown_type") 