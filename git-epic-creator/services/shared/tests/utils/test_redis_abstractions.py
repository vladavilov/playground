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
from enum import Enum

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
            
            def get_default_stream(self) -> str:
                return "test_stream"
        
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


class TestRedisMode:
    """Test the RedisMode enum for switching between pub/sub and streams."""
    
    def test_redis_mode_enum_values(self):
        """Test RedisMode enum has correct values."""
        from utils.redis_abstractions import RedisMode
        
        assert RedisMode.PUB_SUB.value == "pub_sub"
        assert RedisMode.STREAMS.value == "streams"
    
    def test_redis_mode_default(self):
        """Test RedisMode has proper default value."""
        from utils.redis_abstractions import RedisMode
        
        # Should have pub_sub as default for backward compatibility
        assert hasattr(RedisMode, 'PUB_SUB')
        assert hasattr(RedisMode, 'STREAMS')


class TestBaseRedisPublisherWithModes:
    """Test BaseRedisPublisher with pub/sub and streams modes."""
    
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
    def pubsub_publisher(self, mock_redis_client, mock_channel_config):
        """Create publisher in pub/sub mode."""
        from utils.redis_abstractions import RedisMode
        
        class TestPublisher(BaseRedisPublisher):
            def get_default_channel(self) -> str:
                return "test_channel"
            
            def get_default_stream(self) -> str:
                return "test_stream"
        
        return TestPublisher(
            redis_client=mock_redis_client,
            channel_config=mock_channel_config,
            mode=RedisMode.PUB_SUB
        )
    
    @pytest.fixture
    def streams_publisher(self, mock_redis_client, mock_channel_config):
        """Create publisher in streams mode."""
        from utils.redis_abstractions import RedisMode
        
        class TestPublisher(BaseRedisPublisher):
            def get_default_channel(self) -> str:
                return "test_channel"
            
            def get_default_stream(self) -> str:
                return "test_stream"
        
        return TestPublisher(
            redis_client=mock_redis_client,
            channel_config=mock_channel_config,
            mode=RedisMode.STREAMS
        )
    
    @pytest.mark.asyncio
    async def test_publish_message_pubsub_mode(self, pubsub_publisher, mock_redis_client):
        """Test publishing message in pub/sub mode."""
        @dataclass
        class TestMessage(RedisMessage):
            content: str
            
        message = TestMessage(
            message_id=uuid4(),
            timestamp=datetime.now(),
            content="test"
        )
        
        mock_redis_client.publish.return_value = 1
        
        result = await pubsub_publisher.publish_message(message)
        
        assert result is True
        mock_redis_client.publish.assert_called_once()
        # Should NOT call xadd for pub/sub mode
        mock_redis_client.xadd.assert_not_called()
    
    @pytest.mark.asyncio
    async def test_publish_message_streams_mode(self, streams_publisher, mock_redis_client):
        """Test publishing message in streams mode."""
        @dataclass
        class TestMessage(RedisMessage):
            content: str
            
        message = TestMessage(
            message_id=uuid4(),
            timestamp=datetime.now(),
            content="test"
        )
        
        mock_redis_client.xadd.return_value = "1640000000000-0"
        
        result = await streams_publisher.publish_message(message)
        
        assert result is True
        mock_redis_client.xadd.assert_called_once()
        # Should NOT call publish for streams mode
        mock_redis_client.publish.assert_not_called()
    
    @pytest.mark.asyncio
    async def test_publish_to_stream_explicit(self, streams_publisher, mock_redis_client):
        """Test explicit stream publishing method."""
        @dataclass
        class TestMessage(RedisMessage):
            content: str
            
        message = TestMessage(
            message_id=uuid4(),
            timestamp=datetime.now(),
            content="test"
        )
        
        mock_redis_client.xadd.return_value = "1640000000000-0"
        
        result = await streams_publisher.publish_to_stream(message, "custom_stream")
        
        assert result is True
        mock_redis_client.xadd.assert_called_once()
        call_args = mock_redis_client.xadd.call_args
        assert call_args[0][0] == "custom_stream"  # Stream name
        assert "content" in call_args[0][1]  # Message data
    
    @pytest.mark.asyncio
    async def test_stream_publish_failure(self, streams_publisher, mock_redis_client):
        """Test stream publishing failure handling."""
        @dataclass
        class TestMessage(RedisMessage):
            content: str
            
        message = TestMessage(
            message_id=uuid4(),
            timestamp=datetime.now(),
            content="test"
        )
        
        mock_redis_client.xadd.side_effect = Exception("Stream error")
        
        result = await streams_publisher.publish_message(message)
        
        assert result is False


class TestBaseRedisSubscriberWithModes:
    """Test BaseRedisSubscriber with pub/sub and streams modes."""
    
    @pytest.fixture
    def mock_redis_client(self):
        """Mock Redis client for testing."""
        return AsyncMock()
    
    @pytest.fixture
    def mock_channel_config(self):
        """Mock channel configuration."""
        return Mock()
    
    @pytest.fixture
    def mock_message_registry(self):
        """Mock message registry."""
        registry = Mock()
        
        @dataclass
        class TestMessage(RedisMessage):
            content: str
            message_type: str = "test_message"
        
        registry.get_message_class.return_value = TestMessage
        return registry
    
    @pytest.fixture
    def pubsub_subscriber(self, mock_redis_client, mock_channel_config, mock_message_registry):
        """Create subscriber in pub/sub mode."""
        from utils.redis_abstractions import RedisMode
        
        class TestSubscriber(BaseRedisSubscriber):
            def get_subscription_channels(self) -> list[str]:
                return ["test_channel"]
            
            def get_subscription_streams(self) -> list[str]:
                return ["test_stream"]
            
            def get_consumer_group(self) -> str:
                return "test_group"
            
            def get_consumer_name(self) -> str:
                return "test_consumer"
            
            async def handle_message(self, message: RedisMessage, channel: str) -> bool:
                return True
        
        return TestSubscriber(
            redis_client=mock_redis_client,
            channel_config=mock_channel_config,
            message_registry=mock_message_registry,
            mode=RedisMode.PUB_SUB
        )
    
    @pytest.fixture
    def streams_subscriber(self, mock_redis_client, mock_channel_config, mock_message_registry):
        """Create subscriber in streams mode."""
        from utils.redis_abstractions import RedisMode
        
        class TestSubscriber(BaseRedisSubscriber):
            def get_subscription_channels(self) -> list[str]:
                return ["test_channel"]
            
            def get_subscription_streams(self) -> list[str]:
                return ["test_stream"]
            
            def get_consumer_group(self) -> str:
                return "test_group"
            
            def get_consumer_name(self) -> str:
                return "test_consumer"
            
            async def handle_message(self, message: RedisMessage, channel: str) -> bool:
                return True
        
        return TestSubscriber(
            redis_client=mock_redis_client,
            channel_config=mock_channel_config,
            message_registry=mock_message_registry,
            mode=RedisMode.STREAMS
        )
    
    @pytest.mark.asyncio
    async def test_pubsub_start_listening(self, pubsub_subscriber, mock_redis_client):
        """Test starting pub/sub listener."""
        # Create a proper async iterator mock
        class MockAsyncIterator:
            def __init__(self):
                self.messages = [{'type': 'subscribe', 'channel': 'test_channel'}]
                self.index = 0
            
            def __aiter__(self):
                return self
            
            async def __anext__(self):
                if self.index < len(self.messages):
                    message = self.messages[self.index]
                    self.index += 1
                    return message
                else:
                    raise StopAsyncIteration
        
        # Create mock pubsub object with proper method mocking
        mock_pubsub = Mock()
        mock_pubsub.subscribe = AsyncMock()  # Make subscribe async
        mock_pubsub.close = AsyncMock()     # Make close async
        mock_pubsub.listen = Mock(return_value=MockAsyncIterator())  # Regular mock returning async iterator
        
        # Configure the mock to return our pubsub mock
        mock_redis_client.pubsub = Mock(return_value=mock_pubsub)
        
        # This should use pub/sub mechanism
        await pubsub_subscriber.start_listening()
        
        mock_redis_client.pubsub.assert_called_once()
        mock_pubsub.subscribe.assert_called_once_with("test_channel")
    
    @pytest.mark.asyncio
    async def test_streams_start_listening(self, streams_subscriber, mock_redis_client):
        """Test starting streams consumer."""
        mock_redis_client.xgroup_create.return_value = True
        mock_redis_client.xreadgroup.return_value = {}  # Empty for test
        
        # Mock the actual listening method that will be implemented
        with patch.object(streams_subscriber, '_start_streams_listening') as mock_listen:
            await streams_subscriber.start_listening()
            mock_listen.assert_called_once()
    
    @pytest.mark.asyncio
    async def test_process_stream_message(self, streams_subscriber, mock_message_registry):
        """Test processing a stream message with correct list format from xreadgroup."""
        # Mock the Redis xack method
        streams_subscriber.redis_client.xack = AsyncMock(return_value=1)
        
        # xreadgroup returns a list of tuples, not a dictionary
        # Format: [(stream_name, [(message_id, fields), ...]), ...]
        stream_data = [
            ('test_stream', [
                ('1640000000000-0', {
                    b'message_id': b'12345678-1234-5678-9012-123456789012',
                    b'timestamp': b'2021-12-20T12:00:00',
                    b'content': b'test message',
                    b'message_type': b'test_message'
                })
            ])
        ]
        
        result = await streams_subscriber.process_stream_messages(stream_data)
        
        assert result is True
        mock_message_registry.get_message_class.assert_called_once_with('test_message')
        # Verify the message was acknowledged
        streams_subscriber.redis_client.xack.assert_called_once_with(
            'test_stream', 'test_group', '1640000000000-0'
        )
    
    @pytest.mark.asyncio
    async def test_process_stream_messages_multiple_streams(self, streams_subscriber, mock_message_registry):
        """Test processing messages from multiple streams with correct list format."""
        # Mock the Redis xack method
        streams_subscriber.redis_client.xack = AsyncMock(return_value=1)
        
        # Multiple streams with multiple messages each
        stream_data = [
            ('stream1', [
                ('1640000000000-0', {
                    b'message_id': b'12345678-1234-5678-9012-123456789012',
                    b'timestamp': b'2021-12-20T12:00:00',
                    b'content': b'message 1',
                    b'message_type': b'test_message'
                }),
                ('1640000000001-0', {
                    b'message_id': b'12345678-1234-5678-9012-123456789013',
                    b'timestamp': b'2021-12-20T12:00:01',
                    b'content': b'message 2',
                    b'message_type': b'test_message'
                })
            ]),
            ('stream2', [
                ('1640000000002-0', {
                    b'message_id': b'12345678-1234-5678-9012-123456789014',
                    b'timestamp': b'2021-12-20T12:00:02',
                    b'content': b'message 3',
                    b'message_type': b'test_message'
                })
            ])
        ]
        
        result = await streams_subscriber.process_stream_messages(stream_data)
        
        assert result is True
        # Should be called 3 times (once for each message)
        assert mock_message_registry.get_message_class.call_count == 3
        # Should acknowledge all 3 messages
        assert streams_subscriber.redis_client.xack.call_count == 3
    
    @pytest.mark.asyncio
    async def test_process_stream_messages_empty_list(self, streams_subscriber):
        """Test processing empty stream messages list."""
        # Empty list should be handled gracefully
        stream_data = []
        
        result = await streams_subscriber.process_stream_messages(stream_data)
        
        assert result is True
        # No acknowledgments should be made for empty list
        streams_subscriber.redis_client.xack.assert_not_called()
    
    def test_get_consumer_group_required_for_streams(self, streams_subscriber):
        """Test that streams mode requires consumer group configuration."""
        # This tests that abstract methods are properly implemented
        assert streams_subscriber.get_consumer_group() == "test_group"
        assert streams_subscriber.get_consumer_name() == "test_consumer"
        assert streams_subscriber.get_subscription_streams() == ["test_stream"]


class TestRedisModesIntegration:
    """Integration tests for Redis modes functionality."""
    
    @pytest.mark.asyncio
    async def test_mode_switching_compatibility(self):
        """Test that mode switching maintains API compatibility."""
        from utils.redis_abstractions import RedisMode, BaseRedisPublisher
        
        mock_redis = AsyncMock()
        mock_config = Mock()
        
        class TestPublisher(BaseRedisPublisher):
            def get_default_channel(self) -> str:
                return "test_channel"
            
            def get_default_stream(self) -> str:
                return "test_stream"
        
        # Should be able to create with different modes
        pubsub_publisher = TestPublisher(mock_redis, mock_config, RedisMode.PUB_SUB)
        streams_publisher = TestPublisher(mock_redis, mock_config, RedisMode.STREAMS)
        
        assert pubsub_publisher.mode == RedisMode.PUB_SUB
        assert streams_publisher.mode == RedisMode.STREAMS
    
    def test_backward_compatibility_default_mode(self):
        """Test backward compatibility with default pub/sub mode."""
        from utils.redis_abstractions import RedisMode, BaseRedisPublisher
        
        mock_redis = AsyncMock()
        mock_config = Mock()
        
        class TestPublisher(BaseRedisPublisher):
            def get_default_channel(self) -> str:
                return "test_channel"
            
            def get_default_stream(self) -> str:
                return "test_stream"
        
        # Should default to PUB_SUB for backward compatibility
        publisher = TestPublisher(mock_redis, mock_config)
        assert publisher.mode == RedisMode.PUB_SUB 