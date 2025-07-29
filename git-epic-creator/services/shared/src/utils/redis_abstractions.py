"""
Redis abstraction classes that unify common functionality.
Provides base classes, mixins, and utilities for Redis-based services.
"""

import json
from abc import ABC, abstractmethod
from typing import Dict, Any, Type, Optional
from uuid import UUID
from datetime import datetime
from dataclasses import dataclass, asdict
import structlog

logger = structlog.get_logger(__name__)


@dataclass
class RedisMessage:
    """
    Base class for all Redis messages.
    Provides common fields and serialization interface.
    """
    message_id: UUID
    timestamp: datetime

    def to_dict(self) -> Dict[str, Any]:
        """
        Convert message to dictionary for JSON serialization.
        
        Returns:
            Dict[str, Any]: Message data as dictionary
        """
        data = asdict(self)
        
        # Convert UUID and datetime to string for JSON serialization
        data["message_id"] = str(self.message_id)
        data["timestamp"] = self.timestamp.isoformat()
        
        return data

    @classmethod
    def from_dict(cls, data: Dict[str, Any]) -> 'RedisMessage':
        """
        Create message from dictionary (Redis message deserialization).
        
        Args:
            data: Dictionary containing message data
            
        Returns:
            RedisMessage: Parsed message
            
        Raises:
            ValueError: If data is invalid or UUIDs are malformed
        """
        try:
            # Convert string fields back to appropriate types
            if "message_id" in data:
                data["message_id"] = UUID(data["message_id"])
            if "timestamp" in data:
                data["timestamp"] = datetime.fromisoformat(data["timestamp"])
                
            return cls(**data)
        except (KeyError, ValueError, TypeError) as e:
            raise ValueError(f"Invalid message format: {e}")


class RedisHealthMixin:
    """
    Mixin class for Redis connection health checking.
    Provides standardized health check functionality.
    """

    async def check_connection(self) -> bool:
        """
        Check Redis connection health.
        
        Returns:
            bool: True if connection is healthy, False otherwise
        """
        try:
            await self.redis_client.ping()
            logger.debug("Redis connection check successful")
            return True
        except Exception as e:
            logger.error("Redis connection check failed", error=str(e))
            return False

    async def check_health(self) -> dict:
        """
        Check Redis connection health.
        
        Returns:
            dict: Health check results
        """
        try:
            await self.redis_client.ping()
            logger.info("Redis health check passed")
            return {"healthy": True}
        except Exception as e:
            logger.error("Redis health check failed", error=str(e))
            return {"healthy": False, "error": str(e)}

    async def check_health_with_details(self) -> dict:
        """
        Check Redis connection health with detailed information.
        
        Returns:
            dict: Health check results with details
        """
        try:
            # Test basic connectivity
            ping_result = await self.redis_client.ping()
            
            try:
                # Get server info
                info = await self.redis_client.info()
                
                result = {
                    "healthy": True,
                    "ping": ping_result,
                    "version": info.get("redis_version", "unknown"),
                    "connected_clients": info.get("connected_clients", 0),
                    "used_memory": info.get("used_memory", 0),
                    "uptime_in_seconds": info.get("uptime_in_seconds", 0)
                }
                
                logger.info("Redis detailed health check passed", **result)
                return result
                
            except Exception as info_error:
                # Ping succeeded but info failed
                result = {
                    "healthy": False,
                    "error": str(info_error),
                    "ping": ping_result
                }
                logger.error("Redis detailed health check failed", **result)
                return result
            
        except Exception as ping_error:
            # Ping failed
            result = {
                "healthy": False,
                "error": str(ping_error),
                "ping": False
            }
            logger.error("Redis detailed health check failed", **result)
            return result


class JSONSerializationMixin:
    """
    Mixin class for JSON serialization/deserialization of Redis messages.
    Provides standardized message encoding/decoding functionality.
    """

    def serialize_message(self, message: RedisMessage) -> str:
        """
        Serialize RedisMessage to JSON string.
        
        Args:
            message: RedisMessage to serialize
            
        Returns:
            str: JSON string representation
            
        Raises:
            ValueError: If serialization fails
        """
        try:
            return json.dumps(message.to_dict())
        except (TypeError, ValueError) as e:
            raise ValueError(f"Failed to serialize message: {e}")

    def deserialize_message(self, json_data: str, message_class: Type[RedisMessage]) -> RedisMessage:
        """
        Deserialize JSON string to RedisMessage.
        
        Args:
            json_data: JSON string to deserialize
            message_class: Target message class
            
        Returns:
            RedisMessage: Deserialized message instance
            
        Raises:
            ValueError: If deserialization fails
        """
        try:
            data = json.loads(json_data)
            return message_class.from_dict(data)
        except (json.JSONDecodeError, ValueError, TypeError) as e:
            raise ValueError(f"Failed to deserialize message: {e}")


class RedisChannelConfig:
    """
    Configuration class for Redis channel naming strategies.
    Provides consistent channel naming across services.
    """

    def __init__(self, channel_prefix: str, channel_separator: str = ":"):
        """
        Initialize channel configuration.
        
        Args:
            channel_prefix: Prefix for all channels
            channel_separator: Separator between prefix and channel name
        """
        self.channel_prefix = channel_prefix
        self.channel_separator = channel_separator

    def get_channel_name(self, channel: str) -> str:
        """
        Generate channel name with prefix.
        
        Args:
            channel: Base channel name
            
        Returns:
            str: Full channel name with prefix
        """
        return f"{self.channel_prefix}{self.channel_separator}{channel}"

    def get_project_channel_name(self, project_id: UUID) -> str:
        """
        Generate project-specific channel name.
        
        Args:
            project_id: Project UUID
            
        Returns:
            str: Project channel name
        """
        return f"{self.channel_prefix}{self.channel_separator}{project_id}"


class MessageTypeRegistry:
    """
    Registry for mapping message types to message classes.
    Enables dynamic message routing and validation.
    """

    def __init__(self):
        """Initialize empty message type registry."""
        self._message_types: Dict[str, Type[RedisMessage]] = {}

    def register_message_type(self, message_type: str, message_class: Type[RedisMessage]) -> None:
        """
        Register a message type with its corresponding class.
        
        Args:
            message_type: String identifier for the message type
            message_class: Message class to associate with the type
        """
        self._message_types[message_type] = message_class
        logger.debug("Registered message type", message_type=message_type, message_class=message_class.__name__)

    def get_message_class(self, message_type: str) -> Type[RedisMessage]:
        """
        Get message class for a given message type.
        
        Args:
            message_type: String identifier for the message type
            
        Returns:
            Type[RedisMessage]: Message class for the type
            
        Raises:
            ValueError: If message type is not registered
        """
        if message_type not in self._message_types:
            raise ValueError(f"Unknown message type: {message_type}")
        return self._message_types[message_type]


class BaseRedisPublisher(RedisHealthMixin, JSONSerializationMixin, ABC):
    """
    Abstract base class for Redis publishers.
    Provides common publishing functionality with health checking and serialization.
    """

    def __init__(self, redis_client, channel_config: RedisChannelConfig):
        """
        Initialize Redis publisher.
        
        Args:
            redis_client: Redis client instance
            channel_config: Channel configuration for naming
        """
        self.redis_client = redis_client
        self.channel_config = channel_config
        logger.info("BaseRedisPublisher initialized")

    @abstractmethod
    def get_default_channel(self) -> str:
        """
        Get the default channel name for this publisher.
        
        Returns:
            str: Default channel name
        """
        pass

    async def publish_message(self, message: RedisMessage, channel: Optional[str] = None) -> bool:
        """
        Publish message to Redis channel.
        
        Args:
            message: RedisMessage to publish
            channel: Target channel (uses default if None)
            
        Returns:
            bool: True if publishing succeeded, False if failed
        """
        try:
            target_channel = channel or self.get_default_channel()
            
            # Serialize message to JSON
            message_data = self.serialize_message(message)
            
            # Publish to Redis
            subscriber_count = await self.redis_client.publish(target_channel, message_data)
            
            logger.info(
                "Message published to Redis",
                channel=target_channel,
                message_id=str(message.message_id),
                subscriber_count=subscriber_count
            )
            
            return True
            
        except Exception as e:
            logger.error(
                "Failed to publish message to Redis",
                channel=channel or self.get_default_channel(),
                message_id=str(message.message_id) if hasattr(message, 'message_id') else 'unknown',
                error=str(e),
                exc_info=True
            )
            return False


class BaseRedisSubscriber(RedisHealthMixin, JSONSerializationMixin, ABC):
    """
    Abstract base class for Redis subscribers.
    Provides common subscription functionality with health checking and deserialization.
    """

    def __init__(self, redis_client, channel_config: RedisChannelConfig, message_registry: MessageTypeRegistry):
        """
        Initialize Redis subscriber.
        
        Args:
            redis_client: Redis client instance
            channel_config: Channel configuration for naming
            message_registry: Registry for message type routing
        """
        self.redis_client = redis_client
        self.channel_config = channel_config
        self.message_registry = message_registry
        self._running = False
        self._pubsub = None
        logger.info("BaseRedisSubscriber initialized")

    @abstractmethod
    def get_subscription_channels(self) -> list[str]:
        """
        Get list of channels to subscribe to.
        
        Returns:
            list[str]: Channel names to subscribe to
        """
        pass

    @abstractmethod
    async def handle_message(self, message: RedisMessage, channel: str) -> bool:
        """
        Handle received Redis message.
        
        Args:
            message: Deserialized Redis message
            channel: Channel the message was received on
            
        Returns:
            bool: True if message was handled successfully, False otherwise
        """
        pass

    async def process_redis_message(self, redis_message: Dict[str, Any]) -> bool:
        """
        Process raw Redis pub/sub message.
        
        Args:
            redis_message: Raw Redis message
            
        Returns:
            bool: True if message was processed successfully, False otherwise
        """
        try:
            # Decode message data
            message_data = redis_message['data']
            if isinstance(message_data, bytes):
                message_data = message_data.decode('utf-8')
            
            # Parse JSON to get message type
            message_dict = json.loads(message_data)
            message_type = message_dict.get('message_type', 'unknown')
            
            # Get message class from registry
            message_class = self.message_registry.get_message_class(message_type)
            
            # Deserialize message
            message = self.deserialize_message(message_data, message_class)
            
            # Handle the message
            channel = redis_message.get('channel', 'unknown')
            result = await self.handle_message(message, channel)
            
            if result:
                logger.info(
                    "Redis message processed successfully",
                    message_type=message_type,
                    message_id=str(message.message_id),
                    channel=channel
                )
            else:
                logger.error(
                    "Failed to process Redis message",
                    message_type=message_type,
                    message_id=str(message.message_id),
                    channel=channel
                )
                
            return result
            
        except (json.JSONDecodeError, ValueError, KeyError) as e:
            logger.error("Failed to process Redis message", error=str(e))
            return False
        except Exception as e:
            logger.error("Unexpected error processing Redis message", error=str(e), exc_info=True)
            return False

    async def start_listening(self) -> None:
        """
        Start listening for Redis pub/sub messages.
        This method runs continuously until stop_listening() is called.
        """
        try:
            self._running = True
            self._pubsub = self.redis_client.pubsub()
            
            # Subscribe to all configured channels
            channels = self.get_subscription_channels()
            for channel in channels:
                await self._pubsub.subscribe(channel)
            
            logger.info("Started listening for Redis messages", channels=channels)
            
            # Listen for messages
            async for message in self._pubsub.listen():
                if not self._running:
                    break
                    
                if message['type'] == 'message':
                    try:
                        await self.process_redis_message(message)
                    except Exception as e:
                        logger.error("Error processing Redis message", error=str(e), exc_info=True)
                        # Continue processing other messages
                        
            logger.info("Stopped listening for Redis messages")
            
        except Exception as e:
            logger.error("Redis subscriber encountered error", error=str(e), exc_info=True)
        finally:
            if self._pubsub:
                await self._pubsub.close()
                self._pubsub = None

    async def stop_listening(self) -> None:
        """Stop listening for Redis messages."""
        self._running = False
        logger.info("Redis subscriber stop requested") 