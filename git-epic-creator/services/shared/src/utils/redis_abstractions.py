"""
Redis abstraction classes that unify common functionality.
Provides base classes, mixins, and utilities for Redis-based services.
"""

import asyncio
import json
from abc import ABC, abstractmethod
from typing import Dict, Any, Type, Optional
from uuid import UUID
from datetime import datetime
from dataclasses import dataclass, asdict
from enum import Enum
import structlog

logger = structlog.get_logger(__name__)


class RedisMode(Enum):
    """
    Enum for Redis operation modes.
    Supports both pub/sub and streams for different use cases.
    """
    PUB_SUB = "pub_sub"
    STREAMS = "streams"


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
    Supports both pub/sub and streams modes.
    """

    def __init__(self, redis_client, channel_config: RedisChannelConfig, mode: RedisMode = RedisMode.PUB_SUB):
        """
        Initialize Redis publisher.
        
        Args:
            redis_client: Redis client instance
            channel_config: Channel configuration for naming
            mode: Redis mode (pub/sub or streams)
        """
        self.redis_client = redis_client
        self.channel_config = channel_config
        self.mode = mode
        logger.debug("BaseRedisPublisher initialized", mode=mode.value)

    @abstractmethod
    def get_default_channel(self) -> str:
        """
        Get the default channel name for this publisher.
        
        Returns:
            str: Default channel name
        """
        pass

    @abstractmethod
    def get_default_stream(self) -> str:
        """
        Get the default stream name for this publisher.
        
        Returns:
            str: Default stream name
        """
        pass

    async def publish_message(self, message: RedisMessage, channel: Optional[str] = None) -> bool:
        """
        Publish message to Redis using the configured mode.
        
        Args:
            message: RedisMessage to publish
            channel: Target channel/stream (uses default if None)
            
        Returns:
            bool: True if publishing succeeded, False if failed
        """
        if self.mode == RedisMode.PUB_SUB:
            return await self._publish_to_channel(message, channel)
        elif self.mode == RedisMode.STREAMS:
            return await self.publish_to_stream(message, channel)
        else:
            logger.error("Unknown Redis mode", mode=self.mode)
            return False

    async def _publish_to_channel(self, message: RedisMessage, channel: Optional[str] = None) -> bool:
        """
        Publish message to Redis pub/sub channel.
        
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
            
            logger.debug(
                "Message published to Redis channel",
                channel=target_channel,
                message_id=str(message.message_id),
                subscriber_count=subscriber_count,
                mode=self.mode.value
            )
            
            return True
            
        except Exception as e:
            logger.error(
                "Failed to publish message to Redis channel",
                channel=channel or self.get_default_channel(),
                message_id=str(message.message_id) if hasattr(message, 'message_id') else 'unknown',
                error=str(e),
                mode=self.mode.value,
                exc_info=True
            )
            return False

    async def publish_to_stream(self, message: RedisMessage, stream: Optional[str] = None) -> bool:
        """
        Publish message to Redis stream.
        
        Args:
            message: RedisMessage to publish
            stream: Target stream (uses default if None)
            
        Returns:
            bool: True if publishing succeeded, False if failed
        """
        try:
            target_stream = stream or self.get_default_stream()
            
            # Convert message to dictionary for stream
            message_data = message.to_dict()
            
            # Serialize complex values to JSON strings for Redis streams compatibility
            stream_data = {}
            for key, value in message_data.items():
                if isinstance(value, (dict, list)):
                    stream_data[key] = json.dumps(value)
                else:
                    stream_data[key] = str(value)
            
            # Add to Redis stream
            stream_id = await self.redis_client.xadd(target_stream, stream_data)
            
            logger.debug(
                "Message published to Redis stream",
                stream=target_stream,
                message_id=str(message.message_id),
                stream_id=stream_id,
                mode=self.mode.value
            )
            
            return True
            
        except Exception as e:
            logger.error(
                "Failed to publish message to Redis stream",
                stream=stream or self.get_default_stream(),
                message_id=str(message.message_id) if hasattr(message, 'message_id') else 'unknown',
                error=str(e),
                mode=self.mode.value,
                exc_info=True
            )
            return False


class BaseRedisSubscriber(RedisHealthMixin, JSONSerializationMixin, ABC):
    """
    Abstract base class for Redis subscribers.
    Provides common subscription functionality with health checking and deserialization.
    Supports both pub/sub and streams modes.
    """

    def __init__(self, redis_client, channel_config: RedisChannelConfig, message_registry: MessageTypeRegistry, mode: RedisMode = RedisMode.PUB_SUB):
        """
        Initialize Redis subscriber.
        
        Args:
            redis_client: Redis client instance
            channel_config: Channel configuration for naming
            message_registry: Registry for message type routing
            mode: Redis mode (pub/sub or streams)
        """
        self.redis_client = redis_client
        self.channel_config = channel_config
        self.message_registry = message_registry
        self.mode = mode
        self._running = False
        self._pubsub = None
        logger.debug("BaseRedisSubscriber initialized", mode=mode.value)

    @abstractmethod
    def get_subscription_channels(self) -> list[str]:
        """
        Get list of channels to subscribe to.
        
        Returns:
            list[str]: Channel names to subscribe to
        """
        pass

    @abstractmethod
    def get_subscription_streams(self) -> list[str]:
        """
        Get list of streams to consume from.
        
        Returns:
            list[str]: Stream names to consume from
        """
        pass

    @abstractmethod
    def get_consumer_group(self) -> str:
        """
        Get consumer group name for streams mode.
        
        Returns:
            str: Consumer group name
        """
        pass

    @abstractmethod
    def get_consumer_name(self) -> str:
        """
        Get consumer name for streams mode.
        
        Returns:
            str: Consumer name
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

    async def start_listening(self) -> None:
        """
        Start listening for Redis messages using the configured mode.
        This method runs continuously until stop_listening() is called.
        """
        logger.debug(
            "Starting Redis subscriber listener",
            mode=self.mode.value,
            subscriber_class=self.__class__.__name__,
            channels=self.get_subscription_channels() if self.mode == RedisMode.PUB_SUB else None,
            streams=self.get_subscription_streams() if self.mode == RedisMode.STREAMS else None,
            consumer_group=self.get_consumer_group() if self.mode == RedisMode.STREAMS else None,
            consumer_name=self.get_consumer_name() if self.mode == RedisMode.STREAMS else None
        )
        
        try:
            if self.mode == RedisMode.PUB_SUB:
                await self._start_pubsub_listening()
            elif self.mode == RedisMode.STREAMS:
                await self._start_streams_listening()
            else:
                logger.error(
                    "Unknown Redis mode - cannot start listener",
                    mode=self.mode,
                    available_modes=[RedisMode.PUB_SUB.value, RedisMode.STREAMS.value],
                    subscriber_class=self.__class__.__name__
                )
                raise ValueError(f"Unknown Redis mode: {self.mode}")
        except Exception as e:
            logger.error(
                "Failed to start Redis subscriber listener",
                mode=self.mode.value,
                subscriber_class=self.__class__.__name__,
                error=str(e),
                exc_info=True
            )
            raise

    async def _start_pubsub_listening(self) -> None:
        """
        Start listening for Redis pub/sub messages.
        """
        try:
            self._running = True
            self._pubsub = self.redis_client.pubsub()
            
            # Subscribe to all configured channels
            channels = self.get_subscription_channels()
            for channel in channels:
                await self._pubsub.subscribe(channel)
            
            logger.debug("Started listening for Redis pub/sub messages", channels=channels, mode=self.mode.value)
            
            # Listen for messages
            async for message in self._pubsub.listen():
                if not self._running:
                    break
                    
                if message['type'] == 'message':
                    try:
                        await self.process_redis_message(message)
                    except Exception as e:
                        logger.error("Error processing Redis pub/sub message", error=str(e), exc_info=True)
                        # Continue processing other messages
                        
            logger.debug("Stopped listening for Redis pub/sub messages")
            
        except Exception as e:
            logger.error("Redis pub/sub subscriber encountered error", error=str(e), exc_info=True)
        finally:
            if self._pubsub:
                await self._pubsub.close()
                self._pubsub = None

    async def process_pending_messages(self, streams: list[str], consumer_group: str, consumer_name: str) -> int:
        """
        Process all pending messages from previous sessions.
        
        Args:
            streams: List of stream names to check for pending messages
            consumer_group: Consumer group name
            consumer_name: Consumer name
            
        Returns:
            int: Total number of pending messages processed
        """
        total_processed = 0
        
        logger.debug(
            "Starting pending message processing",
            streams=streams,
            consumer_group=consumer_group,
            consumer_name=consumer_name
        )
        
        for stream in streams:
            try:
                # Get detailed pending message information
                pending_info = await self.redis_client.xpending(stream, consumer_group)
                
                if not pending_info:
                    logger.debug(
                        "No pending info available for stream",
                        stream=stream,
                        consumer_group=consumer_group
                    )
                    continue
                
                # Handle both dict and list formats for pending_info
                if isinstance(pending_info, dict):
                    pending_count = pending_info.get('pending', 0)
                elif isinstance(pending_info, list) and len(pending_info) > 0:
                    pending_count = pending_info[0]
                else:
                    pending_count = 0
                
                if pending_count == 0:
                    logger.debug(
                        "No pending messages to process in stream",
                        stream=stream,
                        consumer_group=consumer_group
                    )
                    continue
                
                logger.debug(
                    "Found pending messages to process",
                    stream=stream,
                    consumer_group=consumer_group,
                    pending_count=pending_count
                )
                
                # Read pending messages in batches
                batch_size = 50
                processed_in_stream = 0
                
                while processed_in_stream < pending_count:
                    try:
                        # Read pending messages using stream ID "0" to get pending messages
                        logger.debug(
                            "Reading batch of pending messages",
                            stream=stream,
                            consumer_group=consumer_group,
                            consumer_name=consumer_name,
                            batch_size=batch_size,
                            processed_so_far=processed_in_stream
                        )
                        
                        messages = await self.redis_client.xreadgroup(
                            consumer_group,
                            consumer_name,
                            {stream: '0'},  # Use '0' to read pending messages
                            count=batch_size,
                            block=0  # Don't block for pending messages
                        )
                        
                        if not messages:
                            logger.debug(
                                "No more pending messages available",
                                stream=stream,
                                consumer_group=consumer_group,
                                processed_in_stream=processed_in_stream
                            )
                            break
                        
                        # Process the batch
                        batch_processed = await self.process_stream_messages(messages)
                        
                        if batch_processed:
                            batch_count = sum(len(stream_messages) for _, stream_messages in messages)
                            processed_in_stream += batch_count
                            total_processed += batch_count
                            
                            logger.debug(
                                "Processed batch of pending messages",
                                stream=stream,
                                batch_size=batch_count,
                                processed_in_stream=processed_in_stream,
                                total_processed=total_processed
                            )
                        else:
                            logger.warning(
                                "Failed to process batch of pending messages",
                                stream=stream,
                                consumer_group=consumer_group
                            )
                            break
                            
                    except Exception as batch_error:
                        logger.error(
                            "Error processing batch of pending messages",
                            stream=stream,
                            consumer_group=consumer_group,
                            error=str(batch_error),
                            processed_in_stream=processed_in_stream,
                            exc_info=True
                        )
                        break
                
                logger.debug(
                    "Completed pending message processing for stream",
                    stream=stream,
                    processed_in_stream=processed_in_stream,
                    expected_pending=pending_count
                )
                
            except Exception as stream_error:
                error_str = str(stream_error).lower()
                if "nogroup" in error_str or "consumer group" in error_str:
                    logger.debug(
                        "Consumer group does not exist for stream - skipping pending message processing",
                        stream=stream,
                        consumer_group=consumer_group,
                        error=str(stream_error)
                    )
                else:
                    logger.error(
                        "Error processing pending messages for stream",
                        stream=stream,
                        consumer_group=consumer_group,
                        error=str(stream_error),
                        exc_info=True
                    )
        
        logger.debug(
            "Pending message processing completed for all streams",
            total_processed=total_processed,
            streams=streams,
            consumer_group=consumer_group
        )
        
        return total_processed

    async def _start_streams_listening(self) -> None:
        """
        Start listening for Redis stream messages.
        """
        try:
            self._running = True
            streams = self.get_subscription_streams()
            consumer_group = self.get_consumer_group()
            consumer_name = self.get_consumer_name()
            
            logger.debug(
                "Initializing Redis streams listener",
                streams=streams,
                consumer_group=consumer_group,
                consumer_name=consumer_name,
                mode=self.mode.value,
                stream_count=len(streams)
            )
            
            # Create consumer groups for all streams (ignore if already exists)
            consumer_group_results = {}
            for stream in streams:
                try:
                    # First ensure the stream exists by checking its info
                    try:
                        await self.redis_client.xinfo_stream(stream)
                        logger.debug("Stream already exists", stream=stream)
                    except Exception:
                        # Stream doesn't exist, create it with a dummy message that we'll delete
                        logger.info("Stream doesn't exist, creating it", stream=stream)
                        dummy_id = await self.redis_client.xadd(stream, {"_dummy": "init"})
                        await self.redis_client.xdel(stream, dummy_id)
                        logger.info("Stream created and dummy message removed", stream=stream)
                    
                    # Now create the consumer group
                    await self.redis_client.xgroup_create(stream, consumer_group, id='0', mkstream=True)
                    consumer_group_results[stream] = "created"
                    logger.info(
                        "Consumer group created successfully",
                        stream=stream,
                        consumer_group=consumer_group,
                        action="created"
                    )
                except Exception as e:
                    error_str = str(e).lower()
                    if "busygroup" in error_str or "already exists" in error_str:
                        # Group already exists, which is fine
                        consumer_group_results[stream] = "already_exists"
                        logger.info(
                            "Consumer group already exists",
                            stream=stream,
                            consumer_group=consumer_group,
                            action="already_exists"
                        )
                    else:
                        # Other error, log it but continue
                        consumer_group_results[stream] = f"error: {str(e)}"
                        logger.warning(
                            "Consumer group creation failed",
                            stream=stream,
                            consumer_group=consumer_group,
                            error=str(e),
                            action="failed"
                        )
                    
                    # Check if there are pending messages in the stream for this consumer group
                    try:
                        pending_info = await self.redis_client.xpending(stream, consumer_group)
                        if pending_info and len(pending_info) > 0:
                            pending_count = pending_info[0] if isinstance(pending_info, list) else pending_info.get('pending', 0)
                            logger.debug(
                                "Found pending messages in existing consumer group",
                                stream=stream,
                                consumer_group=consumer_group,
                                pending_count=pending_count
                            )
                    except Exception as pending_error:
                        logger.debug(
                            "Could not check pending messages",
                            stream=stream,
                            consumer_group=consumer_group,
                            error=str(pending_error)
                        )
            
            logger.info(
                "Consumer group setup completed",
                consumer_group_results=consumer_group_results,
                total_streams=len(streams)
            )
            
            logger.info("Started listening for Redis stream messages", streams=streams, consumer_group=consumer_group, consumer_name=consumer_name, mode=self.mode.value)
            
            # First, process any pending messages from previous sessions
            pending_processed = await self.process_pending_messages(streams, consumer_group, consumer_name)
            logger.debug(
                "Pending message processing completed",
                streams=streams,
                consumer_group=consumer_group,
                total_pending_processed=pending_processed
            )
            
            # Listen for messages
            message_read_count = 0
            empty_read_count = 0
            
            while self._running:
                try:
                    logger.debug(
                        "Attempting to read new messages from Redis streams",
                        streams=streams,
                        consumer_group=consumer_group,
                        consumer_name=consumer_name,
                        block_timeout_ms=1000,
                        max_count=10,
                        stream_id_mode="new_messages"
                    )
                    
                    # Read from streams using consumer group (only new messages)
                    messages = await self.redis_client.xreadgroup(
                        consumer_group,
                        consumer_name,
                        {stream: '>' for stream in streams},  # '>' for new messages only
                        count=10,
                        block=1000  # Block for 1 second
                    )
                    
                    if messages:
                        message_read_count += 1
                        total_messages = sum(len(stream_messages) for _, stream_messages in messages)
                        
                        logger.info(
                            "Received new messages from Redis streams",
                            stream_count=len(messages),
                            total_messages=total_messages,
                            message_read_count=message_read_count,
                            streams_with_messages=[stream_name for stream_name, stream_messages in messages if stream_messages],
                            message_type="new_messages"
                        )
                        
                        # Process the new messages
                        processing_success = await self.process_stream_messages(messages)
                        
                        if processing_success:
                            logger.debug(
                                "Successfully processed new messages batch",
                                total_messages=total_messages,
                                message_read_count=message_read_count
                            )
                        else:
                            logger.warning(
                                "Some messages in batch failed to process",
                                total_messages=total_messages,
                                message_read_count=message_read_count
                            )
                    else:
                        empty_read_count += 1
                        if empty_read_count % 10 == 0:  # Log every 10 empty reads to avoid spam
                            logger.debug(
                                "No messages received from Redis streams - checking stream status",
                                empty_read_count=empty_read_count,
                                message_read_count=message_read_count,
                                streams=streams,
                                consumer_group=consumer_group,
                                consumer_name=consumer_name
                            )
                            
                            # Check stream info to debug why no messages are coming
                            for stream in streams:
                                try:
                                    stream_info = await self.redis_client.xinfo_stream(stream)
                                    stream_length = stream_info.get('length', 0)
                                    logger.debug(
                                        "Stream status check",
                                        stream=stream,
                                        length=stream_length,
                                        last_generated_id=stream_info.get('last-generated-id', 'unknown'),
                                        first_entry=stream_info.get('first-entry', 'none'),
                                        last_entry=stream_info.get('last-entry', 'none')
                                    )
                                    
                                    # Check consumer group info
                                    try:
                                        group_info = await self.redis_client.xinfo_groups(stream)
                                        for group in group_info:
                                            if group.get('name') == consumer_group:
                                                logger.debug(
                                                    "Consumer group status",
                                                    stream=stream,
                                                    consumer_group=consumer_group,
                                                    consumers=group.get('consumers', 0),
                                                    pending=group.get('pending', 0),
                                                    last_delivered_id=group.get('last-delivered-id', 'unknown')
                                                )
                                                break
                                    except Exception as group_error:
                                        logger.debug(
                                            "Could not get consumer group info",
                                            stream=stream,
                                            consumer_group=consumer_group,
                                            error=str(group_error)
                                        )
                                        
                                except Exception as stream_error:
                                    logger.debug(
                                        "Could not get stream info",
                                        stream=stream,
                                        error=str(stream_error)
                                    )
                        
                except Exception as e:
                    if self._running:
                        error_category = self._categorize_error(e)
                        is_retryable = self._is_retryable_error(e)
                        error_str = str(e).lower()
                        
                        # Handle NOGROUP error specifically
                        if "nogroup" in error_str:
                            logger.warning(
                                "Consumer group not found, attempting to recreate",
                                error=str(e),
                                streams=streams,
                                consumer_group=consumer_group,
                                consumer_name=consumer_name
                            )
                            
                            # Try to recreate the consumer group
                            try:
                                for stream in streams:
                                    await self.redis_client.xgroup_create(stream, consumer_group, id='0', mkstream=True)
                                    logger.debug(
                                        "Consumer group recreated after NOGROUP error",
                                        stream=stream,
                                        consumer_group=consumer_group
                                    )
                            except Exception as recreate_error:
                                logger.error(
                                    "Failed to recreate consumer group",
                                    error=str(recreate_error),
                                    streams=streams,
                                    consumer_group=consumer_group
                                )
                            
                            # Wait before retrying
                            await asyncio.sleep(2)
                            continue
                        
                        logger.error(
                            "Error reading from Redis streams",
                            error=str(e),
                            error_category=error_category,
                            is_retryable=is_retryable,
                            streams=streams,
                            consumer_group=consumer_group,
                            consumer_name=consumer_name,
                            message_read_count=message_read_count,
                            empty_read_count=empty_read_count,
                            exc_info=True
                        )
                        
                        # For connection errors, wait before retrying
                        if error_category == "redis_connection":
                            logger.debug(
                                "Redis connection error detected, waiting before retry",
                                wait_seconds=5,
                                error_category=error_category
                            )
                            await asyncio.sleep(5)
                        elif error_category == "network":
                            logger.debug(
                                "Network error detected, waiting before retry",
                                wait_seconds=2,
                                error_category=error_category
                            )
                            await asyncio.sleep(2)
                        
            logger.info(
                "Stopped listening for Redis stream messages",
                final_message_read_count=message_read_count,
                final_empty_read_count=empty_read_count,
                streams=streams
            )
            
        except Exception as e:
            logger.error(
                "Redis streams subscriber encountered fatal error",
                error=str(e),
                streams=self.get_subscription_streams() if hasattr(self, 'get_subscription_streams') else 'unknown',
                consumer_group=self.get_consumer_group() if hasattr(self, 'get_consumer_group') else 'unknown',
                consumer_name=self.get_consumer_name() if hasattr(self, 'get_consumer_name') else 'unknown',
                exc_info=True
            )

    async def process_stream_messages(self, stream_messages) -> bool:
        """
        Process messages from Redis streams.
        
        Args:
            stream_messages: Messages from Redis streams (list of tuples)
            
        Returns:
            bool: True if all messages processed successfully
        """
        import time
        
        processing_start_time = time.time()
        
        try:
            all_successful = True
            total_messages = sum(len(messages) for _, messages in stream_messages)
            processed_count = 0
            failed_count = 0
            
            logger.debug(
                "Starting stream message processing pipeline",
                total_streams=len(stream_messages),
                total_messages=total_messages,
                streams=[stream_name for stream_name, _ in stream_messages]
            )
            
            # stream_messages is a list of tuples: [(stream_name, [(message_id, fields), ...]), ...]
            for stream_name, messages in stream_messages:
                stream_processed = 0
                stream_failed = 0
                
                logger.debug(
                    "Processing messages from stream",
                    stream=stream_name,
                    message_count=len(messages)
                )
                
                for message_id, fields in messages:
                    message_start_time = time.time()
                    
                    try:
                        logger.debug(
                            "Processing individual stream message",
                            stream=stream_name,
                            message_id=message_id,
                            field_count=len(fields)
                        )
                        
                        # Convert bytes fields to strings and deserialize JSON values
                        decoded_fields = {}
                        decode_errors = []
                        
                        for key, value in fields.items():
                            try:
                                if isinstance(key, bytes):
                                    key = key.decode('utf-8')
                                if isinstance(value, bytes):
                                    value = value.decode('utf-8')
                                
                                # Try to deserialize JSON values (for complex fields like parameters)
                                if isinstance(value, str) and key in ['parameters']:
                                    try:
                                        decoded_fields[key] = json.loads(value)
                                    except (json.JSONDecodeError, ValueError) as json_error:
                                        decoded_fields[key] = value
                                        decode_errors.append(f"JSON decode failed for {key}: {str(json_error)}")
                                else:
                                    decoded_fields[key] = value
                            except Exception as decode_error:
                                decode_errors.append(f"Field decode failed for {key}: {str(decode_error)}")
                        
                        if decode_errors:
                            logger.warning(
                                "Field decoding issues encountered",
                                stream=stream_name,
                                message_id=message_id,
                                decode_errors=decode_errors
                            )
                        
                        # Get message type and deserialize
                        message_type = decoded_fields.get('message_type', 'unknown')
                        
                        logger.debug(
                            "Deserializing stream message",
                            stream=stream_name,
                            message_id=message_id,
                            message_type=message_type,
                            decoded_field_count=len(decoded_fields)
                        )
                        
                        try:
                            message_class = self.message_registry.get_message_class(message_type)
                            logger.debug(
                                "Message class resolved",
                                message_type=message_type,
                                message_class=message_class.__name__
                            )
                        except Exception as registry_error:
                            logger.error(
                                "Failed to resolve message class from registry",
                                message_type=message_type,
                                stream=stream_name,
                                message_id=message_id,
                                error=str(registry_error),
                                available_types=list(self.message_registry._message_types.keys()) if hasattr(self.message_registry, '_message_types') else 'unknown'
                            )
                            raise
                        
                        # Create message instance
                        try:
                            message = message_class.from_dict(decoded_fields)
                            logger.debug(
                                "Message instance created successfully",
                                stream=stream_name,
                                message_id=message_id,
                                message_type=message_type
                            )
                        except Exception as creation_error:
                            logger.error(
                                "Failed to create message instance",
                                stream=stream_name,
                                message_id=message_id,
                                message_type=message_type,
                                decoded_fields=decoded_fields,
                                error=str(creation_error),
                                exc_info=True
                            )
                            raise
                        
                        # Handle the message
                        logger.debug(
                            "Invoking message handler",
                            stream=stream_name,
                            message_id=message_id,
                            message_type=message_type
                        )
                        
                        success = await self.handle_message(message, stream_name)
                        
                        if success:
                            # Acknowledge the message using the dedicated method
                            ack_success = await self.acknowledge_message(stream_name, self.get_consumer_group(), message_id)
                            
                            if ack_success:
                                logger.debug(
                                    "Stream message processed and acknowledged",
                                    stream=stream_name,
                                    message_id=message_id,
                                    message_type=message_type,
                                    processing_time_ms=round((time.time() - message_start_time) * 1000, 2)
                                )
                                processed_count += 1
                                stream_processed += 1
                            else:
                                logger.error(
                                    "Message processed successfully but acknowledgment failed",
                                    stream=stream_name,
                                    message_id=message_id,
                                    message_type=message_type,
                                    consumer_group=self.get_consumer_group(),
                                    error_category="acknowledgment_failure"
                                )
                                all_successful = False
                                failed_count += 1
                                stream_failed += 1
                        else:
                            all_successful = False
                            failed_count += 1
                            stream_failed += 1
                            logger.error(
                                "Message handler returned failure",
                                stream=stream_name,
                                message_id=message_id,
                                message_type=message_type,
                                processing_time_ms=round((time.time() - message_start_time) * 1000, 2),
                                error_category="handler_failure"
                            )
                            
                    except Exception as e:
                        all_successful = False
                        failed_count += 1
                        stream_failed += 1
                        error_category = self._categorize_error(e)
                        logger.error(
                            "Error processing individual stream message",
                            stream=stream_name,
                            message_id=message_id,
                            error=str(e),
                            error_category=error_category,
                            processing_time_ms=round((time.time() - message_start_time) * 1000, 2),
                            exc_info=True
                        )
                
                logger.debug(
                    "Completed processing messages from stream",
                    stream=stream_name,
                    processed=stream_processed,
                    failed=stream_failed,
                    total=len(messages)
                )
            
            total_processing_time = time.time() - processing_start_time
            
            logger.info(
                "Stream message processing pipeline completed",
                total_messages=total_messages,
                processed_count=processed_count,
                failed_count=failed_count,
                success_rate=round((processed_count / total_messages * 100), 2) if total_messages > 0 else 0,
                total_processing_time_ms=round(total_processing_time * 1000, 2),
                avg_processing_time_ms=round((total_processing_time / total_messages * 1000), 2) if total_messages > 0 else 0,
                all_successful=all_successful
            )
            
            return all_successful
            
        except Exception as e:
            total_processing_time = time.time() - processing_start_time
            logger.error(
                "Fatal error in stream message processing pipeline",
                error=str(e),
                total_processing_time_ms=round(total_processing_time * 1000, 2),
                stream_count=len(stream_messages) if stream_messages else 0,
                exc_info=True
            )
            return False

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
                logger.debug(
                    "Redis pub/sub message processed successfully",
                    message_type=message_type,
                    message_id=str(message.message_id),
                    channel=channel
                )
            else:
                logger.error(
                    "Failed to process Redis pub/sub message",
                    message_type=message_type,
                    message_id=str(message.message_id),
                    channel=channel
                )
                
            return result
            
        except (json.JSONDecodeError, ValueError, KeyError) as e:
            logger.error("Failed to process Redis pub/sub message", error=str(e))
            return False
        except Exception as e:
            logger.error("Unexpected error processing Redis pub/sub message", error=str(e), exc_info=True)
            return False

    def _categorize_error(self, error: Exception) -> str:
        """
        Categorize errors for better handling and logging.
        
        Args:
            error: Exception to categorize
            
        Returns:
            str: Error category for logging and handling decisions
        """
        error_str = str(error).lower()
        error_type = type(error).__name__
        
        # Redis connection errors
        if "connection" in error_str or "timeout" in error_str:
            return "redis_connection"
        
        # Redis command errors
        if "redis" in error_str or error_type in ["RedisError", "ResponseError"]:
            return "redis_command"
        
        # Consumer group errors
        if "nogroup" in error_str or "consumer group" in error_str:
            return "consumer_group_error"
        
        # Serialization/deserialization errors
        if "json" in error_str or error_type in ["JSONDecodeError", "ValueError"]:
            return "serialization"
        
        # Message format errors
        if "message" in error_str and ("format" in error_str or "invalid" in error_str):
            return "message_format"
        
        # Task/handler errors
        if "task" in error_str or "celery" in error_str:
            return "task_execution"
        
        # Network errors
        if "network" in error_str or error_type in ["ConnectionError", "TimeoutError"]:
            return "network"
        
        # Default category
        return "unknown"

    def _is_retryable_error(self, error: Exception) -> bool:
        """
        Determine if an error is retryable.
        
        Args:
            error: Exception to check
            
        Returns:
            bool: True if error is retryable, False otherwise
        """
        error_category = self._categorize_error(error)
        
        # Retryable error categories
        retryable_categories = {
            "redis_connection",
            "network",
            "redis_command",  # Some Redis commands might be retryable
            "consumer_group_error"  # Consumer group errors are retryable
        }
        
        return error_category in retryable_categories

    async def acknowledge_message(self, stream: str, consumer_group: str, message_id: str) -> bool:
        """
        Acknowledge a processed message using XACK command.
        
        Args:
            stream: Stream name where the message was received
            consumer_group: Consumer group name
            message_id: ID of the message to acknowledge
            
        Returns:
            bool: True if acknowledgment succeeded, False otherwise
        """
        try:
            # Use XACK to acknowledge the message
            ack_count = await self.redis_client.xack(stream, consumer_group, message_id)
            
            if ack_count > 0:
                logger.debug(
                    "Message acknowledged successfully",
                    stream=stream,
                    consumer_group=consumer_group,
                    message_id=message_id,
                    ack_count=ack_count
                )
                return True
            else:
                logger.warning(
                    "Message acknowledgment returned zero count",
                    stream=stream,
                    consumer_group=consumer_group,
                    message_id=message_id,
                    ack_count=ack_count
                )
                return False
                
        except Exception as e:
            logger.error(
                "Failed to acknowledge message",
                stream=stream,
                consumer_group=consumer_group,
                message_id=message_id,
                error=str(e),
                exc_info=True
            )
            return False

    async def stop_listening(self) -> None:
        """Stop listening for Redis messages."""
        
        self._running = False
                
        logger.info("Redis subscriber stop completed", mode=self.mode.value) 