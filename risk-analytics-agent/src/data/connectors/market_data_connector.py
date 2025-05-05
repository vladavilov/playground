"""Market Data Connector for consuming market data from Kafka.

This module implements a Kafka consumer for subscribing to and processing market data
from the fixed income market data topic. It handles deserialization, validation, and
routing of market data to the appropriate data gateway components.
"""

import json
import logging
import time
from abc import ABC, abstractmethod
from datetime import datetime
from typing import Any, Callable, Dict, List, Optional, Union

from kafka import KafkaConsumer, KafkaProducer
from kafka.errors import KafkaError, KafkaTimeoutError

# Configure logging for the connectors
logger = logging.getLogger(__name__)


class MessageProcessor(ABC):
    """Interface for processing messages received from data connectors."""
    
    @abstractmethod
    def process_message(self, message: Dict[str, Any], metadata: Dict[str, Any]) -> bool:
        """Process a message received from a data connector.
        
        Args:
            message: The deserialized message content
            metadata: Additional information about the message (topic, timestamp, etc.)
            
        Returns:
            bool: True if processing was successful, False otherwise
        """
        pass


class DeadLetterQueueHandler:
    """Handles messages that failed processing by sending them to a dead letter queue."""
    
    def __init__(self, bootstrap_servers: List[str], topic_prefix: str = "dlq"):
        """Initialize the dead letter queue handler.
        
        Args:
            bootstrap_servers: List of Kafka broker addresses
            topic_prefix: Prefix to use for dead letter queue topics
        """
        self.bootstrap_servers = bootstrap_servers
        self.topic_prefix = topic_prefix
        self.producer = None
        self._initialize_producer()
        
    def _initialize_producer(self) -> None:
        """Initialize the Kafka producer for the dead letter queue."""
        try:
            self.producer = KafkaProducer(
                bootstrap_servers=self.bootstrap_servers,
                value_serializer=lambda v: json.dumps(v).encode('utf-8'),
                acks='all'  # Wait for all replicas to acknowledge
            )
            logger.info("Dead letter queue handler initialized successfully")
        except KafkaError as e:
            logger.error(f"Failed to initialize dead letter queue handler: {e}")
            self.producer = None
            
    def send_to_dlq(self, original_topic: str, message: Any, error_info: Dict[str, Any]) -> bool:
        """Send a failed message to the dead letter queue.
        
        Args:
            original_topic: The original topic the message was consumed from
            message: The original message (can be already deserialized)
            error_info: Information about why the message processing failed
            
        Returns:
            bool: True if the message was sent to DLQ successfully, False otherwise
        """
        if not self.producer:
            logger.error("Dead letter queue producer not initialized")
            return False
            
        dlq_topic = f"{self.topic_prefix}-{original_topic}"
        payload = {
            "original_topic": original_topic,
            "original_message": message,
            "error_info": error_info,
            "timestamp": datetime.now().isoformat()
        }
        
        try:
            future = self.producer.send(dlq_topic, payload)
            # Wait for the send to complete to ensure it worked
            future.get(timeout=10)
            logger.info(f"Message sent to DLQ topic {dlq_topic}")
            return True
        except (KafkaError, KafkaTimeoutError) as e:
            logger.error(f"Failed to send message to DLQ topic {dlq_topic}: {e}")
            return False
            
    def close(self) -> None:
        """Close the DLQ producer."""
        if self.producer:
            self.producer.close()
            logger.info("Dead letter queue handler closed")


class MarketDataConnector:
    """Connector for consuming market data from Kafka."""
    
    def __init__(
        self,
        bootstrap_servers: List[str],
        topic: str = "market-data-fixed-income",
        group_id: str = "risk-analytics-market-data",
        auto_offset_reset: str = "latest",
        enable_auto_commit: bool = True,
        consumer_timeout_ms: int = 1000,
        max_poll_records: int = 500,
        message_processor: Optional[MessageProcessor] = None,
        dead_letter_queue: Optional[DeadLetterQueueHandler] = None
    ):
        """Initialize the MarketDataConnector.
        
        Args:
            bootstrap_servers: List of Kafka broker addresses
            topic: Kafka topic to consume market data from
            group_id: Consumer group ID for this consumer
            auto_offset_reset: Where to start consuming when no offset is stored
            enable_auto_commit: Whether to automatically commit offsets
            consumer_timeout_ms: Time in ms to wait for message before returning
            max_poll_records: Maximum number of records to return in a single poll
            message_processor: Component to process received messages
            dead_letter_queue: Handler for messages that fail processing
        """
        self.bootstrap_servers = bootstrap_servers
        self.topic = topic
        self.group_id = group_id
        self.auto_offset_reset = auto_offset_reset
        self.enable_auto_commit = enable_auto_commit
        self.consumer_timeout_ms = consumer_timeout_ms
        self.max_poll_records = max_poll_records
        self.message_processor = message_processor
        self.dlq_handler = dead_letter_queue
        
        self.consumer = None
        self.running = False
        self.metrics = {
            "messages_processed": 0,
            "messages_failed": 0,
            "processing_time_sum": 0,
            "consumer_lag": 0,
            "last_message_timestamp": None
        }
        
    def connect(self) -> bool:
        """Connect to the Kafka broker and initialize the consumer.
        
        Returns:
            bool: True if connection was successful, False otherwise
        """
        try:
            self.consumer = KafkaConsumer(
                self.topic,
                bootstrap_servers=self.bootstrap_servers,
                group_id=self.group_id,
                auto_offset_reset=self.auto_offset_reset,
                enable_auto_commit=self.enable_auto_commit,
                consumer_timeout_ms=self.consumer_timeout_ms,
                max_poll_records=self.max_poll_records,
                value_deserializer=lambda v: json.loads(v.decode('utf-8'))
            )
            logger.info(f"Connected to Kafka topic: {self.topic}")
            return True
        except KafkaError as e:
            logger.error(f"Failed to connect to Kafka: {e}")
            return False
            
    def start_consuming(self, polling_interval: float = 0.1, max_retries: int = 3) -> None:
        """Start consuming messages from the Kafka topic.
        
        Args:
            polling_interval: Time in seconds to wait between polling attempts
            max_retries: Maximum number of retries for processing a message
        """
        if not self.consumer:
            logger.error("Consumer not initialized. Call connect() first.")
            return
            
        logger.info(f"Starting to consume messages from topic: {self.topic}")
        self.running = True
        
        try:
            while self.running:
                try:
                    # Poll for messages
                    message_batch = self.consumer.poll(timeout_ms=self.consumer_timeout_ms, max_records=self.max_poll_records)
                    
                    for topic_partition, messages in message_batch.items():
                        for message in messages:
                            self._process_message(message, max_retries)
                            
                    # Update consumer lag metric
                    self._update_consumer_lag()
                    
                    # Small sleep to prevent tight loop if no messages
                    time.sleep(polling_interval)
                    
                except Exception as e:
                    logger.error(f"Error during message consumption: {e}")
                    # Attempt to reconnect in case of disconnection
                    self._attempt_reconnect()
                    
        except KeyboardInterrupt:
            logger.info("Received keyboard interrupt, stopping consumer")
            
        finally:
            self.stop_consuming()
            
    def _process_message(self, message: Any, max_retries: int) -> None:
        """Process a single message from Kafka.
        
        Args:
            message: The Kafka message to process
            max_retries: Maximum number of retries for processing
        """
        start_time = time.time()
        retries = 0
        
        while retries <= max_retries:
            try:
                # Extract message value and metadata
                message_value = message.value
                metadata = {
                    "topic": message.topic,
                    "partition": message.partition,
                    "offset": message.offset,
                    "timestamp": message.timestamp,
                    "key": message.key.decode('utf-8') if message.key else None
                }
                
                if self.message_processor:
                    success = self.message_processor.process_message(message_value, metadata)
                    if success:
                        # Update metrics
                        self.metrics["messages_processed"] += 1
                        processing_time = time.time() - start_time
                        self.metrics["processing_time_sum"] += processing_time
                        self.metrics["last_message_timestamp"] = message.timestamp
                        
                        # Log metrics periodically
                        if self.metrics["messages_processed"] % 1000 == 0:
                            avg_processing_time = self.metrics["processing_time_sum"] / self.metrics["messages_processed"]
                            logger.info(f"Processed {self.metrics['messages_processed']} messages. "
                                      f"Avg processing time: {avg_processing_time:.3f}s. "
                                      f"Consumer lag: {self.metrics['consumer_lag']}")
                        
                        return
                else:
                    logger.warning("No message processor configured, message acknowledged but not processed")
                    return
                    
                # If processing failed, increment retry counter
                retries += 1
                logger.warning(f"Failed to process message (retry {retries}/{max_retries})")
                
            except Exception as e:
                logger.error(f"Error processing message: {e}")
                retries += 1
                if retries <= max_retries:
                    logger.info(f"Retrying message processing ({retries}/{max_retries})")
                    time.sleep(0.5 * retries)  # Exponential backoff
                
        # If we've exhausted retries, send to DLQ
        self.metrics["messages_failed"] += 1
        if self.dlq_handler:
            error_info = {
                "error": "Max retries exceeded for message processing",
                "last_retry_time": datetime.now().isoformat()
            }
            self.dlq_handler.send_to_dlq(message.topic, message.value, error_info)
        else:
            logger.error("Message processing failed and no DLQ handler configured")
            
    def _update_consumer_lag(self) -> None:
        """Update the consumer lag metric."""
        try:
            for tp, lag in self.consumer.metrics().items():
                if isinstance(tp, tuple) and len(tp) > 1 and tp[0] == "records-lag-max":
                    self.metrics["consumer_lag"] = lag
                    break
        except Exception as e:
            logger.error(f"Error updating consumer lag metric: {e}")
            
    def _attempt_reconnect(self, max_reconnect_attempts: int = 5, reconnect_backoff: float = 2.0) -> bool:
        """Attempt to reconnect to Kafka after a connection failure.
        
        Args:
            max_reconnect_attempts: Maximum number of reconnection attempts
            reconnect_backoff: Backoff multiplier for reconnection attempts (exponential)
            
        Returns:
            bool: True if reconnection was successful, False otherwise
        """
        if not self.running:
            return False
            
        if self.consumer:
            try:
                self.consumer.close()
            except:
                pass
                
        for attempt in range(max_reconnect_attempts):
            logger.info(f"Attempting to reconnect to Kafka (attempt {attempt+1}/{max_reconnect_attempts})")
            
            if self.connect():
                logger.info("Successfully reconnected to Kafka")
                return True
                
            # Exponential backoff
            sleep_time = reconnect_backoff * (2 ** attempt)
            logger.info(f"Reconnection failed, waiting {sleep_time:.2f}s before next attempt")
            time.sleep(sleep_time)
            
        logger.error(f"Failed to reconnect to Kafka after {max_reconnect_attempts} attempts")
        self.running = False
        return False
        
    def stop_consuming(self) -> None:
        """Stop consuming messages and close the consumer."""
        self.running = False
        if self.consumer:
            logger.info("Stopping market data consumer")
            try:
                self.consumer.close()
                logger.info("Market data consumer closed")
            except Exception as e:
                logger.error(f"Error closing consumer: {e}")
                
    def health_check(self) -> Dict[str, Any]:
        """Check the health of the connector.
        
        Returns:
            Dict: Health check information including connection status and metrics
        """
        health_info = {
            "connected": self.consumer is not None,
            "running": self.running,
            "metrics": self.metrics
        }
        
        # Check if we've received messages recently
        if self.metrics["last_message_timestamp"]:
            last_message_age = (datetime.now().timestamp() * 1000) - self.metrics["last_message_timestamp"]
            health_info["last_message_age_seconds"] = last_message_age / 1000
            
        return health_info 