"""Trade History Connector for consuming trade data from Kafka and REST API.

This module implements both a Kafka consumer for subscribing to real-time trade updates 
and a REST API client for fetching historical trade data in batch mode. It provides a 
unified interface for accessing trade data regardless of the source.
"""

import json
import logging
import time
import threading
from abc import ABC, abstractmethod
from datetime import datetime, timedelta
from typing import Any, Callable, Dict, List, Optional, Union

import requests
from kafka import KafkaConsumer
from kafka.errors import KafkaError, KafkaTimeoutError
from requests.adapters import HTTPAdapter
from urllib3.util.retry import Retry

# Configure logging
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


class TradeHistoryConnector:
    """Connector for consuming trade history data from both REST API and Kafka streams."""
    
    def __init__(
        self,
        rest_api_url: str,
        rest_api_key: str,
        kafka_bootstrap_servers: List[str],
        kafka_topic: str = "trade-history-fixed-income-updates",
        kafka_group_id: str = "risk-analytics-trade-history",
        message_processor: Optional[MessageProcessor] = None,
        max_retries: int = 3,
        retry_backoff: float = 0.5,
        connection_timeout: int = 30,
        connection_pool_size: int = 10
    ):
        """Initialize the TradeHistoryConnector.
        
        Args:
            rest_api_url: Base URL for the REST API
            rest_api_key: API key for authentication
            kafka_bootstrap_servers: List of Kafka broker addresses
            kafka_topic: Kafka topic for real-time trade updates
            kafka_group_id: Consumer group ID for Kafka consumer
            message_processor: Component to process received messages
            max_retries: Maximum number of retries for HTTP requests
            retry_backoff: Backoff factor for retries (in seconds)
            connection_timeout: Connection timeout (in seconds)
            connection_pool_size: Maximum number of connections in the pool
        """
        self.rest_api_url = rest_api_url
        self.rest_api_key = rest_api_key
        self.kafka_bootstrap_servers = kafka_bootstrap_servers
        self.kafka_topic = kafka_topic
        self.kafka_group_id = kafka_group_id
        self.message_processor = message_processor
        self.max_retries = max_retries
        self.retry_backoff = retry_backoff
        self.connection_timeout = connection_timeout
        self.connection_pool_size = connection_pool_size
        
        # HTTP Session with connection pooling and retry strategy
        self.session = self._create_http_session()
        
        # Kafka consumer
        self.kafka_consumer = None
        self.kafka_thread = None
        self.running = False
        
        # Cache for recently fetched data
        self.cache = {}
        self.cache_expiry = {}
        self.cache_max_size = 100  # Maximum number of items in cache
        self.cache_ttl = 300  # Cache TTL in seconds
        
        # Metrics
        self.metrics = {
            "rest_requests": 0,
            "rest_errors": 0,
            "rest_total_time": 0,
            "kafka_messages_processed": 0,
            "kafka_messages_failed": 0,
            "cache_hits": 0,
            "cache_misses": 0,
            "last_batch_size": 0,
            "last_batch_time": None
        }
        
    def _create_http_session(self) -> requests.Session:
        """Create an HTTP session with connection pooling and retry strategy.
        
        Returns:
            requests.Session: Configured HTTP session
        """
        session = requests.Session()
        
        # Configure retry strategy
        retry_strategy = Retry(
            total=self.max_retries,
            backoff_factor=self.retry_backoff,
            status_forcelist=[429, 500, 502, 503, 504],
            allowed_methods=["GET"]
        )
        
        # Create adapter with retry strategy and connection pooling
        adapter = HTTPAdapter(
            max_retries=retry_strategy,
            pool_connections=self.connection_pool_size,
            pool_maxsize=self.connection_pool_size
        )
        
        # Mount adapter to session
        session.mount("http://", adapter)
        session.mount("https://", adapter)
        
        return session
        
    def connect_kafka(self) -> bool:
        """Connect to the Kafka broker and initialize the consumer.
        
        Returns:
            bool: True if connection was successful, False otherwise
        """
        try:
            self.kafka_consumer = KafkaConsumer(
                self.kafka_topic,
                bootstrap_servers=self.kafka_bootstrap_servers,
                group_id=self.kafka_group_id,
                auto_offset_reset="latest",
                value_deserializer=lambda v: json.loads(v.decode('utf-8'))
            )
            logger.info(f"Connected to Kafka topic: {self.kafka_topic}")
            return True
        except KafkaError as e:
            logger.error(f"Failed to connect to Kafka: {e}")
            return False
            
    def start_kafka_consumer(self) -> bool:
        """Start the Kafka consumer in a separate thread.
        
        Returns:
            bool: True if consumer was started successfully, False otherwise
        """
        if self.running:
            logger.warning("Kafka consumer already running")
            return True
            
        if not self.kafka_consumer and not self.connect_kafka():
            logger.error("Failed to connect to Kafka")
            return False
            
        # Start consumer in a separate thread
        self.running = True
        self.kafka_thread = threading.Thread(target=self._kafka_consumer_loop)
        self.kafka_thread.daemon = True
        self.kafka_thread.start()
        
        logger.info("Kafka consumer thread started")
        return True
        
    def _kafka_consumer_loop(self) -> None:
        """Main loop for Kafka consumer thread."""
        logger.info(f"Starting to consume messages from Kafka topic: {self.kafka_topic}")
        
        try:
            for message in self.kafka_consumer:
                if not self.running:
                    break
                    
                try:
                    # Extract message value and metadata
                    message_value = message.value
                    metadata = {
                        "source": "kafka",
                        "topic": message.topic,
                        "partition": message.partition,
                        "offset": message.offset,
                        "timestamp": message.timestamp
                    }
                    
                    # Process message
                    if self.message_processor:
                        success = self.message_processor.process_message(message_value, metadata)
                        if success:
                            self.metrics["kafka_messages_processed"] += 1
                        else:
                            self.metrics["kafka_messages_failed"] += 1
                    else:
                        logger.warning("No message processor configured for Kafka consumer")
                        
                except Exception as e:
                    logger.error(f"Error processing Kafka message: {e}")
                    self.metrics["kafka_messages_failed"] += 1
                    
        except KafkaError as e:
            logger.error(f"Kafka consumer error: {e}")
            # Attempt to reconnect
            if self.running:
                self._attempt_kafka_reconnect()
                
        except Exception as e:
            logger.error(f"Unexpected error in Kafka consumer: {e}")
            
        finally:
            if self.kafka_consumer:
                self.kafka_consumer.close()
                logger.info("Kafka consumer closed")
                
    def _attempt_kafka_reconnect(self, max_attempts: int = 5, backoff_factor: float = 2.0) -> bool:
        """Attempt to reconnect to Kafka after a connection failure.
        
        Args:
            max_attempts: Maximum number of reconnection attempts
            backoff_factor: Backoff factor for retries (in seconds)
            
        Returns:
            bool: True if reconnection was successful, False otherwise
        """
        if not self.running:
            return False
            
        # Close existing consumer
        if self.kafka_consumer:
            try:
                self.kafka_consumer.close()
            except:
                pass
                
        logger.info("Attempting to reconnect to Kafka")
        
        for attempt in range(max_attempts):
            logger.info(f"Reconnection attempt {attempt + 1}/{max_attempts}")
            
            if self.connect_kafka():
                # Restart consumer thread
                self.kafka_thread = threading.Thread(target=self._kafka_consumer_loop)
                self.kafka_thread.daemon = True
                self.kafka_thread.start()
                
                logger.info("Successfully reconnected to Kafka")
                return True
                
            # Exponential backoff
            sleep_time = backoff_factor * (2 ** attempt)
            logger.info(f"Reconnection failed, waiting {sleep_time:.2f}s before next attempt")
            time.sleep(sleep_time)
            
        logger.error(f"Failed to reconnect to Kafka after {max_attempts} attempts")
        self.running = False
        return False
        
    def stop_kafka_consumer(self) -> None:
        """Stop the Kafka consumer thread."""
        if not self.running:
            return
            
        logger.info("Stopping Kafka consumer")
        self.running = False
        
        if self.kafka_thread and self.kafka_thread.is_alive():
            self.kafka_thread.join(timeout=10)
            
        if self.kafka_consumer:
            try:
                self.kafka_consumer.close()
            except:
                pass
                
        logger.info("Kafka consumer stopped")
        
    def fetch_historical_trades(
        self,
        start_date: Union[datetime, str],
        end_date: Union[datetime, str],
        asset_class: str = "FIXED_INCOME",
        security_id: Optional[str] = None,
        page_size: int = 500,
        max_pages: int = 100
    ) -> List[Dict[str, Any]]:
        """Fetch historical trade data from the REST API.
        
        Args:
            start_date: Start date for trade data (inclusive)
            end_date: End date for trade data (inclusive)
            asset_class: Asset class filter
            security_id: Optional security ID filter
            page_size: Number of trades to fetch per page
            max_pages: Maximum number of pages to fetch
            
        Returns:
            List[Dict[str, Any]]: List of trade records
        """
        # Convert dates to string format if needed
        if isinstance(start_date, datetime):
            start_date = start_date.strftime('%Y-%m-%d')
        if isinstance(end_date, datetime):
            end_date = end_date.strftime('%Y-%m-%d')
            
        # Check cache for exact match on query parameters
        cache_key = f"{start_date}_{end_date}_{asset_class}_{security_id}"
        if cache_key in self.cache and datetime.now().timestamp() < self.cache_expiry.get(cache_key, 0):
            self.metrics["cache_hits"] += 1
            logger.info(f"Cache hit for historical trades: {cache_key}")
            return self.cache[cache_key]
            
        self.metrics["cache_misses"] += 1
        
        # Prepare request parameters
        params = {
            "startDate": start_date,
            "endDate": end_date,
            "assetClass": asset_class,
            "pageSize": page_size
        }
        
        if security_id:
            params["securityId"] = security_id
            
        headers = {
            "Authorization": f"Bearer {self.rest_api_key}",
            "Accept": "application/json",
            "Content-Type": "application/json"
        }
        
        all_trades = []
        page = 1
        total_pages = None
        
        start_time = time.time()
        
        while page <= max_pages:
            try:
                params["page"] = page
                
                # Send request with timeout
                response = self.session.get(
                    f"{self.rest_api_url}/trade-history",
                    headers=headers,
                    params=params,
                    timeout=self.connection_timeout
                )
                
                self.metrics["rest_requests"] += 1
                
                # Check for successful response
                response.raise_for_status()
                
                # Parse response
                data = response.json()
                trades = data.get("trades", [])
                all_trades.extend(trades)
                
                # Update total pages if not set
                if total_pages is None and "totalPages" in data:
                    total_pages = data["totalPages"]
                    
                # Check if there are more pages
                if not data.get("hasNextPage", False) or not trades:
                    break
                    
                # Increment page counter
                page += 1
                
                # Rate limiting - be nice to the API
                time.sleep(0.1)
                
            except requests.exceptions.RequestException as e:
                self.metrics["rest_errors"] += 1
                logger.error(f"Error fetching trade history from REST API: {e}")
                break
                
        # Update metrics
        request_time = time.time() - start_time
        self.metrics["rest_total_time"] += request_time
        self.metrics["last_batch_size"] = len(all_trades)
        self.metrics["last_batch_time"] = datetime.now().timestamp()
        
        logger.info(f"Fetched {len(all_trades)} trades from REST API in {request_time:.2f}s")
        
        # Cache the results
        if len(self.cache) >= self.cache_max_size:
            # Remove oldest item (simple LRU implementation)
            oldest_key = min(self.cache_expiry, key=self.cache_expiry.get)
            self.cache.pop(oldest_key, None)
            self.cache_expiry.pop(oldest_key, None)
            
        # Add to cache with expiry time
        self.cache[cache_key] = all_trades
        self.cache_expiry[cache_key] = datetime.now().timestamp() + self.cache_ttl
        
        # Process the trades with the message processor if present
        if self.message_processor:
            successful_count = 0
            for trade in all_trades:
                metadata = {
                    "source": "rest",
                    "batch_query": {
                        "start_date": start_date,
                        "end_date": end_date,
                        "asset_class": asset_class,
                        "security_id": security_id
                    }
                }
                
                if self.message_processor.process_message(trade, metadata):
                    successful_count += 1
                    
            logger.info(f"Processed {successful_count}/{len(all_trades)} trades through message processor")
            
        return all_trades
        
    def fetch_recent_trades(self, hours: int = 24, **kwargs) -> List[Dict[str, Any]]:
        """Fetch trades for the last N hours.
        
        Args:
            hours: Number of hours to look back
            **kwargs: Additional parameters to pass to fetch_historical_trades
            
        Returns:
            List[Dict[str, Any]]: List of trade records
        """
        end_date = datetime.now()
        start_date = end_date - timedelta(hours=hours)
        
        return self.fetch_historical_trades(
            start_date=start_date,
            end_date=end_date,
            **kwargs
        )
        
    def fetch_daily_trades(self, date: Optional[Union[datetime, str]] = None, **kwargs) -> List[Dict[str, Any]]:
        """Fetch trades for a specific day.
        
        Args:
            date: Date to fetch trades for (defaults to yesterday)
            **kwargs: Additional parameters to pass to fetch_historical_trades
            
        Returns:
            List[Dict[str, Any]]: List of trade records
        """
        if date is None:
            # Default to yesterday
            date = datetime.now() - timedelta(days=1)
            
        if isinstance(date, datetime):
            date_str = date.strftime('%Y-%m-%d')
        else:
            date_str = date
            
        return self.fetch_historical_trades(
            start_date=date_str,
            end_date=date_str,
            **kwargs
        )
        
    def clear_cache(self) -> None:
        """Clear the cache of fetched trade data."""
        self.cache = {}
        self.cache_expiry = {}
        logger.info("Trade history cache cleared")
        
    def health_check(self) -> Dict[str, Any]:
        """Check the health of the connector.
        
        Returns:
            Dict[str, Any]: Health status information
        """
        kafka_status = "connected" if self.kafka_consumer and self.running else "disconnected"
        
        # Check REST API connectivity
        rest_status = "unknown"
        try:
            # Simple ping/health endpoint
            response = self.session.get(
                f"{self.rest_api_url}/health",
                headers={"Authorization": f"Bearer {self.rest_api_key}"},
                timeout=5
            )
            rest_status = "connected" if response.status_code == 200 else "error"
        except:
            rest_status = "error"
            
        health_info = {
            "kafka_status": kafka_status,
            "rest_status": rest_status,
            "metrics": self.metrics
        }
        
        return health_info
        
    def __del__(self) -> None:
        """Clean up resources when the object is destroyed."""
        self.stop_kafka_consumer()
        
        if self.session:
            self.session.close() 