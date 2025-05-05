"""Redis implementation of the feature store interface.

This module provides a Redis-based implementation of the feature store interface,
optimized for low-latency feature storage and retrieval.
"""

import json
import logging
import pickle
from datetime import datetime
from typing import Any, Dict, List, Optional, Tuple, Union

import redis
from redis.exceptions import RedisError

from .base_store import FeatureStore

# Configure logging
logger = logging.getLogger(__name__)


class RedisFeatureStore(FeatureStore):
    """Redis implementation of the feature store interface.
    
    This implementation uses Redis Hash objects for storing the latest feature values 
    and Redis Sorted Sets for storing historical time series data.
    """
    
    def __init__(
        self,
        host: str = "localhost",
        port: int = 6379,
        db: int = 0,
        password: Optional[str] = None,
        socket_timeout: float = 5.0,
        encoding: str = "utf-8",
        decode_responses: bool = False,
        feature_prefix: str = "feature:",
        history_prefix: str = "history:",
        max_pool_size: int = 10,
        max_ttl: Optional[int] = None
    ):
        """Initialize the Redis feature store.
        
        Args:
            host: Redis server host
            port: Redis server port
            db: Redis database number
            password: Optional password for Redis authentication
            socket_timeout: Timeout for Redis socket operations (in seconds)
            encoding: Character encoding for Redis commands
            decode_responses: Whether to decode Redis responses
            feature_prefix: Prefix for feature keys
            history_prefix: Prefix for history keys
            max_pool_size: Maximum number of connections in the Redis connection pool
            max_ttl: Maximum TTL for feature values (in seconds, None for no TTL)
        """
        self.host = host
        self.port = port
        self.db = db
        self.password = password
        self.socket_timeout = socket_timeout
        self.encoding = encoding
        self.decode_responses = decode_responses
        self.feature_prefix = feature_prefix
        self.history_prefix = history_prefix
        self.max_pool_size = max_pool_size
        self.max_ttl = max_ttl
        
        self.client = None
        self.connection_pool = None
        self.is_connected = False
        
    def connect(self) -> bool:
        """Connect to Redis.
        
        Returns:
            bool: True if connection successful, False otherwise
        """
        try:
            # Create connection pool
            self.connection_pool = redis.ConnectionPool(
                host=self.host,
                port=self.port,
                db=self.db,
                password=self.password,
                socket_timeout=self.socket_timeout,
                encoding=self.encoding,
                decode_responses=self.decode_responses,
                max_connections=self.max_pool_size
            )
            
            # Create Redis client
            self.client = redis.Redis(connection_pool=self.connection_pool)
            
            # Test connection with ping
            self.client.ping()
            
            self.is_connected = True
            logger.info(f"Connected to Redis at {self.host}:{self.port}")
            return True
            
        except RedisError as e:
            logger.error(f"Failed to connect to Redis at {self.host}:{self.port}: {e}")
            self.client = None
            self.connection_pool = None
            self.is_connected = False
            return False
            
    def disconnect(self) -> bool:
        """Disconnect from Redis.
        
        Returns:
            bool: True if disconnection successful, False otherwise
        """
        if not self.is_connected:
            return True
            
        try:
            if self.connection_pool:
                self.connection_pool.disconnect()
                
            self.client = None
            self.connection_pool = None
            self.is_connected = False
            
            logger.info(f"Disconnected from Redis at {self.host}:{self.port}")
            return True
            
        except RedisError as e:
            logger.error(f"Error disconnecting from Redis: {e}")
            return False
            
    def _get_feature_key(self, feature_name: str, entity_id: Optional[str] = None) -> str:
        """Get the Redis key for a feature.
        
        Args:
            feature_name: Name of the feature
            entity_id: Optional ID of the entity associated with this feature
            
        Returns:
            str: Redis key
        """
        if entity_id:
            return f"{self.feature_prefix}{entity_id}:{feature_name}"
        else:
            return f"{self.feature_prefix}{feature_name}"
            
    def _get_history_key(self, feature_name: str, entity_id: Optional[str] = None) -> str:
        """Get the Redis key for feature history.
        
        Args:
            feature_name: Name of the feature
            entity_id: Optional ID of the entity associated with this feature
            
        Returns:
            str: Redis key
        """
        if entity_id:
            return f"{self.history_prefix}{entity_id}:{feature_name}"
        else:
            return f"{self.history_prefix}{feature_name}"
            
    def _serialize_value(self, value: Any) -> bytes:
        """Serialize a value for storage in Redis.
        
        Args:
            value: Value to serialize
            
        Returns:
            bytes: Serialized value
        """
        try:
            # For simple types, use JSON serialization
            if isinstance(value, (int, float, str, bool, list, dict)) and not isinstance(value, bytes):
                return json.dumps(value).encode()
            # For complex types, use pickle
            else:
                return pickle.dumps(value)
        except (TypeError, pickle.PickleError) as e:
            logger.error(f"Error serializing value: {e}")
            # Fallback to string representation
            return str(value).encode()
            
    def _deserialize_value(self, serialized_value: bytes) -> Any:
        """Deserialize a value from Redis storage.
        
        Args:
            serialized_value: Serialized value
            
        Returns:
            Any: Deserialized value
        """
        if not serialized_value:
            return None
            
        try:
            # Try JSON first (for simple types)
            return json.loads(serialized_value.decode())
        except (json.JSONDecodeError, UnicodeDecodeError):
            try:
                # Fall back to pickle for complex types
                return pickle.loads(serialized_value)
            except pickle.PickleError as e:
                logger.error(f"Error deserializing value: {e}")
                # Return raw bytes as last resort
                return serialized_value
                
    def _serialize_metadata(self, metadata: Dict[str, Any]) -> Dict[str, bytes]:
        """Serialize metadata for storage in Redis.
        
        Args:
            metadata: Metadata to serialize
            
        Returns:
            Dict[str, bytes]: Serialized metadata
        """
        serialized = {}
        for key, value in metadata.items():
            serialized[key] = self._serialize_value(value)
        return serialized
        
    def _deserialize_metadata(self, serialized_metadata: Dict[str, bytes]) -> Dict[str, Any]:
        """Deserialize metadata from Redis storage.
        
        Args:
            serialized_metadata: Serialized metadata
            
        Returns:
            Dict[str, Any]: Deserialized metadata
        """
        metadata = {}
        for key, value in serialized_metadata.items():
            metadata[key] = self._deserialize_value(value)
        return metadata
        
    def write_feature(
        self, 
        feature_name: str, 
        feature_value: Any,
        entity_id: Optional[str] = None,
        timestamp: Optional[datetime] = None,
        metadata: Optional[Dict[str, Any]] = None
    ) -> bool:
        """Write a feature value to Redis.
        
        Args:
            feature_name: Name of the feature
            feature_value: Value of the feature
            entity_id: Optional ID of the entity associated with this feature
            timestamp: Optional timestamp for the feature value
            metadata: Optional metadata associated with this feature value
            
        Returns:
            bool: True if write successful, False otherwise
        """
        if not self.is_connected or not self.client:
            logger.error("Not connected to Redis")
            return False
            
        try:
            # Prepare keys
            feature_key = self._get_feature_key(feature_name, entity_id)
            history_key = self._get_history_key(feature_name, entity_id)
            
            # Prepare timestamp
            if timestamp is None:
                timestamp = datetime.now()
            timestamp_ms = int(timestamp.timestamp() * 1000)
            
            # Prepare metadata
            if metadata is None:
                metadata = {}
            metadata['timestamp'] = timestamp.isoformat()
            
            # Serialize value and metadata
            serialized_value = self._serialize_value(feature_value)
            serialized_metadata = self._serialize_metadata(metadata)
            
            # Start pipeline
            pipeline = self.client.pipeline(transaction=True)
            
            # Store latest value and metadata in hash
            pipeline.hset(feature_key, 'value', serialized_value)
            for meta_key, meta_value in serialized_metadata.items():
                pipeline.hset(feature_key, f"meta:{meta_key}", meta_value)
                
            # Store in history (sorted set by timestamp)
            history_value = self._serialize_value({
                'value': feature_value,
                'metadata': metadata
            })
            pipeline.zadd(history_key, {history_value: timestamp_ms})
            
            # Set TTL if configured
            if self.max_ttl:
                pipeline.expire(feature_key, self.max_ttl)
                pipeline.expire(history_key, self.max_ttl)
                
            # Execute pipeline
            pipeline.execute()
            
            logger.debug(f"Wrote feature {feature_name} for entity {entity_id} to Redis")
            return True
            
        except RedisError as e:
            logger.error(f"Error writing feature to Redis: {e}")
            return False
            
    def read_feature(
        self,
        feature_name: str,
        entity_id: Optional[str] = None,
        timestamp: Optional[datetime] = None
    ) -> Tuple[Any, Dict[str, Any]]:
        """Read a feature value from Redis.
        
        Args:
            feature_name: Name of the feature
            entity_id: Optional ID of the entity associated with this feature
            timestamp: Optional timestamp for point-in-time feature retrieval
            
        Returns:
            Tuple[Any, Dict[str, Any]]: Feature value and metadata
        """
        if not self.is_connected or not self.client:
            logger.error("Not connected to Redis")
            return None, {}
            
        try:
            # If timestamp is provided, use history to get point-in-time value
            if timestamp is not None:
                history_data = self.read_feature_history(
                    feature_name=feature_name,
                    entity_id=entity_id,
                    end_time=timestamp,
                    limit=1
                )
                
                if history_data:
                    # Return the most recent value before or at the timestamp
                    ts, value, meta = history_data[0]
                    return value, meta
                else:
                    logger.warning(f"No historical data found for feature {feature_name} at {timestamp}")
                    return None, {}
            
            # Otherwise get the latest value
            feature_key = self._get_feature_key(feature_name, entity_id)
            
            # Get all hash fields
            hash_data = self.client.hgetall(feature_key)
            
            if not hash_data:
                logger.warning(f"Feature {feature_name} for entity {entity_id} not found in Redis")
                return None, {}
                
            # Extract value and metadata
            serialized_value = hash_data.get(b'value')
            value = self._deserialize_value(serialized_value) if serialized_value else None
            
            # Extract metadata
            metadata = {}
            for key, val in hash_data.items():
                if key != b'value' and key.startswith(b'meta:'):
                    meta_key = key.decode().replace('meta:', '')
                    metadata[meta_key] = self._deserialize_value(val)
                    
            return value, metadata
            
        except RedisError as e:
            logger.error(f"Error reading feature from Redis: {e}")
            return None, {}
            
    def read_feature_history(
        self,
        feature_name: str,
        entity_id: Optional[str] = None,
        start_time: Optional[datetime] = None,
        end_time: Optional[datetime] = None,
        limit: Optional[int] = None
    ) -> List[Tuple[datetime, Any, Dict[str, Any]]]:
        """Read historical feature values from Redis.
        
        Args:
            feature_name: Name of the feature
            entity_id: Optional ID of the entity associated with this feature
            start_time: Optional start time for the history (inclusive)
            end_time: Optional end time for the history (inclusive)
            limit: Optional maximum number of values to return
            
        Returns:
            List[Tuple[datetime, Any, Dict[str, Any]]]: List of (timestamp, value, metadata) tuples
        """
        if not self.is_connected or not self.client:
            logger.error("Not connected to Redis")
            return []
            
        try:
            history_key = self._get_history_key(feature_name, entity_id)
            
            # Convert timestamps to milliseconds
            min_score = '-inf'
            max_score = '+inf'
            
            if start_time:
                min_score = int(start_time.timestamp() * 1000)
            if end_time:
                max_score = int(end_time.timestamp() * 1000)
                
            # Get data from sorted set with scores (timestamps)
            if limit is not None:
                # ZREVRANGEBYSCORE for newest-first
                result = self.client.zrevrangebyscore(
                    history_key,
                    max_score,
                    min_score,
                    start=0,
                    num=limit,
                    withscores=True
                )
            else:
                # Get all matching data
                result = self.client.zrevrangebyscore(
                    history_key,
                    max_score,
                    min_score,
                    withscores=True
                )
                
            # Process results
            history = []
            for entry_bytes, score in result:
                # Convert score (timestamp) back to datetime
                entry_time = datetime.fromtimestamp(score / 1000.0)
                
                # Deserialize entry
                entry = self._deserialize_value(entry_bytes)
                
                if isinstance(entry, dict):
                    value = entry.get('value')
                    metadata = entry.get('metadata', {})
                else:
                    # Handle older format
                    value = entry
                    metadata = {'timestamp': entry_time.isoformat()}
                    
                history.append((entry_time, value, metadata))
                
            return history
            
        except RedisError as e:
            logger.error(f"Error reading feature history from Redis: {e}")
            return []
            
    def read_features(
        self,
        feature_names: List[str],
        entity_id: Optional[str] = None,
        timestamp: Optional[datetime] = None
    ) -> Dict[str, Tuple[Any, Dict[str, Any]]]:
        """Read multiple feature values from Redis.
        
        Args:
            feature_names: List of feature names
            entity_id: Optional ID of the entity associated with these features
            timestamp: Optional timestamp for point-in-time feature retrieval
            
        Returns:
            Dict[str, Tuple[Any, Dict[str, Any]]]: Dict mapping feature names to (value, metadata) tuples
        """
        results = {}
        
        # For timestamp queries, we might be able to optimize with pipelining
        # but for now, just make individual calls
        for feature_name in feature_names:
            value, metadata = self.read_feature(
                feature_name=feature_name,
                entity_id=entity_id,
                timestamp=timestamp
            )
            results[feature_name] = (value, metadata)
            
        return results
        
    def delete_feature(
        self,
        feature_name: str,
        entity_id: Optional[str] = None,
        timestamp: Optional[datetime] = None
    ) -> bool:
        """Delete a feature value from Redis.
        
        Args:
            feature_name: Name of the feature
            entity_id: Optional ID of the entity associated with this feature
            timestamp: Optional timestamp for the feature value to delete
            
        Returns:
            bool: True if deletion successful, False otherwise
        """
        if not self.is_connected or not self.client:
            logger.error("Not connected to Redis")
            return False
            
        try:
            feature_key = self._get_feature_key(feature_name, entity_id)
            history_key = self._get_history_key(feature_name, entity_id)
            
            if timestamp is not None:
                # Delete specific timestamp from history
                timestamp_ms = int(timestamp.timestamp() * 1000)
                
                # Find entries at this exact timestamp
                entries = self.client.zrangebyscore(
                    history_key,
                    timestamp_ms,
                    timestamp_ms
                )
                
                # Use pipeline for atomic operation
                pipeline = self.client.pipeline(transaction=True)
                
                # Remove entries at this timestamp
                for entry in entries:
                    pipeline.zrem(history_key, entry)
                    
                pipeline.execute()
                
                return True
            else:
                # Delete all data for this feature
                pipeline = self.client.pipeline(transaction=True)
                pipeline.delete(feature_key)
                pipeline.delete(history_key)
                pipeline.execute()
                
                logger.info(f"Deleted feature {feature_name} for entity {entity_id} from Redis")
                return True
                
        except RedisError as e:
            logger.error(f"Error deleting feature from Redis: {e}")
            return False
            
    def list_features(
        self,
        entity_id: Optional[str] = None,
        prefix: Optional[str] = None
    ) -> List[str]:
        """List available features in Redis.
        
        Args:
            entity_id: Optional ID of the entity to filter features
            prefix: Optional prefix to filter feature names
            
        Returns:
            List[str]: List of feature names
        """
        if not self.is_connected or not self.client:
            logger.error("Not connected to Redis")
            return []
            
        try:
            # Construct search pattern
            if entity_id:
                pattern = f"{self.feature_prefix}{entity_id}:*"
            else:
                pattern = f"{self.feature_prefix}*"
                
            # Get keys matching pattern
            keys = self.client.keys(pattern)
            
            # Extract feature names from keys
            features = []
            for key in keys:
                key_str = key.decode() if isinstance(key, bytes) else key
                
                # Extract feature name from key
                if entity_id:
                    # Format: feature_prefix + entity_id + ":" + feature_name
                    entity_prefix = f"{self.feature_prefix}{entity_id}:"
                    feature_name = key_str[len(entity_prefix):]
                else:
                    # Format: feature_prefix + feature_name
                    # OR: feature_prefix + entity_id + ":" + feature_name
                    feature_parts = key_str[len(self.feature_prefix):].split(":", 1)
                    if len(feature_parts) > 1:
                        # This is an entity-specific feature
                        feature_name = feature_parts[1]
                    else:
                        # This is a global feature
                        feature_name = feature_parts[0]
                        
                # Apply prefix filter if provided
                if prefix and not feature_name.startswith(prefix):
                    continue
                    
                features.append(feature_name)
                
            return features
            
        except RedisError as e:
            logger.error(f"Error listing features from Redis: {e}")
            return []
            
    def health_check(self) -> Dict[str, Any]:
        """Check the health of the Redis feature store.
        
        Returns:
            Dict[str, Any]: Health check information including connection status and metrics
        """
        health_info = {
            "store_type": "redis",
            "connected": self.is_connected,
            "host": self.host,
            "port": self.port,
            "metrics": {}
        }
        
        if not self.is_connected or not self.client:
            return health_info
            
        try:
            # Test connection
            ping_result = self.client.ping()
            health_info["ping"] = ping_result
            
            # Get info about Redis server
            info = self.client.info()
            
            # Add relevant metrics
            health_info["metrics"] = {
                "connected_clients": info.get("connected_clients"),
                "used_memory_human": info.get("used_memory_human"),
                "total_connections_received": info.get("total_connections_received"),
                "uptime_in_seconds": info.get("uptime_in_seconds")
            }
            
            # Count keys
            feature_count = len(self.client.keys(f"{self.feature_prefix}*"))
            history_count = len(self.client.keys(f"{self.history_prefix}*"))
            
            health_info["key_counts"] = {
                "features": feature_count,
                "history": history_count
            }
            
            return health_info
            
        except RedisError as e:
            logger.error(f"Error checking Redis health: {e}")
            health_info["error"] = str(e)
            health_info["connected"] = False
            return health_info 