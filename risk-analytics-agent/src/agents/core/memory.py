"""
Memory components for agents.
Provides different memory mechanisms for storing context and history.
"""

import datetime
from typing import Dict, List, Any, Optional, Union, Tuple
import numpy as np
import json
import logging
from abc import ABC, abstractmethod
import os
import time
import pickle
import redis

logger = logging.getLogger(__name__)

class Memory(ABC):
    """Abstract base class for memory implementations."""
    
    @abstractmethod
    def store(self, key: str, value: Any, metadata: Optional[Dict] = None) -> None:
        """Store an item in memory."""
        pass
    
    @abstractmethod
    def retrieve(self, key: str, default: Any = None) -> Any:
        """Retrieve an item from memory."""
        pass
    
    @abstractmethod
    def forget(self, key: str) -> bool:
        """Remove an item from memory."""
        pass
    
    @abstractmethod
    def clear(self) -> None:
        """Clear all items from memory."""
        pass
    
    @abstractmethod
    def get_keys(self) -> List[str]:
        """Get all keys in memory."""
        pass
    
    @abstractmethod
    def get_size(self) -> int:
        """Get the number of items in memory."""
        pass


class SimpleMemory(Memory):
    """
    Simple in-memory storage with timestamps.
    """
    
    def __init__(self):
        self._storage = {}
    
    def store(self, key: str, value: Any, metadata: Optional[Dict] = None) -> None:
        """
        Store an item in memory.
        
        Args:
            key: Unique identifier for the item
            value: The value to store
            metadata: Additional metadata for the item
        """
        if metadata is None:
            metadata = {}
        
        self._storage[key] = {
            "value": value,
            "timestamp": datetime.datetime.now(),
            "metadata": metadata
        }
        logger.debug(f"Stored in memory: {key}")
    
    def retrieve(
        self, 
        key: str, 
        default: Any = None, 
        max_age: Optional[datetime.timedelta] = None
    ) -> Any:
        """
        Retrieve an item from memory.
        
        Args:
            key: Key to retrieve
            default: Default value if key not found or expired
            max_age: Maximum age for the item to be valid
            
        Returns:
            The stored value or default if not found/expired
        """
        if key not in self._storage:
            return default
        
        item = self._storage[key]
        
        # Check for expiry
        if max_age is not None:
            age = datetime.datetime.now() - item["timestamp"]
            if age > max_age:
                logger.debug(f"Memory item {key} has expired (age: {age}, max: {max_age})")
                return default
        
        return item["value"]
    
    def retrieve_with_metadata(
        self, 
        key: str, 
        default: Any = None,
        max_age: Optional[datetime.timedelta] = None
    ) -> Tuple[Any, Dict]:
        """
        Retrieve an item with its metadata.
        
        Returns:
            Tuple of (value, metadata dict)
        """
        if key not in self._storage:
            return default, {}
        
        item = self._storage[key]
        
        # Check for expiry
        if max_age is not None:
            age = datetime.datetime.now() - item["timestamp"]
            if age > max_age:
                return default, {}
        
        return item["value"], {
            "timestamp": item["timestamp"],
            "age": (datetime.datetime.now() - item["timestamp"]).total_seconds(),
            **item["metadata"]
        }
    
    def forget(self, key: str) -> bool:
        """
        Remove an item from memory.
        
        Returns:
            True if item was removed, False if not found
        """
        if key in self._storage:
            del self._storage[key]
            logger.debug(f"Removed from memory: {key}")
            return True
        return False
    
    def clear(self) -> None:
        """Clear all items from memory."""
        self._storage = {}
        logger.debug("Memory cleared")
    
    def get_keys(self) -> List[str]:
        """Get all keys in memory."""
        return list(self._storage.keys())
    
    def get_size(self) -> int:
        """Get the number of items in memory."""
        return len(self._storage)
    
    def get_recent_items(
        self, 
        n: int = 10, 
        max_age: Optional[datetime.timedelta] = None
    ) -> List[Tuple[str, Any, datetime.datetime]]:
        """
        Get the n most recent items in memory.
        
        Args:
            n: Maximum number of items to return
            max_age: Maximum age of items to consider
            
        Returns:
            List of (key, value, timestamp) tuples
        """
        items = []
        now = datetime.datetime.now()
        
        for key, item in self._storage.items():
            timestamp = item["timestamp"]
            
            # Skip if too old
            if max_age is not None and (now - timestamp) > max_age:
                continue
            
            items.append((key, item["value"], timestamp))
        
        # Sort by timestamp (newest first)
        items.sort(key=lambda x: x[2], reverse=True)
        
        # Return at most n items
        return items[:n]


class VectorMemory(Memory):
    """
    Memory with vector embeddings for semantic retrieval.
    Requires an embedding function to convert values to vectors.
    """
    
    def __init__(self, embedding_function):
        """
        Initialize vector memory.
        
        Args:
            embedding_function: Function that converts a value to a vector
        """
        self._storage = {}
        self._embedding_function = embedding_function
        self._vector_storage = {}
    
    def store(self, key: str, value: Any, metadata: Optional[Dict] = None) -> None:
        """
        Store an item with its vector embedding.
        
        Args:
            key: Unique identifier for the item
            value: The value to store
            metadata: Additional metadata for the item
        """
        if metadata is None:
            metadata = {}
        
        # Store the value
        self._storage[key] = {
            "value": value,
            "timestamp": datetime.datetime.now(),
            "metadata": metadata
        }
        
        # Compute and store the embedding
        try:
            if isinstance(value, (str, list, dict)):
                # For strings, lists and dicts, create embedding
                embedding = self._embedding_function(value)
                self._vector_storage[key] = embedding
            elif hasattr(value, "__str__"):
                # For other objects with string representation
                embedding = self._embedding_function(str(value))
                self._vector_storage[key] = embedding
        except Exception as e:
            logger.warning(f"Failed to create embedding for {key}: {e}")
    
    def retrieve(self, key: str, default: Any = None) -> Any:
        """Retrieve an item by key."""
        if key not in self._storage:
            return default
        return self._storage[key]["value"]
    
    def forget(self, key: str) -> bool:
        """Remove an item from memory."""
        if key in self._storage:
            del self._storage[key]
            if key in self._vector_storage:
                del self._vector_storage[key]
            return True
        return False
    
    def clear(self) -> None:
        """Clear all items from memory."""
        self._storage = {}
        self._vector_storage = {}
    
    def get_keys(self) -> List[str]:
        """Get all keys in memory."""
        return list(self._storage.keys())
    
    def get_size(self) -> int:
        """Get the number of items in memory."""
        return len(self._storage)
    
    def similar_to_text(
        self, 
        query: str, 
        n: int = 5, 
        threshold: float = 0.0
    ) -> List[Tuple[str, Any, float]]:
        """
        Find items with embeddings similar to the query text.
        
        Args:
            query: The query text
            n: Maximum number of results
            threshold: Minimum similarity score (0-1)
            
        Returns:
            List of (key, value, similarity) tuples
        """
        if not self._vector_storage:
            return []
        
        # Compute query embedding
        query_embedding = self._embedding_function(query)
        
        results = []
        for key, embedding in self._vector_storage.items():
            # Compute cosine similarity
            similarity = self._cosine_similarity(query_embedding, embedding)
            
            if similarity >= threshold:
                results.append((key, self._storage[key]["value"], similarity))
        
        # Sort by similarity (highest first)
        results.sort(key=lambda x: x[2], reverse=True)
        
        # Return at most n results
        return results[:n]
    
    def similar_to_key(
        self, 
        key: str, 
        n: int = 5, 
        threshold: float = 0.0
    ) -> List[Tuple[str, Any, float]]:
        """
        Find items with embeddings similar to the item with the given key.
        
        Args:
            key: Key of the reference item
            n: Maximum number of results
            threshold: Minimum similarity score (0-1)
            
        Returns:
            List of (key, value, similarity) tuples
        """
        if key not in self._vector_storage:
            return []
        
        query_embedding = self._vector_storage[key]
        
        results = []
        for k, embedding in self._vector_storage.items():
            if k == key:
                continue  # Skip the reference item
            
            # Compute cosine similarity
            similarity = self._cosine_similarity(query_embedding, embedding)
            
            if similarity >= threshold:
                results.append((k, self._storage[k]["value"], similarity))
        
        # Sort by similarity (highest first)
        results.sort(key=lambda x: x[2], reverse=True)
        
        # Return at most n results
        return results[:n]
    
    @staticmethod
    def _cosine_similarity(a, b):
        """Compute cosine similarity between vectors a and b."""
        return np.dot(a, b) / (np.linalg.norm(a) * np.linalg.norm(b))


class MemoryFactory:
    """Factory for creating different memory types."""
    
    @staticmethod
    def create_memory(memory_type: str, **kwargs) -> Memory:
        """
        Create a memory instance based on type.
        
        Args:
            memory_type: Type of memory to create
            **kwargs: Additional arguments for the memory constructor
            
        Returns:
            Memory instance
        """
        if memory_type.lower() == "simple":
            return SimpleMemory()
        elif memory_type.lower() == "vector":
            if "embedding_function" not in kwargs:
                raise ValueError("embedding_function is required for vector memory")
            return VectorMemory(kwargs["embedding_function"])
        else:
            raise ValueError(f"Unknown memory type: {memory_type}")


class PersistentMemory(Memory):
    """
    Memory that persists to disk.
    Wraps another memory implementation and adds persistence.
    """
    
    def __init__(self, wrapped_memory: Memory, file_path: str):
        """
        Initialize persistent memory.
        
        Args:
            wrapped_memory: The memory implementation to wrap
            file_path: Path to the file for persistence
        """
        self._memory = wrapped_memory
        self._file_path = file_path
        self._load()
    
    def store(self, key: str, value: Any, metadata: Optional[Dict] = None) -> None:
        """Store an item and save to disk."""
        self._memory.store(key, value, metadata)
        self._save()
    
    def retrieve(self, key: str, default: Any = None) -> Any:
        """Retrieve an item."""
        return self._memory.retrieve(key, default)
    
    def forget(self, key: str) -> bool:
        """Remove an item and save to disk."""
        result = self._memory.forget(key)
        if result:
            self._save()
        return result
    
    def clear(self) -> None:
        """Clear all items and save to disk."""
        self._memory.clear()
        self._save()
    
    def get_keys(self) -> List[str]:
        """Get all keys in memory."""
        return self._memory.get_keys()
    
    def get_size(self) -> int:
        """Get the number of items in memory."""
        return self._memory.get_size()
    
    def _save(self) -> None:
        """Save memory to disk."""
        # Simple implementation for SimpleMemory
        if isinstance(self._memory, SimpleMemory):
            serializable = {}
            for key, item in self._memory._storage.items():
                serializable[key] = {
                    "value": item["value"],
                    "timestamp": item["timestamp"].isoformat(),
                    "metadata": item["metadata"]
                }
            
            with open(self._file_path, 'w') as f:
                json.dump(serializable, f)
            
            logger.debug(f"Memory saved to {self._file_path}")
        else:
            logger.warning(f"Persistence not supported for {type(self._memory)}")
    
    def _load(self) -> None:
        """Load memory from disk."""
        if not isinstance(self._memory, SimpleMemory):
            logger.warning(f"Persistence not supported for {type(self._memory)}")
            return
        
        try:
            with open(self._file_path, 'r') as f:
                serialized = json.load(f)
            
            for key, item in serialized.items():
                self._memory._storage[key] = {
                    "value": item["value"],
                    "timestamp": datetime.datetime.fromisoformat(item["timestamp"]),
                    "metadata": item["metadata"]
                }
            
            logger.debug(f"Memory loaded from {self._file_path}")
        except FileNotFoundError:
            logger.info(f"No saved memory found at {self._file_path}")
        except Exception as e:
            logger.error(f"Error loading memory from {self._file_path}: {e}")


class AgentMemory:
    """
    Memory system for agents to store and retrieve information.
    
    Provides three types of memory:
    1. Short-term: For temporary storage within a session or request
    2. Long-term: For persistent storage across sessions
    3. Episodic: For storing complete interaction episodes
    """
    
    def __init__(self, 
                 short_term_config: Dict[str, Any] = None,
                 long_term_config: Dict[str, Any] = None,
                 episodic_config: Dict[str, Any] = None):
        """
        Initialize the agent memory system.
        
        Args:
            short_term_config: Configuration for short-term memory
            long_term_config: Configuration for long-term memory
            episodic_config: Configuration for episodic memory
        """
        self.logger = logging.getLogger('agent_memory')
        
        # Initialize short-term memory
        self.short_term_config = short_term_config or {}
        self.short_term_memory = {}
        self.short_term_type = self.short_term_config.get('type', 'local')
        self.short_term_ttl = self.short_term_config.get('ttl_seconds', 3600)  # Default 1 hour
        
        # Initialize Redis connection if needed
        self.redis_client = None
        if self.short_term_type == 'redis' or (long_term_config and long_term_config.get('type') == 'redis'):
            self._initialize_redis()
        
        # Initialize long-term memory
        self.long_term_config = long_term_config or {}
        self.long_term_type = self.long_term_config.get('type', 'local')
        self.long_term_memory = {}
        
        # Initialize episodic memory
        self.episodic_config = episodic_config or {}
        self.episodic_enabled = self.episodic_config.get('enabled', False)
        self.max_episodes = self.episodic_config.get('max_episodes', 100)
        self.episodes = []
        self.current_episode = None
        
        self.logger.info("Memory system initialized")
    
    def _initialize_redis(self) -> None:
        """Initialize Redis connection for memory storage."""
        try:
            # Get Redis configuration from environment or config
            host = os.environ.get('REDIS_HOST', 'localhost')
            port = int(os.environ.get('REDIS_PORT', 6379))
            db = int(os.environ.get('REDIS_DB', 0))
            password = os.environ.get('REDIS_PASSWORD', None)
            
            self.redis_client = redis.Redis(
                host=host,
                port=port,
                db=db,
                password=password,
                decode_responses=False  # Keep binary data for flexibility
            )
            self.logger.info(f"Connected to Redis at {host}:{port}")
        except Exception as e:
            self.logger.error(f"Failed to connect to Redis: {e}")
            self.logger.warning("Falling back to local memory storage")
            self.redis_client = None
            self.short_term_type = 'local'
            self.long_term_type = 'local'
    
    def add_to_short_term(self, key: str, value: Any) -> bool:
        """
        Add item to short-term memory.
        
        Args:
            key: Key for storing the value
            value: Value to store
            
        Returns:
            True if successful, False otherwise
        """
        try:
            # Store with timestamp for TTL management
            memory_item = {
                'value': value,
                'timestamp': time.time(),
                'ttl': self.short_term_ttl
            }
            
            if self.short_term_type == 'redis' and self.redis_client:
                pickled_value = pickle.dumps(memory_item)
                self.redis_client.setex(f"st:{key}", self.short_term_ttl, pickled_value)
            else:
                self.short_term_memory[key] = memory_item
            
            # Add to current episode if episodic memory enabled
            if self.episodic_enabled and self.current_episode is not None:
                self.current_episode['memory_actions'].append({
                    'action': 'add_short_term',
                    'key': key,
                    'timestamp': time.time()
                })
            
            return True
        except Exception as e:
            self.logger.error(f"Error adding to short-term memory: {e}")
            return False
    
    def get_from_short_term(self, key: str, default: Any = None) -> Any:
        """
        Retrieve item from short-term memory.
        
        Args:
            key: Key to retrieve
            default: Default value if key not found or expired
            
        Returns:
            Retrieved value or default
        """
        try:
            if self.short_term_type == 'redis' and self.redis_client:
                pickled_value = self.redis_client.get(f"st:{key}")
                if pickled_value is None:
                    return default
                
                memory_item = pickle.loads(pickled_value)
            else:
                if key not in self.short_term_memory:
                    return default
                
                memory_item = self.short_term_memory[key]
            
            # Check if item has expired
            current_time = time.time()
            if current_time - memory_item['timestamp'] > memory_item['ttl']:
                self.logger.debug(f"Short-term memory item {key} has expired")
                if self.short_term_type != 'redis':  # Redis handles expiration automatically
                    del self.short_term_memory[key]
                return default
            
            # Add to current episode if episodic memory enabled
            if self.episodic_enabled and self.current_episode is not None:
                self.current_episode['memory_actions'].append({
                    'action': 'get_short_term',
                    'key': key,
                    'timestamp': time.time()
                })
            
            return memory_item['value']
        except Exception as e:
            self.logger.error(f"Error retrieving from short-term memory: {e}")
            return default
    
    def add_to_long_term(self, key: str, value: Any) -> bool:
        """
        Add item to long-term memory.
        
        Args:
            key: Key for storing the value
            value: Value to store
            
        Returns:
            True if successful, False otherwise
        """
        try:
            # Store with timestamp
            memory_item = {
                'value': value,
                'timestamp': time.time()
            }
            
            if self.long_term_type == 'redis' and self.redis_client:
                pickled_value = pickle.dumps(memory_item)
                self.redis_client.set(f"lt:{key}", pickled_value)
            elif self.long_term_type == 'pinot':
                # Implementation for Apache Pinot would go here
                # For now, fall back to local storage
                self.long_term_memory[key] = memory_item
            else:
                self.long_term_memory[key] = memory_item
            
            # Add to current episode if episodic memory enabled
            if self.episodic_enabled and self.current_episode is not None:
                self.current_episode['memory_actions'].append({
                    'action': 'add_long_term',
                    'key': key,
                    'timestamp': time.time()
                })
            
            return True
        except Exception as e:
            self.logger.error(f"Error adding to long-term memory: {e}")
            return False
    
    def get_from_long_term(self, key: str, default: Any = None) -> Any:
        """
        Retrieve item from long-term memory.
        
        Args:
            key: Key to retrieve
            default: Default value if key not found
            
        Returns:
            Retrieved value or default
        """
        try:
            if self.long_term_type == 'redis' and self.redis_client:
                pickled_value = self.redis_client.get(f"lt:{key}")
                if pickled_value is None:
                    return default
                
                memory_item = pickle.loads(pickled_value)
            elif self.long_term_type == 'pinot':
                # Implementation for Apache Pinot would go here
                # For now, fall back to local storage
                if key not in self.long_term_memory:
                    return default
                
                memory_item = self.long_term_memory[key]
            else:
                if key not in self.long_term_memory:
                    return default
                
                memory_item = self.long_term_memory[key]
            
            # Add to current episode if episodic memory enabled
            if self.episodic_enabled and self.current_episode is not None:
                self.current_episode['memory_actions'].append({
                    'action': 'get_long_term',
                    'key': key,
                    'timestamp': time.time()
                })
            
            return memory_item['value']
        except Exception as e:
            self.logger.error(f"Error retrieving from long-term memory: {e}")
            return default
    
    def start_episode(self, episode_id: str = None) -> str:
        """
        Start a new memory episode.
        
        Args:
            episode_id: Optional ID for the episode, generated if not provided
            
        Returns:
            Episode ID
        """
        if not self.episodic_enabled:
            self.logger.warning("Episodic memory is disabled")
            return ""
        
        if episode_id is None:
            episode_id = f"episode_{int(time.time())}"
        
        self.current_episode = {
            'id': episode_id,
            'start_time': time.time(),
            'end_time': None,
            'memory_actions': [],
            'metadata': {}
        }
        
        self.logger.info(f"Started new episode: {episode_id}")
        return episode_id
    
    def end_episode(self, metadata: Dict[str, Any] = None) -> bool:
        """
        End the current episode and save it.
        
        Args:
            metadata: Optional metadata to attach to the episode
            
        Returns:
            True if successful, False otherwise
        """
        if not self.episodic_enabled or self.current_episode is None:
            return False
        
        # Update episode with end time and metadata
        self.current_episode['end_time'] = time.time()
        
        if metadata:
            self.current_episode['metadata'] = metadata
        
        # Add to episodes list
        self.episodes.append(self.current_episode)
        
        # Trim episodes list if needed
        if len(self.episodes) > self.max_episodes:
            self.episodes = self.episodes[-self.max_episodes:]
        
        # Clear current episode
        episode_id = self.current_episode['id']
        self.current_episode = None
        
        self.logger.info(f"Ended episode: {episode_id}")
        return True
    
    def get_episode(self, episode_id: str) -> Optional[Dict[str, Any]]:
        """
        Retrieve a specific episode.
        
        Args:
            episode_id: ID of the episode to retrieve
            
        Returns:
            Episode dictionary or None if not found
        """
        if not self.episodic_enabled:
            return None
        
        for episode in self.episodes:
            if episode['id'] == episode_id:
                return episode
        
        return None
    
    def get_episodes(self, count: int = 10) -> List[Dict[str, Any]]:
        """
        Get the most recent episodes.
        
        Args:
            count: Number of episodes to retrieve
            
        Returns:
            List of episode dictionaries
        """
        if not self.episodic_enabled:
            return []
        
        return self.episodes[-count:]
    
    def clear_short_term(self) -> bool:
        """
        Clear all short-term memory.
        
        Returns:
            True if successful, False otherwise
        """
        try:
            if self.short_term_type == 'redis' and self.redis_client:
                # Delete all keys with the short-term prefix
                keys = self.redis_client.keys("st:*")
                if keys:
                    self.redis_client.delete(*keys)
            else:
                self.short_term_memory = {}
            
            return True
        except Exception as e:
            self.logger.error(f"Error clearing short-term memory: {e}")
            return False
    
    def clear_long_term(self) -> bool:
        """
        Clear all long-term memory.
        
        Returns:
            True if successful, False otherwise
        """
        try:
            if self.long_term_type == 'redis' and self.redis_client:
                # Delete all keys with the long-term prefix
                keys = self.redis_client.keys("lt:*")
                if keys:
                    self.redis_client.delete(*keys)
            else:
                self.long_term_memory = {}
            
            return True
        except Exception as e:
            self.logger.error(f"Error clearing long-term memory: {e}")
            return False
    
    def clear_episodes(self) -> bool:
        """
        Clear all episodic memory.
        
        Returns:
            True if successful, False otherwise
        """
        if not self.episodic_enabled:
            return False
        
        self.episodes = []
        self.current_episode = None
        return True
    
    def save(self, path: str) -> bool:
        """
        Save memory state to disk.
        
        Args:
            path: Directory to save memory state
            
        Returns:
            True if successful, False otherwise
        """
        try:
            os.makedirs(path, exist_ok=True)
            
            # Save short-term memory if not using Redis
            if self.short_term_type != 'redis':
                with open(os.path.join(path, 'short_term.pkl'), 'wb') as file:
                    pickle.dump(self.short_term_memory, file)
            
            # Save long-term memory if not using external storage
            if self.long_term_type not in ['redis', 'pinot']:
                with open(os.path.join(path, 'long_term.pkl'), 'wb') as file:
                    pickle.dump(self.long_term_memory, file)
            
            # Save episodic memory
            if self.episodic_enabled:
                with open(os.path.join(path, 'episodes.json'), 'w') as file:
                    # Episodes might contain non-serializable objects, so we need to clean it
                    # This is a simplified approach, in a real implementation we'd need more robust serialization
                    serializable_episodes = []
                    for episode in self.episodes:
                        # Create a simple version for serialization
                        simplified = {
                            'id': episode['id'],
                            'start_time': episode['start_time'],
                            'end_time': episode['end_time'],
                            'memory_actions': [
                                {k: v for k, v in action.items() if isinstance(v, (str, int, float, bool, type(None)))}
                                for action in episode['memory_actions']
                            ],
                            'metadata': {k: v for k, v in episode.get('metadata', {}).items() 
                                        if isinstance(v, (str, int, float, bool, type(None)))}
                        }
                        serializable_episodes.append(simplified)
                    
                    json.dump(serializable_episodes, file)
            
            self.logger.info(f"Memory state saved to {path}")
            return True
        except Exception as e:
            self.logger.error(f"Error saving memory state: {e}")
            return False
    
    def load(self, path: str) -> bool:
        """
        Load memory state from disk.
        
        Args:
            path: Directory containing saved memory state
            
        Returns:
            True if successful, False otherwise
        """
        try:
            # Load short-term memory if not using Redis
            if self.short_term_type != 'redis':
                short_term_path = os.path.join(path, 'short_term.pkl')
                if os.path.exists(short_term_path):
                    with open(short_term_path, 'rb') as file:
                        self.short_term_memory = pickle.load(file)
            
            # Load long-term memory if not using external storage
            if self.long_term_type not in ['redis', 'pinot']:
                long_term_path = os.path.join(path, 'long_term.pkl')
                if os.path.exists(long_term_path):
                    with open(long_term_path, 'rb') as file:
                        self.long_term_memory = pickle.load(file)
            
            # Load episodic memory
            if self.episodic_enabled:
                episodes_path = os.path.join(path, 'episodes.json')
                if os.path.exists(episodes_path):
                    with open(episodes_path, 'r') as file:
                        self.episodes = json.load(file)
            
            self.logger.info(f"Memory state loaded from {path}")
            return True
        except Exception as e:
            self.logger.error(f"Error loading memory state: {e}")
            return False 