"""Base interface for feature stores.

This module defines the core interfaces and abstract base classes for all feature 
store adapters, ensuring consistent behavior across different storage backends.
"""

import logging
from abc import ABC, abstractmethod
from datetime import datetime
from typing import Any, Dict, List, Optional, Tuple, Union

# Configure logging
logger = logging.getLogger(__name__)


class FeatureStore(ABC):
    """Abstract base class for all feature store implementations."""
    
    @abstractmethod
    def connect(self) -> bool:
        """Connect to the feature store.
        
        Returns:
            bool: True if connection successful, False otherwise
        """
        pass
    
    @abstractmethod
    def disconnect(self) -> bool:
        """Disconnect from the feature store.
        
        Returns:
            bool: True if disconnection successful, False otherwise
        """
        pass
    
    @abstractmethod
    def write_feature(
        self, 
        feature_name: str, 
        feature_value: Any,
        entity_id: Optional[str] = None,
        timestamp: Optional[datetime] = None,
        metadata: Optional[Dict[str, Any]] = None
    ) -> bool:
        """Write a feature value to the feature store.
        
        Args:
            feature_name: Name of the feature
            feature_value: Value of the feature
            entity_id: Optional ID of the entity associated with this feature
            timestamp: Optional timestamp for the feature value
            metadata: Optional metadata associated with this feature value
            
        Returns:
            bool: True if write successful, False otherwise
        """
        pass
    
    @abstractmethod
    def read_feature(
        self,
        feature_name: str,
        entity_id: Optional[str] = None,
        timestamp: Optional[datetime] = None
    ) -> Tuple[Any, Dict[str, Any]]:
        """Read a feature value from the feature store.
        
        Args:
            feature_name: Name of the feature
            entity_id: Optional ID of the entity associated with this feature
            timestamp: Optional timestamp for point-in-time feature retrieval
            
        Returns:
            Tuple[Any, Dict[str, Any]]: Feature value and metadata
        """
        pass
    
    @abstractmethod
    def read_feature_history(
        self,
        feature_name: str,
        entity_id: Optional[str] = None,
        start_time: Optional[datetime] = None,
        end_time: Optional[datetime] = None,
        limit: Optional[int] = None
    ) -> List[Tuple[datetime, Any, Dict[str, Any]]]:
        """Read historical feature values from the feature store.
        
        Args:
            feature_name: Name of the feature
            entity_id: Optional ID of the entity associated with this feature
            start_time: Optional start time for the history (inclusive)
            end_time: Optional end time for the history (inclusive)
            limit: Optional maximum number of values to return
            
        Returns:
            List[Tuple[datetime, Any, Dict[str, Any]]]: List of (timestamp, value, metadata) tuples
        """
        pass
    
    @abstractmethod
    def read_features(
        self,
        feature_names: List[str],
        entity_id: Optional[str] = None,
        timestamp: Optional[datetime] = None
    ) -> Dict[str, Tuple[Any, Dict[str, Any]]]:
        """Read multiple feature values from the feature store.
        
        Args:
            feature_names: List of feature names
            entity_id: Optional ID of the entity associated with these features
            timestamp: Optional timestamp for point-in-time feature retrieval
            
        Returns:
            Dict[str, Tuple[Any, Dict[str, Any]]]: Dict mapping feature names to (value, metadata) tuples
        """
        pass
    
    @abstractmethod
    def delete_feature(
        self,
        feature_name: str,
        entity_id: Optional[str] = None,
        timestamp: Optional[datetime] = None
    ) -> bool:
        """Delete a feature value from the feature store.
        
        Args:
            feature_name: Name of the feature
            entity_id: Optional ID of the entity associated with this feature
            timestamp: Optional timestamp for the feature value to delete
            
        Returns:
            bool: True if deletion successful, False otherwise
        """
        pass
    
    @abstractmethod
    def list_features(
        self,
        entity_id: Optional[str] = None,
        prefix: Optional[str] = None
    ) -> List[str]:
        """List available features in the feature store.
        
        Args:
            entity_id: Optional ID of the entity to filter features
            prefix: Optional prefix to filter feature names
            
        Returns:
            List[str]: List of feature names
        """
        pass
    
    @abstractmethod
    def health_check(self) -> Dict[str, Any]:
        """Check the health of the feature store.
        
        Returns:
            Dict[str, Any]: Health check information including connection status and metrics
        """
        pass
    
    def __enter__(self):
        """Enter context manager, connecting to the feature store.
        
        Returns:
            FeatureStore: Self reference for context manager
        """
        self.connect()
        return self
    
    def __exit__(self, exc_type, exc_val, exc_tb):
        """Exit context manager, disconnecting from the feature store."""
        self.disconnect() 