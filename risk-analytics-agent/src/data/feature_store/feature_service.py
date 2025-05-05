"""Feature Service for unified access to features from different stores.

This module provides a unified interface for accessing features from different
feature stores, with caching, monitoring, and on-demand feature computation.
"""

import logging
import time
from datetime import datetime, timedelta
from functools import lru_cache
from typing import Any, Dict, List, Optional, Tuple, Union

from .base_store import FeatureStore
from .feature_registry import FeatureDefinition, FeatureRegistry, TimeSeriesFeatureDefinition

# Configure logging
logger = logging.getLogger(__name__)


class FeatureService:
    """Service for unified access to features from different stores."""
    
    def __init__(
        self,
        registry: FeatureRegistry,
        default_store: FeatureStore,
        stores: Optional[Dict[str, FeatureStore]] = None,
        enable_monitoring: bool = True,
        cache_size: int = 1000,
        cache_ttl: int = 300  # 5 minutes in seconds
    ):
        """Initialize the feature service.
        
        Args:
            registry: Feature registry containing feature definitions
            default_store: Default feature store to use
            stores: Optional dictionary mapping store names to store instances
            enable_monitoring: Whether to enable monitoring
            cache_size: Size of the in-memory cache
            cache_ttl: Time-to-live for cached features in seconds
        """
        self.registry = registry
        self.default_store = default_store
        self.stores = stores or {}
        self.stores["default"] = default_store
        self.enable_monitoring = enable_monitoring
        self.cache_size = cache_size
        self.cache_ttl = cache_ttl
        
        # Cache for feature values
        # Note: We're using a dictionary for more control over ttl and eviction
        # rather than direct use of lru_cache
        self.cache = {}
        self.cache_expiry = {}
        
        # Metrics for monitoring
        self.metrics = {
            "cache_hits": 0,
            "cache_misses": 0,
            "feature_reads": 0,
            "feature_reads_by_name": {},
            "compute_requests": 0,
            "read_latency_sum": 0.0,
            "compute_latency_sum": 0.0
        }
        
    def get_feature(
        self,
        feature_name: str,
        entity_id: Optional[str] = None,
        timestamp: Optional[datetime] = None,
        version: Optional[str] = None,
        store_name: Optional[str] = None,
        use_cache: bool = True,
        compute_if_missing: bool = True
    ) -> Tuple[Any, Dict[str, Any]]:
        """Get a feature value from the appropriate store.
        
        Args:
            feature_name: Name of the feature
            entity_id: Optional ID of the entity associated with this feature
            timestamp: Optional timestamp for point-in-time feature retrieval
            version: Optional specific feature version
            store_name: Optional specific store to use
            use_cache: Whether to use cached values
            compute_if_missing: Whether to compute the feature if missing
            
        Returns:
            Tuple[Any, Dict[str, Any]]: Feature value and metadata
            
        Raises:
            KeyError: If the feature is not found in the registry
            ValueError: If the specified store is not available
        """
        # Check feature in registry
        feature_def = self.registry.get_feature(feature_name, version)
        if not feature_def:
            raise KeyError(f"Feature {feature_name} not found in registry")
            
        # Track metrics
        start_time = time.time()
        self.metrics["feature_reads"] += 1
        if feature_name not in self.metrics["feature_reads_by_name"]:
            self.metrics["feature_reads_by_name"][feature_name] = 0
        self.metrics["feature_reads_by_name"][feature_name] += 1
        
        # Check cache if enabled
        cache_key = None
        if use_cache:
            cache_key = self._get_cache_key(feature_name, entity_id, timestamp, version)
            cached_result = self._get_from_cache(cache_key)
            if cached_result is not None:
                self.metrics["cache_hits"] += 1
                return cached_result
            self.metrics["cache_misses"] += 1
            
        # Determine which store to use
        store = self._get_store(store_name, feature_def)
        
        try:
            # Try to get from store
            value, metadata = store.read_feature(
                feature_name=feature_name,
                entity_id=entity_id,
                timestamp=timestamp
            )
            
            # If feature not found and compute_if_missing is enabled, try to compute it
            if value is None and compute_if_missing and feature_def.calculation_fn:
                value, metadata = self._compute_feature(feature_def, entity_id, timestamp)
                
                # If computation succeeded, store the result
                if value is not None:
                    store.write_feature(
                        feature_name=feature_name,
                        feature_value=value,
                        entity_id=entity_id,
                        timestamp=timestamp,
                        metadata=metadata
                    )
            
            # Update cache if enabled
            if use_cache and cache_key and value is not None:
                self._add_to_cache(cache_key, (value, metadata))
                
            # Update metrics
            self.metrics["read_latency_sum"] += time.time() - start_time
            
            return value, metadata
            
        except Exception as e:
            logger.error(f"Error getting feature {feature_name}: {e}")
            # In case of error, try to compute if possible
            if compute_if_missing and feature_def.calculation_fn:
                try:
                    value, metadata = self._compute_feature(feature_def, entity_id, timestamp)
                    if value is not None and use_cache and cache_key:
                        self._add_to_cache(cache_key, (value, metadata))
                    return value, metadata
                except Exception as compute_error:
                    logger.error(f"Error computing feature {feature_name}: {compute_error}")
                    
            return None, {}
            
    def get_features(
        self,
        feature_names: List[str],
        entity_id: Optional[str] = None,
        timestamp: Optional[datetime] = None,
        use_cache: bool = True,
        compute_if_missing: bool = True
    ) -> Dict[str, Tuple[Any, Dict[str, Any]]]:
        """Get multiple feature values.
        
        Args:
            feature_names: List of feature names
            entity_id: Optional ID of the entity associated with these features
            timestamp: Optional timestamp for point-in-time feature retrieval
            use_cache: Whether to use cached values
            compute_if_missing: Whether to compute missing features
            
        Returns:
            Dict[str, Tuple[Any, Dict[str, Any]]]: Dict mapping feature names to (value, metadata) tuples
        """
        results = {}
        
        # Group features by store for more efficient batch retrieval
        features_by_store = {}
        for feature_name in feature_names:
            feature_def = self.registry.get_feature(feature_name)
            if not feature_def:
                logger.warning(f"Feature {feature_name} not found in registry")
                results[feature_name] = (None, {})
                continue
                
            store_name = self._get_store_name_for_feature(feature_def)
            if store_name not in features_by_store:
                features_by_store[store_name] = []
            features_by_store[store_name].append(feature_name)
            
        # Retrieve features from each store
        for store_name, store_features in features_by_store.items():
            store = self.stores.get(store_name)
            if not store:
                logger.error(f"Store {store_name} not found")
                for feature_name in store_features:
                    results[feature_name] = (None, {})
                continue
                
            # Check cache first
            if use_cache:
                still_needed = []
                for feature_name in store_features:
                    cache_key = self._get_cache_key(feature_name, entity_id, timestamp)
                    cached_result = self._get_from_cache(cache_key)
                    if cached_result is not None:
                        results[feature_name] = cached_result
                        self.metrics["cache_hits"] += 1
                    else:
                        still_needed.append(feature_name)
                        self.metrics["cache_misses"] += 1
            else:
                still_needed = store_features
                
            if not still_needed:
                continue
                
            # Get features from store
            try:
                store_results = store.read_features(
                    feature_names=still_needed,
                    entity_id=entity_id,
                    timestamp=timestamp
                )
                
                # Update results and cache
                for feature_name, result in store_results.items():
                    results[feature_name] = result
                    if use_cache:
                        cache_key = self._get_cache_key(feature_name, entity_id, timestamp)
                        self._add_to_cache(cache_key, result)
                        
                # If any features are still missing, try to compute them
                if compute_if_missing:
                    missing_features = [f for f in still_needed if f not in store_results or store_results[f][0] is None]
                    for feature_name in missing_features:
                        feature_def = self.registry.get_feature(feature_name)
                        if feature_def and feature_def.calculation_fn:
                            try:
                                value, metadata = self._compute_feature(feature_def, entity_id, timestamp)
                                if value is not None:
                                    # Store computed value
                                    store.write_feature(
                                        feature_name=feature_name,
                                        feature_value=value,
                                        entity_id=entity_id,
                                        timestamp=timestamp,
                                        metadata=metadata
                                    )
                                    # Update results and cache
                                    results[feature_name] = (value, metadata)
                                    if use_cache:
                                        cache_key = self._get_cache_key(feature_name, entity_id, timestamp)
                                        self._add_to_cache(cache_key, (value, metadata))
                            except Exception as e:
                                logger.error(f"Error computing feature {feature_name}: {e}")
                                results[feature_name] = (None, {})
                        else:
                            results[feature_name] = (None, {})
                            
            except Exception as e:
                logger.error(f"Error getting features from store {store_name}: {e}")
                for feature_name in still_needed:
                    results[feature_name] = (None, {})
                    
        return results
        
    def get_feature_history(
        self,
        feature_name: str,
        entity_id: Optional[str] = None,
        start_time: Optional[datetime] = None,
        end_time: Optional[datetime] = None,
        limit: Optional[int] = None,
        store_name: Optional[str] = None
    ) -> List[Tuple[datetime, Any, Dict[str, Any]]]:
        """Get historical feature values.
        
        Args:
            feature_name: Name of the feature
            entity_id: Optional ID of the entity associated with this feature
            start_time: Optional start time for the history (inclusive)
            end_time: Optional end time for the history (inclusive)
            limit: Optional maximum number of values to return
            store_name: Optional specific store to use
            
        Returns:
            List[Tuple[datetime, Any, Dict[str, Any]]]: List of (timestamp, value, metadata) tuples
            
        Raises:
            KeyError: If the feature is not found in the registry
            ValueError: If the specified store is not available
        """
        # Check feature in registry
        feature_def = self.registry.get_feature(feature_name)
        if not feature_def:
            raise KeyError(f"Feature {feature_name} not found in registry")
            
        # Determine which store to use
        store = self._get_store(store_name, feature_def)
        
        try:
            # Try to get history from store
            return store.read_feature_history(
                feature_name=feature_name,
                entity_id=entity_id,
                start_time=start_time,
                end_time=end_time,
                limit=limit
            )
        except Exception as e:
            logger.error(f"Error getting feature history for {feature_name}: {e}")
            return []
            
    def write_feature(
        self,
        feature_name: str,
        feature_value: Any,
        entity_id: Optional[str] = None,
        timestamp: Optional[datetime] = None,
        metadata: Optional[Dict[str, Any]] = None,
        store_name: Optional[str] = None
    ) -> bool:
        """Write a feature value to the appropriate store.
        
        Args:
            feature_name: Name of the feature
            feature_value: Value of the feature
            entity_id: Optional ID of the entity associated with this feature
            timestamp: Optional timestamp for the feature value
            metadata: Optional metadata associated with this feature value
            store_name: Optional specific store to use
            
        Returns:
            bool: True if write successful, False otherwise
            
        Raises:
            KeyError: If the feature is not found in the registry
            ValueError: If the specified store is not available
        """
        # Check feature in registry
        feature_def = self.registry.get_feature(feature_name)
        if not feature_def:
            raise KeyError(f"Feature {feature_name} not found in registry")
            
        # Determine which store to use
        store = self._get_store(store_name, feature_def)
        
        try:
            # Write to store
            result = store.write_feature(
                feature_name=feature_name,
                feature_value=feature_value,
                entity_id=entity_id,
                timestamp=timestamp,
                metadata=metadata
            )
            
            # Update cache if successful
            if result:
                cache_key = self._get_cache_key(feature_name, entity_id, timestamp)
                self._add_to_cache(cache_key, (feature_value, metadata or {}))
                
            return result
            
        except Exception as e:
            logger.error(f"Error writing feature {feature_name}: {e}")
            return False
            
    def get_feature_vector(
        self,
        feature_names: List[str],
        entity_id: Optional[str] = None,
        timestamp: Optional[datetime] = None,
        use_cache: bool = True,
        compute_if_missing: bool = True
    ) -> List[Any]:
        """Get a feature vector (values only) for the specified features.
        
        Args:
            feature_names: List of feature names
            entity_id: Optional ID of the entity associated with these features
            timestamp: Optional timestamp for point-in-time feature retrieval
            use_cache: Whether to use cached values
            compute_if_missing: Whether to compute missing features
            
        Returns:
            List[Any]: List of feature values
        """
        features = self.get_features(
            feature_names=feature_names,
            entity_id=entity_id,
            timestamp=timestamp,
            use_cache=use_cache,
            compute_if_missing=compute_if_missing
        )
        
        # Extract just the values, maintaining the order of feature_names
        return [features.get(name, (None, {}))[0] for name in feature_names]
        
    def get_feature_vector_with_metadata(
        self,
        feature_names: List[str],
        entity_id: Optional[str] = None,
        timestamp: Optional[datetime] = None,
        use_cache: bool = True,
        compute_if_missing: bool = True
    ) -> Tuple[List[Any], List[Dict[str, Any]]]:
        """Get a feature vector with associated metadata for the specified features.
        
        Args:
            feature_names: List of feature names
            entity_id: Optional ID of the entity associated with these features
            timestamp: Optional timestamp for point-in-time feature retrieval
            use_cache: Whether to use cached values
            compute_if_missing: Whether to compute missing features
            
        Returns:
            Tuple[List[Any], List[Dict[str, Any]]]: Tuple of (values, metadata)
        """
        features = self.get_features(
            feature_names=feature_names,
            entity_id=entity_id,
            timestamp=timestamp,
            use_cache=use_cache,
            compute_if_missing=compute_if_missing
        )
        
        # Extract values and metadata, maintaining the order of feature_names
        values = []
        metadata_list = []
        
        for name in feature_names:
            value, metadata = features.get(name, (None, {}))
            values.append(value)
            metadata_list.append(metadata)
            
        return values, metadata_list
        
    def invalidate_cache(
        self,
        feature_name: Optional[str] = None,
        entity_id: Optional[str] = None
    ) -> None:
        """Invalidate cache entries.
        
        Args:
            feature_name: Optional specific feature to invalidate
            entity_id: Optional specific entity to invalidate
        """
        if feature_name is None and entity_id is None:
            # Invalidate entire cache
            self.cache = {}
            self.cache_expiry = {}
            logger.info("Invalidated entire feature cache")
            return
            
        # Invalidate specific entries
        keys_to_remove = []
        for key in self.cache.keys():
            parts = key.split(":")
            if len(parts) >= 2:
                key_feature = parts[0]
                key_entity = parts[1] if len(parts) > 1 and parts[1] != "None" else None
                
                if (feature_name is None or key_feature == feature_name) and (entity_id is None or key_entity == entity_id):
                    keys_to_remove.append(key)
                    
        # Remove the keys
        for key in keys_to_remove:
            self.cache.pop(key, None)
            self.cache_expiry.pop(key, None)
            
        logger.info(f"Invalidated {len(keys_to_remove)} cache entries")
        
    def get_metrics(self) -> Dict[str, Any]:
        """Get metrics about feature service usage.
        
        Returns:
            Dict[str, Any]: Dictionary of metrics
        """
        # Calculate derived metrics
        if self.metrics["feature_reads"] > 0:
            cache_hit_rate = self.metrics["cache_hits"] / (self.metrics["cache_hits"] + self.metrics["cache_misses"])
            avg_read_latency = self.metrics["read_latency_sum"] / self.metrics["feature_reads"]
        else:
            cache_hit_rate = 0.0
            avg_read_latency = 0.0
            
        if self.metrics["compute_requests"] > 0:
            avg_compute_latency = self.metrics["compute_latency_sum"] / self.metrics["compute_requests"]
        else:
            avg_compute_latency = 0.0
            
        # Get store metrics
        store_metrics = {}
        for name, store in self.stores.items():
            store_metrics[name] = store.health_check()
            
        return {
            "cache_size": len(self.cache),
            "cache_hit_rate": cache_hit_rate,
            "feature_reads": self.metrics["feature_reads"],
            "feature_reads_by_name": self.metrics["feature_reads_by_name"],
            "compute_requests": self.metrics["compute_requests"],
            "avg_read_latency_ms": avg_read_latency * 1000,
            "avg_compute_latency_ms": avg_compute_latency * 1000,
            "stores": store_metrics
        }
        
    def _get_cache_key(
        self,
        feature_name: str,
        entity_id: Optional[str] = None,
        timestamp: Optional[datetime] = None,
        version: Optional[str] = None
    ) -> str:
        """Get a cache key for the specified feature.
        
        Args:
            feature_name: Name of the feature
            entity_id: Optional ID of the entity associated with this feature
            timestamp: Optional timestamp for point-in-time feature retrieval
            version: Optional specific feature version
            
        Returns:
            str: Cache key
        """
        return f"{feature_name}:{entity_id}:{timestamp}:{version}"
        
    def _get_from_cache(self, key: str) -> Optional[Tuple[Any, Dict[str, Any]]]:
        """Get a value from the cache.
        
        Args:
            key: Cache key
            
        Returns:
            Optional[Tuple[Any, Dict[str, Any]]]: Cached result or None if not found or expired
        """
        # Check if the key exists
        if key not in self.cache:
            return None
            
        # Check if the entry has expired
        expiry = self.cache_expiry.get(key, 0)
        if expiry < time.time():
            # Remove expired entry
            self.cache.pop(key, None)
            self.cache_expiry.pop(key, None)
            return None
            
        return self.cache[key]
        
    def _add_to_cache(self, key: str, value: Tuple[Any, Dict[str, Any]]) -> None:
        """Add a value to the cache.
        
        Args:
            key: Cache key
            value: Value to cache
        """
        # Check if we need to evict an entry
        if len(self.cache) >= self.cache_size:
            # Find the oldest entry
            oldest_key = min(self.cache_expiry, key=self.cache_expiry.get)
            self.cache.pop(oldest_key, None)
            self.cache_expiry.pop(oldest_key, None)
            
        # Add the new entry
        self.cache[key] = value
        self.cache_expiry[key] = time.time() + self.cache_ttl
        
    def _get_store_name_for_feature(self, feature_def: FeatureDefinition) -> str:
        """Determine which store to use for a feature.
        
        Args:
            feature_def: Feature definition
            
        Returns:
            str: Store name
        """
        # Check feature metadata for store preference
        store_name = feature_def.metadata.get("store")
        if store_name and store_name in self.stores:
            return store_name
            
        # Use default store
        return "default"
        
    def _get_store(
        self,
        store_name: Optional[str],
        feature_def: FeatureDefinition
    ) -> FeatureStore:
        """Get the appropriate store instance.
        
        Args:
            store_name: Optional specific store name
            feature_def: Feature definition
            
        Returns:
            FeatureStore: The store instance to use
            
        Raises:
            ValueError: If the specified store is not available
        """
        if store_name:
            # Explicitly specified store
            store = self.stores.get(store_name)
            if not store:
                raise ValueError(f"Store {store_name} not found")
            return store
            
        # Check feature metadata for store preference
        metadata_store = feature_def.metadata.get("store")
        if metadata_store and metadata_store in self.stores:
            return self.stores[metadata_store]
            
        # Use default store
        return self.default_store
        
    def _compute_feature(
        self,
        feature_def: FeatureDefinition,
        entity_id: Optional[str] = None,
        timestamp: Optional[datetime] = None
    ) -> Tuple[Any, Dict[str, Any]]:
        """Compute a feature value using its calculation function.
        
        Args:
            feature_def: Feature definition
            entity_id: Optional ID of the entity associated with this feature
            timestamp: Optional timestamp for point-in-time feature retrieval
            
        Returns:
            Tuple[Any, Dict[str, Any]]: Computed feature value and metadata
            
        Raises:
            ValueError: If the feature has no calculation function
        """
        if not feature_def.calculation_fn:
            raise ValueError(f"Feature {feature_def.name} has no calculation function")
            
        start_time = time.time()
        self.metrics["compute_requests"] += 1
        
        try:
            # Get dependencies if needed
            dependencies = {}
            for dep_name in feature_def.dependencies:
                dep_value, _ = self.get_feature(
                    feature_name=dep_name,
                    entity_id=entity_id,
                    timestamp=timestamp
                )
                dependencies[dep_name] = dep_value
                
            # Calculate the feature
            value = feature_def.calculate(
                entity_id=entity_id,
                timestamp=timestamp,
                dependencies=dependencies
            )
            
            # Prepare metadata
            metadata = {
                "calculated_at": datetime.now().isoformat(),
                "version": feature_def.version,
                "dependencies": feature_def.dependencies
            }
            
            if isinstance(feature_def, TimeSeriesFeatureDefinition):
                metadata["time_window"] = feature_def.time_window.total_seconds()
                metadata["time_aggregation"] = feature_def.time_aggregation
                
            # Update metrics
            compute_time = time.time() - start_time
            self.metrics["compute_latency_sum"] += compute_time
            
            return value, metadata
            
        except Exception as e:
            logger.error(f"Error computing feature {feature_def.name}: {e}")
            raise
            
    def refresh_feature(
        self,
        feature_name: str,
        entity_id: Optional[str] = None,
        timestamp: Optional[datetime] = None,
        force: bool = False
    ) -> bool:
        """Refresh a feature by recomputing it.
        
        Args:
            feature_name: Name of the feature
            entity_id: Optional ID of the entity associated with this feature
            timestamp: Optional timestamp for the feature value
            force: Whether to recompute even if not needed
            
        Returns:
            bool: True if refresh successful, False otherwise
        """
        # Get feature definition
        feature_def = self.registry.get_feature(feature_name)
        if not feature_def:
            logger.error(f"Feature {feature_name} not found in registry")
            return False
            
        # Check if calculation function exists
        if not feature_def.calculation_fn:
            logger.error(f"Feature {feature_name} has no calculation function")
            return False
            
        # Check if refresh is needed
        if not force:
            # See if the feature exists and is not stale
            value, metadata = self.get_feature(
                feature_name=feature_name,
                entity_id=entity_id,
                timestamp=timestamp,
                compute_if_missing=False
            )
            
            if value is not None:
                # Check if the feature has TTL and whether it's expired
                if feature_def.ttl:
                    calculated_at = metadata.get("calculated_at")
                    if calculated_at:
                        calculated_time = datetime.fromisoformat(calculated_at)
                        now = datetime.now()
                        if now - calculated_time < feature_def.ttl:
                            # Feature is still fresh
                            return True
                else:
                    # No TTL, feature is considered fresh
                    return True
                    
        try:
            # Compute the feature
            value, metadata = self._compute_feature(feature_def, entity_id, timestamp)
            
            # Write the feature to the appropriate store
            store_name = feature_def.metadata.get("store")
            return self.write_feature(
                feature_name=feature_name,
                feature_value=value,
                entity_id=entity_id,
                timestamp=timestamp,
                metadata=metadata,
                store_name=store_name
            )
            
        except Exception as e:
            logger.error(f"Error refreshing feature {feature_name}: {e}")
            return False 