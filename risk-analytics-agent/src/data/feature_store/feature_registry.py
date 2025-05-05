"""Feature Registry for managing feature definitions and metadata.

This module provides a registry for defining, documenting, and managing features,
including their properties, relationships, and versioning.
"""

import enum
import json
import logging
import os
from datetime import datetime, timedelta
from typing import Any, Callable, Dict, List, Optional, Set, Tuple, Type, Union

# Configure logging
logger = logging.getLogger(__name__)


class FeatureType(enum.Enum):
    """Enumeration of supported feature value types."""
    
    NUMERIC = "numeric"
    CATEGORICAL = "categorical"
    TEXT = "text"
    BOOLEAN = "boolean"
    TIMESTAMP = "timestamp"
    VECTOR = "vector"
    JSON = "json"
    # Add more types as needed


class FeatureDomain(enum.Enum):
    """Enumeration of feature domains or categories."""
    
    MARKET = "market"              # Market-level features (e.g., indices, interest rates)
    SECURITY = "security"          # Security-specific features (e.g., price, yield)
    TRADE = "trade"                # Trade-related features (e.g., volume, direction)
    LIQUIDITY = "liquidity"        # Liquidity-related features
    VOLATILITY = "volatility"      # Volatility measures
    MACRO = "macro"                # Macroeconomic indicators
    SENTIMENT = "sentiment"        # Market sentiment indicators
    TECHNICAL = "technical"        # Technical indicators
    POSITION = "position"          # Position-related features
    # Add more domains as needed


class FeatureFrequency(enum.Enum):
    """Enumeration of feature update frequencies."""
    
    TICK = "tick"                  # Updated on each tick/event
    SECOND = "second"              # Updated every second
    MINUTE = "minute"              # Updated every minute
    HOUR = "hour"                  # Updated hourly
    DAY = "day"                    # Updated daily
    WEEK = "week"                  # Updated weekly
    MONTH = "month"                # Updated monthly
    QUARTER = "quarter"            # Updated quarterly
    YEAR = "year"                  # Updated yearly
    IRREGULAR = "irregular"        # Irregularly updated
    # Add more frequencies as needed


class FeatureDefinition:
    """Definition of a feature including its metadata and calculation method."""
    
    def __init__(
        self,
        name: str,
        version: str,
        description: str,
        feature_type: FeatureType,
        domain: FeatureDomain,
        frequency: FeatureFrequency,
        calculation_fn: Optional[Callable] = None,
        dependencies: Optional[List[str]] = None,
        tags: Optional[List[str]] = None,
        is_online: bool = True,
        ttl: Optional[timedelta] = None,
        owner: Optional[str] = None,
        created_at: Optional[datetime] = None,
        updated_at: Optional[datetime] = None,
        metadata: Optional[Dict[str, Any]] = None
    ):
        """Initialize a feature definition.
        
        Args:
            name: Unique name for the feature
            version: Version of the feature (e.g., "1.0.0")
            description: Detailed description of the feature
            feature_type: Type of the feature value
            domain: Domain or category of the feature
            frequency: Update frequency of the feature
            calculation_fn: Optional function to calculate the feature
            dependencies: Optional list of feature names this feature depends on
            tags: Optional list of tags for the feature
            is_online: Whether this feature is available for online serving
            ttl: Optional time-to-live for the feature
            owner: Optional owner/team responsible for the feature
            created_at: Optional creation timestamp
            updated_at: Optional last update timestamp
            metadata: Optional additional metadata
        """
        self.name = name
        self.version = version
        self.description = description
        self.feature_type = feature_type
        self.domain = domain
        self.frequency = frequency
        self.calculation_fn = calculation_fn
        self.dependencies = dependencies or []
        self.tags = tags or []
        self.is_online = is_online
        self.ttl = ttl
        self.owner = owner
        self.created_at = created_at or datetime.now()
        self.updated_at = updated_at or datetime.now()
        self.metadata = metadata or {}
        
    def to_dict(self) -> Dict[str, Any]:
        """Convert the feature definition to a dictionary.
        
        Returns:
            Dict[str, Any]: Dictionary representation of the feature definition
        """
        result = {
            "name": self.name,
            "version": self.version,
            "description": self.description,
            "feature_type": self.feature_type.value,
            "domain": self.domain.value,
            "frequency": self.frequency.value,
            "dependencies": self.dependencies,
            "tags": self.tags,
            "is_online": self.is_online,
            "ttl": self.ttl.total_seconds() if self.ttl else None,
            "owner": self.owner,
            "created_at": self.created_at.isoformat() if self.created_at else None,
            "updated_at": self.updated_at.isoformat() if self.updated_at else None,
            "metadata": self.metadata,
            "has_calculation_fn": self.calculation_fn is not None
        }
        return result
        
    @classmethod
    def from_dict(cls, data: Dict[str, Any]) -> 'FeatureDefinition':
        """Create a feature definition from a dictionary.
        
        Args:
            data: Dictionary representation of the feature definition
            
        Returns:
            FeatureDefinition: Instantiated feature definition
        """
        # Convert string enums back to actual enum values
        feature_type = FeatureType(data["feature_type"])
        domain = FeatureDomain(data["domain"])
        frequency = FeatureFrequency(data["frequency"])
        
        # Convert timestamp strings back to datetime objects
        created_at = datetime.fromisoformat(data["created_at"]) if data.get("created_at") else None
        updated_at = datetime.fromisoformat(data["updated_at"]) if data.get("updated_at") else None
        
        # Convert TTL seconds back to timedelta
        ttl = timedelta(seconds=data["ttl"]) if data.get("ttl") is not None else None
        
        # Note: calculation_fn cannot be serialized/deserialized
        return cls(
            name=data["name"],
            version=data["version"],
            description=data["description"],
            feature_type=feature_type,
            domain=domain,
            frequency=frequency,
            calculation_fn=None,  # Can't deserialize functions
            dependencies=data.get("dependencies", []),
            tags=data.get("tags", []),
            is_online=data.get("is_online", True),
            ttl=ttl,
            owner=data.get("owner"),
            created_at=created_at,
            updated_at=updated_at,
            metadata=data.get("metadata", {})
        )
        
    def calculate(self, *args, **kwargs) -> Any:
        """Calculate the feature value using the provided calculation function.
        
        Args:
            *args: Positional arguments for the calculation function
            **kwargs: Keyword arguments for the calculation function
            
        Returns:
            Any: Calculated feature value
            
        Raises:
            ValueError: If no calculation function is defined
        """
        if self.calculation_fn is None:
            raise ValueError(f"No calculation function defined for feature {self.name}")
            
        return self.calculation_fn(*args, **kwargs)
        
    def __str__(self) -> str:
        return f"Feature({self.name}, v{self.version}, {self.domain.value})"
        
    def __repr__(self) -> str:
        return self.__str__()


class TimeSeriesFeatureDefinition(FeatureDefinition):
    """Definition of a time series feature with additional time properties."""
    
    def __init__(
        self,
        name: str,
        version: str,
        description: str,
        feature_type: FeatureType,
        domain: FeatureDomain,
        frequency: FeatureFrequency,
        time_window: timedelta,
        time_aggregation: str,
        calculation_fn: Optional[Callable] = None,
        dependencies: Optional[List[str]] = None,
        tags: Optional[List[str]] = None,
        is_online: bool = True,
        ttl: Optional[timedelta] = None,
        owner: Optional[str] = None,
        created_at: Optional[datetime] = None,
        updated_at: Optional[datetime] = None,
        metadata: Optional[Dict[str, Any]] = None
    ):
        """Initialize a time series feature definition.
        
        Args:
            name: Unique name for the feature
            version: Version of the feature (e.g., "1.0.0")
            description: Detailed description of the feature
            feature_type: Type of the feature value
            domain: Domain or category of the feature
            frequency: Update frequency of the feature
            time_window: Time window for the feature (e.g., 5 minutes, 1 day)
            time_aggregation: Aggregation method (e.g., "sum", "avg", "max")
            calculation_fn: Optional function to calculate the feature
            dependencies: Optional list of feature names this feature depends on
            tags: Optional list of tags for the feature
            is_online: Whether this feature is available for online serving
            ttl: Optional time-to-live for the feature
            owner: Optional owner/team responsible for the feature
            created_at: Optional creation timestamp
            updated_at: Optional last update timestamp
            metadata: Optional additional metadata
        """
        super().__init__(
            name=name,
            version=version,
            description=description,
            feature_type=feature_type,
            domain=domain,
            frequency=frequency,
            calculation_fn=calculation_fn,
            dependencies=dependencies,
            tags=tags,
            is_online=is_online,
            ttl=ttl,
            owner=owner,
            created_at=created_at,
            updated_at=updated_at,
            metadata=metadata
        )
        self.time_window = time_window
        self.time_aggregation = time_aggregation
        
    def to_dict(self) -> Dict[str, Any]:
        """Convert the time series feature definition to a dictionary.
        
        Returns:
            Dict[str, Any]: Dictionary representation of the feature definition
        """
        result = super().to_dict()
        result.update({
            "time_window_seconds": self.time_window.total_seconds(),
            "time_aggregation": self.time_aggregation
        })
        return result
        
    @classmethod
    def from_dict(cls, data: Dict[str, Any]) -> 'TimeSeriesFeatureDefinition':
        """Create a time series feature definition from a dictionary.
        
        Args:
            data: Dictionary representation of the feature definition
            
        Returns:
            TimeSeriesFeatureDefinition: Instantiated feature definition
        """
        # Convert string enums back to actual enum values
        feature_type = FeatureType(data["feature_type"])
        domain = FeatureDomain(data["domain"])
        frequency = FeatureFrequency(data["frequency"])
        
        # Convert timestamp strings back to datetime objects
        created_at = datetime.fromisoformat(data["created_at"]) if data.get("created_at") else None
        updated_at = datetime.fromisoformat(data["updated_at"]) if data.get("updated_at") else None
        
        # Convert TTL seconds back to timedelta
        ttl = timedelta(seconds=data["ttl"]) if data.get("ttl") is not None else None
        
        # Convert time window seconds back to timedelta
        time_window = timedelta(seconds=data["time_window_seconds"])
        
        return cls(
            name=data["name"],
            version=data["version"],
            description=data["description"],
            feature_type=feature_type,
            domain=domain,
            frequency=frequency,
            time_window=time_window,
            time_aggregation=data["time_aggregation"],
            calculation_fn=None,  # Can't deserialize functions
            dependencies=data.get("dependencies", []),
            tags=data.get("tags", []),
            is_online=data.get("is_online", True),
            ttl=ttl,
            owner=data.get("owner"),
            created_at=created_at,
            updated_at=updated_at,
            metadata=data.get("metadata", {})
        )
        
    def __str__(self) -> str:
        return f"TimeSeriesFeature({self.name}, v{self.version}, {self.domain.value}, window={self.time_window})"


class FeatureGroup:
    """A group of related features."""
    
    def __init__(
        self,
        name: str,
        description: str,
        features: Optional[List[FeatureDefinition]] = None,
        tags: Optional[List[str]] = None,
        owner: Optional[str] = None,
        metadata: Optional[Dict[str, Any]] = None
    ):
        """Initialize a feature group.
        
        Args:
            name: Name of the feature group
            description: Description of the feature group
            features: Optional list of feature definitions in this group
            tags: Optional list of tags for the group
            owner: Optional owner/team responsible for the group
            metadata: Optional additional metadata
        """
        self.name = name
        self.description = description
        self.features = features or []
        self.tags = tags or []
        self.owner = owner
        self.metadata = metadata or {}
        
    def add_feature(self, feature: FeatureDefinition) -> None:
        """Add a feature to the group.
        
        Args:
            feature: Feature definition to add
        """
        self.features.append(feature)
        
    def remove_feature(self, feature_name: str) -> bool:
        """Remove a feature from the group.
        
        Args:
            feature_name: Name of the feature to remove
            
        Returns:
            bool: True if the feature was found and removed, False otherwise
        """
        original_length = len(self.features)
        self.features = [f for f in self.features if f.name != feature_name]
        return len(self.features) < original_length
        
    def get_feature(self, feature_name: str) -> Optional[FeatureDefinition]:
        """Get a feature from the group by name.
        
        Args:
            feature_name: Name of the feature to get
            
        Returns:
            Optional[FeatureDefinition]: The feature definition if found, None otherwise
        """
        for feature in self.features:
            if feature.name == feature_name:
                return feature
        return None
        
    def to_dict(self) -> Dict[str, Any]:
        """Convert the feature group to a dictionary.
        
        Returns:
            Dict[str, Any]: Dictionary representation of the feature group
        """
        return {
            "name": self.name,
            "description": self.description,
            "features": [feature.to_dict() for feature in self.features],
            "tags": self.tags,
            "owner": self.owner,
            "metadata": self.metadata
        }
        
    @classmethod
    def from_dict(cls, data: Dict[str, Any]) -> 'FeatureGroup':
        """Create a feature group from a dictionary.
        
        Args:
            data: Dictionary representation of the feature group
            
        Returns:
            FeatureGroup: Instantiated feature group
        """
        # Determine the appropriate feature class for each feature
        features = []
        for feature_data in data.get("features", []):
            if "time_window_seconds" in feature_data:
                feature = TimeSeriesFeatureDefinition.from_dict(feature_data)
            else:
                feature = FeatureDefinition.from_dict(feature_data)
            features.append(feature)
            
        return cls(
            name=data["name"],
            description=data["description"],
            features=features,
            tags=data.get("tags", []),
            owner=data.get("owner"),
            metadata=data.get("metadata", {})
        )


class FeatureRegistry:
    """Registry for managing and accessing feature definitions."""
    
    def __init__(self, storage_path: Optional[str] = None):
        """Initialize the feature registry.
        
        Args:
            storage_path: Optional path to store the registry on disk
        """
        self.features: Dict[str, Dict[str, FeatureDefinition]] = {}  # name -> version -> definition
        self.groups: Dict[str, FeatureGroup] = {}  # name -> group
        self.storage_path = storage_path
        
    def register_feature(self, feature: FeatureDefinition) -> None:
        """Register a feature definition in the registry.
        
        Args:
            feature: Feature definition to register
        """
        if feature.name not in self.features:
            self.features[feature.name] = {}
            
        self.features[feature.name][feature.version] = feature
        logger.info(f"Registered feature {feature.name} version {feature.version}")
        
    def register_feature_group(self, group: FeatureGroup) -> None:
        """Register a feature group in the registry.
        
        Args:
            group: Feature group to register
        """
        self.groups[group.name] = group
        
        # Also register individual features
        for feature in group.features:
            self.register_feature(feature)
            
        logger.info(f"Registered feature group {group.name} with {len(group.features)} features")
        
    def get_feature(self, name: str, version: Optional[str] = None) -> Optional[FeatureDefinition]:
        """Get a feature definition from the registry.
        
        Args:
            name: Name of the feature
            version: Optional specific version to retrieve (defaults to latest)
            
        Returns:
            Optional[FeatureDefinition]: The feature definition if found, None otherwise
        """
        if name not in self.features:
            return None
            
        if version:
            return self.features[name].get(version)
        else:
            # Return latest version based on semantic versioning
            versions = sorted(self.features[name].keys(), key=lambda v: [int(x) for x in v.split('.')])
            if not versions:
                return None
            return self.features[name][versions[-1]]
            
    def get_feature_group(self, name: str) -> Optional[FeatureGroup]:
        """Get a feature group from the registry.
        
        Args:
            name: Name of the feature group
            
        Returns:
            Optional[FeatureGroup]: The feature group if found, None otherwise
        """
        return self.groups.get(name)
        
    def list_features(
        self,
        domain: Optional[FeatureDomain] = None,
        feature_type: Optional[FeatureType] = None,
        tags: Optional[List[str]] = None,
        is_online: Optional[bool] = None
    ) -> List[FeatureDefinition]:
        """List features in the registry, optionally filtered.
        
        Args:
            domain: Optional domain filter
            feature_type: Optional feature type filter
            tags: Optional tags filter (features must have all tags)
            is_online: Optional filter for online availability
            
        Returns:
            List[FeatureDefinition]: List of matching feature definitions
        """
        results = []
        
        for name, versions in self.features.items():
            # Get the latest version of each feature
            latest_version = sorted(versions.keys(), key=lambda v: [int(x) for x in v.split('.')])[-1]
            feature = versions[latest_version]
            
            # Apply filters
            if domain and feature.domain != domain:
                continue
                
            if feature_type and feature.feature_type != feature_type:
                continue
                
            if is_online is not None and feature.is_online != is_online:
                continue
                
            if tags and not all(tag in feature.tags for tag in tags):
                continue
                
            results.append(feature)
            
        return results
        
    def list_feature_groups(self, tags: Optional[List[str]] = None) -> List[FeatureGroup]:
        """List feature groups in the registry, optionally filtered by tags.
        
        Args:
            tags: Optional tags filter (groups must have all tags)
            
        Returns:
            List[FeatureGroup]: List of matching feature groups
        """
        if not tags:
            return list(self.groups.values())
            
        return [group for group in self.groups.values() if all(tag in group.tags for tag in tags)]
        
    def get_feature_dependencies(self, name: str, version: Optional[str] = None) -> List[FeatureDefinition]:
        """Get all dependencies of a feature.
        
        Args:
            name: Name of the feature
            version: Optional specific version
            
        Returns:
            List[FeatureDefinition]: List of dependent feature definitions
        """
        feature = self.get_feature(name, version)
        if not feature:
            return []
            
        dependencies = []
        for dep_name in feature.dependencies:
            dep_feature = self.get_feature(dep_name)
            if dep_feature:
                dependencies.append(dep_feature)
                
        return dependencies
        
    def get_feature_dependents(self, name: str, version: Optional[str] = None) -> List[FeatureDefinition]:
        """Get all features that depend on the specified feature.
        
        Args:
            name: Name of the feature
            version: Optional specific version
            
        Returns:
            List[FeatureDefinition]: List of features that depend on this feature
        """
        feature = self.get_feature(name, version)
        if not feature:
            return []
            
        dependents = []
        for feat_name, versions in self.features.items():
            for feat_version, feat in versions.items():
                if name in feat.dependencies:
                    dependents.append(feat)
                    
        return dependents
        
    def save(self, path: Optional[str] = None) -> bool:
        """Save the registry to disk.
        
        Args:
            path: Optional path to save to (defaults to storage_path)
            
        Returns:
            bool: True if successful, False otherwise
        """
        save_path = path or self.storage_path
        if not save_path:
            logger.error("No storage path specified for feature registry")
            return False
            
        try:
            # Create directory if it doesn't exist
            os.makedirs(os.path.dirname(save_path), exist_ok=True)
            
            # Convert registry to serializable format
            registry_data = {
                "features": {
                    name: {
                        version: feature.to_dict()
                        for version, feature in versions.items()
                    }
                    for name, versions in self.features.items()
                },
                "groups": {
                    name: group.to_dict()
                    for name, group in self.groups.items()
                }
            }
            
            # Write to file
            with open(save_path, 'w') as f:
                json.dump(registry_data, f, indent=2)
                
            logger.info(f"Saved feature registry to {save_path}")
            return True
            
        except Exception as e:
            logger.error(f"Error saving feature registry: {e}")
            return False
            
    @classmethod
    def load(cls, path: str) -> 'FeatureRegistry':
        """Load a registry from disk.
        
        Args:
            path: Path to load from
            
        Returns:
            FeatureRegistry: Loaded registry
            
        Raises:
            FileNotFoundError: If the registry file doesn't exist
            json.JSONDecodeError: If the registry file is invalid JSON
        """
        registry = cls(storage_path=path)
        
        try:
            with open(path, 'r') as f:
                registry_data = json.load(f)
                
            # Load features
            for name, versions in registry_data.get("features", {}).items():
                registry.features[name] = {}
                for version, feature_data in versions.items():
                    # Determine the appropriate feature class
                    if "time_window_seconds" in feature_data:
                        feature = TimeSeriesFeatureDefinition.from_dict(feature_data)
                    else:
                        feature = FeatureDefinition.from_dict(feature_data)
                        
                    registry.features[name][version] = feature
                    
            # Load groups
            for name, group_data in registry_data.get("groups", {}).items():
                group = FeatureGroup.from_dict(group_data)
                registry.groups[name] = group
                
            logger.info(f"Loaded feature registry from {path}")
            return registry
            
        except FileNotFoundError:
            logger.warning(f"Feature registry file not found: {path}")
            return registry
            
        except json.JSONDecodeError as e:
            logger.error(f"Invalid feature registry file: {e}")
            return registry 