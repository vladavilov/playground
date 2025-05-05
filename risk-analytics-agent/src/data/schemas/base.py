"""Base schemas and utilities for data models.

This module provides base classes and utilities for defining consistent data models
across the risk analytics system.
"""

from datetime import datetime
from typing import Any, Dict, List, Optional, Union

from pydantic import BaseModel, Field, validator


class BaseSchema(BaseModel):
    """Base class for all schema models in the system."""
    
    class Config:
        """Pydantic configuration for all schema models."""
        
        # Allow extra fields when parsing, but don't include them in serialized output
        extra = "ignore"
        
        # Validate field assignments
        validate_assignment = True
        
        # Populate models with the fields defined in the parent classes
        allow_population_by_field_name = True
        
        # Always use camelCase for serialization (JSON keys)
        alias_generator = lambda string: ''.join(
            word.capitalize() if i else word 
            for i, word in enumerate(string.split('_'))
        )
        
        # Make all validators strict by default
        validate_all = True
        
        # Generate JSON Schema compatible with OpenAPI
        schema_extra = {
            "x-risk-analytics-version": "1.0.0"
        }


class TimestampMixin(BaseModel):
    """Mixin for models that include timestamps."""
    
    timestamp: datetime = Field(
        ...,
        description="Timestamp for when this data point was recorded",
        example="2023-07-21T14:30:15.123Z"
    )
    
    @validator("timestamp")
    def validate_timestamp(cls, v: datetime) -> datetime:
        """Validate that the timestamp is not in the future."""
        now = datetime.now()
        if v > now:
            raise ValueError(f"Timestamp cannot be in the future: {v} > {now}")
        return v


class SourceMetadataMixin(BaseModel):
    """Mixin for models that include source metadata."""
    
    metadata: Dict[str, Any] = Field(
        ...,
        description="Metadata about the data source and quality",
        example={"source": "BLOOMBERG", "confidence": 0.95}
    )
    
    @validator("metadata")
    def validate_metadata(cls, v: Dict[str, Any]) -> Dict[str, Any]:
        """Validate that the metadata contains required fields."""
        if "source" not in v:
            raise ValueError("Metadata must contain 'source' field")
        return v 