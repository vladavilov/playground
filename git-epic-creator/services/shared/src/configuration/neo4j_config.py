"""
Neo4j database configuration settings.
"""

from pydantic import Field, field_validator
from .base_config import BaseConfig
from functools import lru_cache

class Neo4jSettings(BaseConfig):
    """
    Defines the Neo4j database configuration settings.
    """
    NEO4J_URI: str = Field(default="bolt://localhost:7687", description="Neo4j connection URI")
    NEO4J_USERNAME: str = Field(default="neo4j", description="Neo4j username")
    NEO4J_PASSWORD: str = Field(default="neo4j123", description="Neo4j password")
    NEO4J_DATABASE: str = Field(default="neo4j", description="Neo4j database name")
    NEO4J_CONNECTION_TIMEOUT: float = Field(default=30.0, description="Neo4j connection timeout in seconds")
    NEO4J_MAX_RETRY_ATTEMPTS: int = Field(default=3, description="Neo4j maximum retry attempts")
    NEO4J_RETRY_DELAY: float = Field(default=2.0, description="Neo4j retry delay in seconds")
    NEO4J_MAX_CONNECTION_POOL_SIZE: int = Field(default=50, description="Neo4j maximum connection pool size")
    NEO4J_MAX_TRANSACTION_RETRY_TIME: float = Field(default=30.0, description="Neo4j maximum transaction retry time in seconds")

    @field_validator('NEO4J_CONNECTION_TIMEOUT', 'NEO4J_RETRY_DELAY', 'NEO4J_MAX_TRANSACTION_RETRY_TIME')
    @classmethod
    def validate_timeout(cls, v):
        """Validate that timeout values are positive."""
        if v <= 0:
            raise ValueError(f"Timeout values must be positive, got {v}")
        return v
    
    @field_validator('NEO4J_MAX_RETRY_ATTEMPTS', 'NEO4J_MAX_CONNECTION_POOL_SIZE')
    @classmethod
    def validate_positive_int(cls, v):
        """Validate that integer values are positive."""
        if v <= 0:
            raise ValueError(f"Integer values must be positive, got {v}")
        return v
    
@lru_cache()
def get_neo4j_settings() -> Neo4jSettings:
    """
    Creates a cached instance of Neo4jSettings.
    This ensures that the settings are loaded only once and reused across the application.
    """
    return Neo4jSettings() 