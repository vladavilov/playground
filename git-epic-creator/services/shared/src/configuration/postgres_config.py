"""
PostgreSQL database configuration settings.
"""

from functools import lru_cache
from pydantic import Field, field_validator
from .base_config import BaseConfig

class PostgresSettings(BaseConfig):
    """
    Defines the PostgreSQL database configuration settings.
    """
    POSTGRES_HOST: str = Field(default="localhost", description="PostgreSQL host")
    POSTGRES_PORT: int = Field(default=5432, description="PostgreSQL port")
    POSTGRES_USER: str = Field(default="postgres", description="PostgreSQL user")
    POSTGRES_PASSWORD: str = Field(default="postgres123", description="PostgreSQL password")
    POSTGRES_DB: str = Field(default="requirementsdb", description="PostgreSQL database name")
    POSTGRES_SCHEMA: str = Field(default="public", description="PostgreSQL schema")
    POSTGRES_POOL_SIZE: int = Field(default=5, description="PostgreSQL connection pool size")
    POSTGRES_MAX_OVERFLOW: int = Field(default=10, description="PostgreSQL connection pool max overflow")
    POSTGRES_POOL_TIMEOUT: int = Field(default=30, description="PostgreSQL connection pool timeout in seconds")
    POSTGRES_POOL_RECYCLE: int = Field(default=1800, description="PostgreSQL connection pool recycle time in seconds")

    @field_validator('POSTGRES_PORT')
    @classmethod
    def validate_port(cls, v):
        """Validate that port is in valid range."""
        if not 1 <= v <= 65535:
            raise ValueError(f"Port must be between 1 and 65535, got {v}")
        return v

    @property
    def DATABASE_URL(self) -> str:
        """Construct the database URL from individual components."""
        return f"postgresql://{self.POSTGRES_USER}:{self.POSTGRES_PASSWORD}@{self.POSTGRES_HOST}:{self.POSTGRES_PORT}/{self.POSTGRES_DB}"

    @property
    def ASYNC_DATABASE_URL(self) -> str:
        """Construct the async database URL from individual components."""
        return f"postgresql+asyncpg://{self.POSTGRES_USER}:{self.POSTGRES_PASSWORD}@{self.POSTGRES_HOST}:{self.POSTGRES_PORT}/{self.POSTGRES_DB}" 

@lru_cache()
def get_postgres_settings() -> PostgresSettings:
    """
    Returns a cached instance of the PostgresSettings.
    """
    return PostgresSettings() 