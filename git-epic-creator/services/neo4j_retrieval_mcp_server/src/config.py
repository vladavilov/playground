"""Configuration settings for Neo4j Retrieval MCP Server.

Reuses shared library configurations for Redis, HTTP client, and common settings.
Adds MCP-specific configuration for transport mode.
"""

from functools import lru_cache
from pydantic import Field
from pydantic_settings import BaseSettings, SettingsConfigDict

# Import shared configurations
from configuration.common_config import get_app_settings
from configuration.http_client_config import HTTPClientSettings
from configuration.redis_config import RedisSettings


class MCPServerSettings(BaseSettings):
    """MCP-specific configuration settings.
    
    Extends shared configurations with MCP transport settings.
    """

    model_config = SettingsConfigDict(
        env_file=".env",
        env_file_encoding="utf-8",
        case_sensitive=True,
        extra="ignore"
    )

    # MCP Transport configuration
    PORT: int = Field(
        default=8082,
        description="HTTP server port (when using HTTP transport)"
    )
    MCP_TRANSPORT: str = Field(
        default="stdio",
        description="MCP transport mode: 'stdio' or 'http'"
    )


@lru_cache()
def get_mcp_settings() -> MCPServerSettings:
    """Get cached MCP-specific settings."""
    return MCPServerSettings()


def get_http_client_settings() -> HTTPClientSettings:
    """Get HTTP client settings from shared configuration."""
    return get_app_settings().http_client


def get_redis_settings() -> RedisSettings:
    """Get Redis settings from shared configuration."""
    return get_app_settings().redis


def get_project_management_url() -> str:
    """Get Project Management Service URL from shared config."""
    return get_app_settings().http_client.PROJECT_MANAGEMENT_SERVICE_URL


def get_retrieval_service_url() -> str:
    """Get Neo4j Retrieval Service URL from shared config."""
    return get_app_settings().http_client.GRAPH_RAG_SERVICE_URL
