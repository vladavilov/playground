"""Configuration settings for Neo4j Retrieval MCP Server.

Reuses shared library configurations for Redis, HTTP client, and common settings.
Adds MCP-specific configuration for transport mode and OAuth discovery.
"""

from functools import lru_cache
from pydantic import Field
from pydantic_settings import BaseSettings, SettingsConfigDict

# Import shared configurations
from configuration.common_config import get_app_settings
from configuration.http_client_config import HTTPClientSettings
from configuration.redis_config import RedisSettings
from configuration.azure_auth_config import get_azure_auth_settings


class MCPServerSettings(BaseSettings):
    """MCP-specific configuration settings.
    
    Extends shared configurations with MCP transport and OAuth settings.
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
    
    # MCP Server base URL (for OAuth discovery metadata)
    MCP_SERVER_URL: str = Field(
        default="http://localhost:8082",
        description="Base URL of this MCP server (for OAuth discovery)"
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


def get_auth_service_url() -> str:
    """Get Authentication Service URL from shared config."""
    return get_app_settings().http_client.AUTH_SERVICE_URL


def get_oauth_discovery_metadata() -> dict:
    """
    Get OAuth Protected Resource Metadata for VS Code discovery.
    
    This metadata is returned at /.well-known/oauth-protected-resource
    to help VS Code discover the authorization server.
    
    Returns:
        OAuth Protected Resource Metadata (RFC 9728)
    """
    azure_settings = get_azure_auth_settings()
    mcp_settings = get_mcp_settings()
    
    # Build authorization server URL (Azure AD or mock)
    auth_server_url = (
        f"{azure_settings.AZURE_AD_AUTHORITY}/{azure_settings.AZURE_TENANT_ID}/v2.0"
    )
    
    # Build scope
    scope = f"api://{azure_settings.AZURE_CLIENT_ID}/user_impersonation"
    
    return {
        "resource": f"{mcp_settings.MCP_SERVER_URL}/mcp",
        "authorization_servers": [auth_server_url],
        "bearer_methods_supported": ["header"],
        "scopes_supported": [scope, "openid", "profile", "email"],
        "resource_documentation": "https://github.com/your-org/neo4j-retrieval-mcp-server"
    }
