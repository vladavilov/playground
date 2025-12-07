"""Configuration settings for Neo4j Retrieval MCP Server.

Reuses shared library configurations for Redis, HTTP client, and common settings.
Adds MCP-specific configuration for transport mode and OAuth discovery.
"""

from dataclasses import dataclass
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

    PORT: int = Field(
        default=8082,
        description="HTTP server port (when using HTTP transport)"
    )
    MCP_TRANSPORT: str = Field(
        default="stdio",
        description="MCP transport mode: 'stdio' or 'http'"
    )
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


# OAuth metadata building


@dataclass(frozen=True)
class OAuthEndpoints:
    """Azure AD OAuth endpoint URLs and common metadata."""
    
    issuer: str
    authorization_endpoint: str
    token_endpoint: str
    jwks_uri: str
    userinfo_endpoint: str
    client_id: str
    api_scope: str
    
    @property
    def base_scopes(self) -> list[str]:
        """Standard OIDC scopes."""
        return [self.api_scope, "openid", "profile", "email"]
    
    @property
    def extended_scopes(self) -> list[str]:
        """Extended scopes including offline_access."""
        return [*self.base_scopes, "offline_access"]


@lru_cache()
def _get_oauth_endpoints() -> OAuthEndpoints:
    """Build OAuth endpoints from Azure AD and MCP settings (cached)."""
    azure = get_azure_auth_settings()
    mcp = get_mcp_settings()
    
    base = f"{azure.AZURE_AD_AUTHORITY}/{azure.AZURE_TENANT_ID}"
    
    return OAuthEndpoints(
        issuer=f"{base}/v2.0",
        authorization_endpoint=f"{base}/oauth2/v2.0/authorize",
        token_endpoint=f"{base}/oauth2/v2.0/token",
        jwks_uri=f"{base}/discovery/v2.0/keys",
        userinfo_endpoint=f"{mcp.MCP_SERVER_URL}/userinfo",
        client_id=azure.AZURE_CLIENT_ID,
        api_scope=f"api://{azure.AZURE_CLIENT_ID}/user_impersonation",
    )


def get_oauth_discovery_metadata() -> dict:
    """
    Get OAuth Protected Resource Metadata (RFC 9728).
    
    Returned at /.well-known/oauth-protected-resource for VS Code discovery.
    """
    endpoints = _get_oauth_endpoints()
    mcp = get_mcp_settings()
    
    return {
        "resource": f"{mcp.MCP_SERVER_URL}/mcp",
        "authorization_servers": [endpoints.issuer],
        "bearer_methods_supported": ["header"],
        "scopes_supported": endpoints.base_scopes,
        "resource_documentation": "https://github.com/your-org/neo4j-retrieval-mcp-server",
        "grant_types_supported": ["authorization_code", "refresh_token"],
        "authorization_endpoint": endpoints.authorization_endpoint,
        "token_endpoint": endpoints.token_endpoint,
        "userinfo_endpoint": endpoints.userinfo_endpoint,
        "client_id": endpoints.client_id,
    }


def get_authorization_server_metadata() -> dict:
    """
    Get OAuth Authorization Server Metadata (RFC 8414).
    
    Returned at /.well-known/oauth-authorization-server for full OAuth discovery.
    """
    endpoints = _get_oauth_endpoints()
    
    return {
        "issuer": endpoints.issuer,
        "authorization_endpoint": endpoints.authorization_endpoint,
        "token_endpoint": endpoints.token_endpoint,
        "jwks_uri": endpoints.jwks_uri,
        "response_types_supported": ["code", "id_token", "code id_token"],
        "response_modes_supported": ["query", "fragment", "form_post"],
        "grant_types_supported": ["authorization_code", "refresh_token"],
        "scopes_supported": endpoints.extended_scopes,
        "token_endpoint_auth_methods_supported": ["client_secret_basic", "client_secret_post"],
        "code_challenge_methods_supported": ["S256"],
        "userinfo_endpoint": endpoints.userinfo_endpoint,
        "client_id": endpoints.client_id,
    }
