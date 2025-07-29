"""
Configuration module for the mock auth service.
Provides centralized settings management with environment variable support.
"""
from pydantic import Field, HttpUrl
from pydantic_settings import BaseSettings, SettingsConfigDict

class Settings(BaseSettings):
    """
    Loads configuration from environment variables.
    Provides sensible defaults if variables are not set.
    """
    # The Tenant ID to be used by the mock server and the client app.
    AZURE_AD_TENANT_ID: str = Field(
        default="e7963c3a-3b3a-43b6-9426-89e433d07e69",
        min_length=1,
        description="Azure AD tenant ID"
    )

    # The Client ID (audience) to be used by the mock server and the client app.
    AZURE_AD_CLIENT_ID: str = Field(
        default="a9e304a9-5b6c-4ef7-9b37-23a579a6d7be",
        min_length=1,
        description="Azure AD client ID"
    )

    # The base URL of the mock server, used to construct endpoint URLs.
    # This should match the service name and port in docker-compose.
    AZURE_AD_AUTHORITY: HttpUrl = Field(
        default="http://mock-auth-service:8005",
        description="Azure AD authority URL"
    )

    # Optional RSA private key in PEM format for JWT signing
    # If not provided, keys will be loaded from file or generated
    MOCK_AUTH_PRIVATE_KEY: str = ""

    # Optional key ID for JWT signing
    # If not provided, a key ID will be loaded from file or generated
    MOCK_AUTH_KEY_ID: str = ""

    model_config = SettingsConfigDict(
        env_file=".env",
        env_file_encoding="utf-8",
        extra="ignore" # Ignore extra fields from the .env file
    )

# Create a single, importable instance of the settings
settings = Settings()
