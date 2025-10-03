"""
Configuration module for the mock auth service.
Provides centralized settings management with environment variable support.
"""
import os
from pydantic import Field, HttpUrl, AliasChoices
from pydantic_settings import BaseSettings, SettingsConfigDict


class Settings(BaseSettings):
    """
    Loads configuration from environment variables.
    Provides sensible defaults if variables are not set.
    """
    AZURE_AD_TENANT_ID: str = Field(
        default="e7963c3a-3b3a-43b6-9426-89e433d07e69",
        min_length=1,
        description="Azure AD tenant ID",
        validation_alias=AliasChoices("AZURE_AD_TENANT_ID", "AZURE_TENANT_ID"),
    )

    AZURE_AD_CLIENT_ID: str = Field(
        default="a9e304a9-5b6c-4ef7-9b37-23a579a6d7be",
        min_length=1,
        description="Azure AD client ID",
        validation_alias=AliasChoices("AZURE_AD_CLIENT_ID", "AZURE_CLIENT_ID"),
    )

    # The base URL of the mock server, used to construct endpoint URLs.
    # This should match the service name and port in docker-compose.
    AZURE_AD_AUTHORITY: HttpUrl = Field(
        default="https://mock-auth-service:8005",
        description="Azure AD authority URL",
    )

    # Optional: client secret validation (reads either AZURE_CLIENT_SECRET or MOCK_CLIENT_SECRET)
    AZURE_CLIENT_SECRET: str = Field(
        default_factory=lambda: os.getenv("AZURE_CLIENT_SECRET") or os.getenv("MOCK_CLIENT_SECRET") or "",
        description="Client secret used to validate token requests",
    )

    # Optional RSA private key in PEM format for JWT signing
    # If not provided, keys will be loaded from file or generated
    MOCK_AUTH_PRIVATE_KEY: str = ""

    # Optional key ID for JWT signing
    # If not provided, a key ID will be loaded from file or generated
    MOCK_AUTH_KEY_ID: str = ""

    # SSL/TLS configuration for HTTPS
    SSL_CERTFILE: str = Field(
        default="/app/certs/cert.pem",
        description="Path to SSL certificate file",
    )

    SSL_KEYFILE: str = Field(
        default="/app/certs/key.pem",
        description="Path to SSL private key file",
    )

    ENABLE_HTTPS: bool = Field(
        default=True,
        description="Enable HTTPS for the mock auth service",
    )

    model_config = SettingsConfigDict(
        env_file=".env",
        env_file_encoding="utf-8",
        extra="ignore",  # Ignore extra fields from the .env file
    )


# Create a single, importable instance of the settings
settings = Settings()
