import uuid
from pydantic_settings import BaseSettings, SettingsConfigDict

class Settings(BaseSettings):
    """
    Loads configuration from environment variables.
    Provides sensible defaults if variables are not set.
    """
    # The Tenant ID to be used by the mock server and the client app.
    AZURE_AD_TENANT_ID: str = "e7963c3a-3b3a-43b6-9426-89e433d07e69"

    # The Client ID (audience) to be used by the mock server and the client app.
    AZURE_AD_CLIENT_ID: str = "a9e304a9-5b6c-4ef7-9b37-23a579a6d7be"

    # The base URL of the mock server, used to construct endpoint URLs.
    # This should match the service name and port in docker-compose.
    AZURE_AD_AUTHORITY: str = "http://mock-auth-service:8005"

    model_config = SettingsConfigDict(
        env_file=".env",
        env_file_encoding="utf-8",
        extra="ignore" # Ignore extra fields from the .env file
    )

# Create a single, importable instance of the settings
settings = Settings()
