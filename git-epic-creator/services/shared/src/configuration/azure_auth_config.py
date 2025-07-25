from functools import lru_cache
from pydantic_settings import BaseSettings, SettingsConfigDict
from pydantic import Field, computed_field
from dotenv import load_dotenv

from .base_config import BaseConfig

load_dotenv()

class AzureAuthSettings(BaseConfig):
    """
    Defines the Azure AD authentication settings using Pydantic's BaseSettings.
    It automatically reads from environment variables and .env files.
    """
    
    # Azure AD Configuration
    AZURE_TENANT_ID: str = Field(
        default="",
        description="Azure AD tenant ID"
    )
    AZURE_CLIENT_ID: str = Field(
        default="",
        description="Azure AD application client ID"
    )
    AZURE_CLIENT_SECRET: str = Field(
        default="",
        description="Azure AD application client secret"
    )
    AZURE_SCOPE_DESCRIPTION: str = Field(
        default="user_impersonation",
        description="Azure AD scope description"
    )
    
    # OpenAPI Configuration
    OPENAPI_CLIENT_ID: str = Field(
        default="",
        description="OpenAPI client ID for Swagger UI"
    )
    
    # CORS Configuration
    BACKEND_CORS_ORIGINS: list[str] = Field(
        default=["http://localhost:8000"],
        description="List of allowed CORS origins"
    )
    
    @computed_field
    @property
    def SCOPE_NAME(self) -> str:
        """Compute the full scope name."""
        return f'api://{self.AZURE_CLIENT_ID}/{self.AZURE_SCOPE_DESCRIPTION}'
    
    @computed_field
    @property
    def SCOPES(self) -> dict[str, str]:
        """Compute the scopes dictionary."""
        return {
            self.SCOPE_NAME: self.AZURE_SCOPE_DESCRIPTION,
        }
    
    @computed_field
    @property
    def OPENAPI_AUTHORIZATION_URL(self) -> str:
        """Compute the OpenAPI authorization URL."""
        return f"https://login.microsoftonline.com/{self.AZURE_TENANT_ID}/oauth2/v2.0/authorize"
    
    @computed_field
    @property
    def OPENAPI_TOKEN_URL(self) -> str:
        """Compute the OpenAPI token URL."""
        return f"https://login.microsoftonline.com/{self.AZURE_TENANT_ID}/oauth2/v2.0/token"

@lru_cache()
def get_azure_auth_settings() -> AzureAuthSettings:
    """
    Creates a cached instance of AzureAuthSettings.
    This ensures that the settings are loaded only once and reused across the application.
    """
    return AzureAuthSettings()