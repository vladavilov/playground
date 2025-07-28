from functools import lru_cache
from pydantic import Field, computed_field, field_validator
from dotenv import load_dotenv
import re

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
    
    @field_validator('AZURE_TENANT_ID')
    @classmethod
    def validate_tenant_id(cls, v: str) -> str:
        """Validate that tenant ID is not empty and has valid UUID format."""
        if not v or v.strip() == "":
            raise ValueError(
                "AZURE_TENANT_ID is required. Please set this environment variable to your Azure AD tenant ID."
            )
        
        # Check if it's a valid UUID format
        uuid_pattern = re.compile(
            r'^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$',
            re.IGNORECASE
        )
        if not uuid_pattern.match(v):
            raise ValueError(
                f"AZURE_TENANT_ID '{v}' is not a valid UUID format. "
                f"Expected format: xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx"
            )
        
        return v
    AZURE_CLIENT_ID: str = Field(
        default="",
        description="Azure AD application client ID"
    )
    
    @field_validator('AZURE_CLIENT_ID')
    @classmethod
    def validate_client_id(cls, v: str) -> str:
        """Validate that client ID is not empty."""
        if not v or v.strip() == "":
            raise ValueError(
                "AZURE_CLIENT_ID is required. Please set this environment variable to your Azure AD application client ID."
            )
        return v
    AZURE_CLIENT_SECRET: str = Field(
        default="",
        description="Azure AD application client secret"
    )
    AZURE_SCOPE_DESCRIPTION: str = Field(
        default="user_impersonation",
        description="Azure AD scope description"
    )
    AZURE_AD_AUTHORITY: str = Field(
        default="https://login.mock.com",
        description="Azure AD authority URL"
    )
    
    @computed_field
    @property
    def OPENID_CONFIG_URL(self) -> str:
        """Compute the OpenID configuration URL using AZURE_AD_AUTHORITY and AZURE_TENANT_ID."""
        return f"{self.AZURE_AD_AUTHORITY}/{self.AZURE_TENANT_ID}/v2.0/.well-known/openid-configuration"
    
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
        return f"{self.AZURE_AD_AUTHORITY}/{self.AZURE_TENANT_ID}/oauth2/v2.0/authorize"
    
    @computed_field
    @property
    def OPENAPI_TOKEN_URL(self) -> str:
        """Compute the OpenAPI token URL."""
        return f"{self.AZURE_AD_AUTHORITY}/{self.AZURE_TENANT_ID}/oauth2/v2.0/token"

@lru_cache()
def get_azure_auth_settings() -> AzureAuthSettings:
    """
    Creates a cached instance of AzureAuthSettings.
    This ensures that the settings are loaded only once and reused across the application.
    """
    return AzureAuthSettings()