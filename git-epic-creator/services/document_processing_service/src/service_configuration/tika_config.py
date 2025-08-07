"""
Tika configuration settings for document processing service.
"""
from pydantic import Field, field_validator
from pydantic_settings import BaseSettings, SettingsConfigDict


class TikaSettings(BaseSettings):
    """Tika configuration settings for document processing."""

    model_config = SettingsConfigDict(
        env_file=".env",
        env_file_encoding='utf-8',
        case_sensitive=True,
        extra='ignore'
    )

    # Tika server configuration
    TIKA_SERVER_JAR: str = Field(
        default="/opt/tika-server/tika-server.jar",
        description="Path to the Tika server JAR file"
    )

    TIKA_SERVER_ENDPOINT: str = Field(
        default="http://localhost:9998",
        description="Tika server endpoint URL"
    )

    TIKA_LOG_PATH: str = Field(
        default="/tmp/tika-logs",
        description="Directory path for Tika server log files (Tika will create log files within this directory)"
    )

    # Timeout settings
    TIKA_SERVER_TIMEOUT: int = Field(
        default=300,
        description="Tika server timeout in seconds (for processing operations)"
    )

    TIKA_CLIENT_TIMEOUT: int = Field(
        default=120,
        description="Tika client timeout in seconds (for server startup)"
    )

    # Version information
    TIKA_VERSION: str = Field(
        default="3.1.0",
        description="Tika version for reference and compatibility"
    )
    
    # Client mode configuration
    TIKA_CLIENT_ONLY: bool = Field(
        default=True,
        description="If True, use Tika as a REST client only, relying on external server"
    )
    
    # Server startup configuration
    TIKA_SERVER_AUTO_START: bool = Field(
        default=True,
        description="Whether to automatically start Tika server if not running"
    )
    
    TIKA_SERVER_STARTUP_TIMEOUT: int = Field(
        default=60,
        description="Timeout in seconds to wait for Tika server startup"
    )

    @field_validator('TIKA_SERVER_ENDPOINT')
    @classmethod
    def validate_endpoint_url(cls, v: str) -> str:
        """Validate that the endpoint URL is properly formatted."""
        if not v.startswith(('http://', 'https://')):
            raise ValueError('Tika server endpoint must start with http:// or https://')
        return v

    @field_validator('TIKA_SERVER_TIMEOUT', 'TIKA_CLIENT_TIMEOUT')
    @classmethod
    def validate_positive_timeout(cls, v: int) -> int:
        """Validate that timeout values are positive."""
        if v <= 0:
            raise ValueError('Timeout values must be positive')
        return v

    @field_validator('TIKA_SERVER_JAR', 'TIKA_LOG_PATH')
    @classmethod
    def validate_path_not_empty(cls, v: str) -> str:
        """Validate that paths are not empty."""
        if not v.strip():
            raise ValueError('Path cannot be empty')
        return v.strip()

    @field_validator('TIKA_VERSION')
    @classmethod
    def validate_version_format(cls, v: str) -> str:
        """Validate that version follows semantic versioning pattern."""
        import re
        if not re.match(r'^\d+\.\d+\.\d+$', v):
            raise ValueError('Version must follow semantic versioning format (e.g., 3.1.0)')
        return v