"""
HTTP client configuration settings.
"""
from pydantic import Field, field_validator
from pydantic_settings import BaseSettings, SettingsConfigDict

class HTTPClientSettings(BaseSettings):
    """HTTP client configuration settings."""

    model_config = SettingsConfigDict(
        env_file=".env",
        env_file_encoding='utf-8',
        case_sensitive=True,
        extra='ignore'
    )

    # Service URLs
    PROJECT_MANAGEMENT_SERVICE_URL: str = Field(
        default="http://localhost:8001",
        description="URL for the project management service"
    )
    AI_REQUIREMENTS_SERVICE_URL: str = Field(
        default="http://localhost:8000",
        description="URL for the AI requirements service"
    )
    AI_TASKS_SERVICE_URL: str = Field(
        default="http://localhost:8003",
        description="URL for the AI tasks/backlog generation service"
    )
    GITLAB_CLIENT_SERVICE_URL: str = Field(
        default="http://localhost:8011",
        description="URL for the GitLab client service"
    )

    # Connection settings
    CONNECTION_TIMEOUT: float = Field(
        default=30.0,
        description="Connection timeout in seconds",
        alias="HTTP_CONNECTION_TIMEOUT"
    )

    READ_TIMEOUT: float = Field(
        default=180.0,
        description="Read timeout in seconds (3 minutes for long-running workflows)",
        alias="HTTP_READ_TIMEOUT"
    )

    # Retry settings
    MAX_RETRIES: int = Field(
        default=3,
        description="Maximum number of retry attempts",
        alias="HTTP_MAX_RETRIES"
    )

    RETRY_BACKOFF_FACTOR: float = Field(
        default=2.0,
        description="Backoff factor for exponential retry",
        alias="HTTP_RETRY_BACKOFF_FACTOR"
    )

    # Connection pooling
    MAX_CONNECTIONS: int = Field(
        default=100,
        description="Maximum number of connections in pool",
        alias="HTTP_MAX_CONNECTIONS"
    )

    MAX_KEEPALIVE_CONNECTIONS: int = Field(
        default=20,
        description="Maximum number of keep-alive connections",
        alias="HTTP_MAX_KEEPALIVE_CONNECTIONS"
    )

    @field_validator('PROJECT_MANAGEMENT_SERVICE_URL')
    @classmethod
    def validate_url(cls, v: str) -> str:
        """Validate that the URL is properly formatted."""
        if not v.startswith(('http://', 'https://')):
            raise ValueError('URL must start with http:// or https://')
        return v

    @field_validator('AI_REQUIREMENTS_SERVICE_URL')
    @classmethod
    def validate_ai_url(cls, v: str) -> str:
        """Validate that the URL is properly formatted."""
        if not v.startswith(('http://', 'https://')):
            raise ValueError('URL must start with http:// or https://')
        return v

    @field_validator('AI_TASKS_SERVICE_URL')
    @classmethod
    def validate_ai_tasks_url(cls, v: str) -> str:
        """Validate that the URL is properly formatted."""
        if not v.startswith(('http://', 'https://')):
            raise ValueError('URL must start with http:// or https://')
        return v

    @field_validator('GITLAB_CLIENT_SERVICE_URL')
    @classmethod
    def validate_gitlab_client_url(cls, v: str) -> str:
        """Validate that the URL is properly formatted."""
        if not v.startswith(('http://', 'https://')):
            raise ValueError('URL must start with http:// or https://')
        return v

    @field_validator('CONNECTION_TIMEOUT', 'READ_TIMEOUT')
    @classmethod
    def validate_positive_timeout(cls, v: float) -> float:
        """Validate that timeout values are positive."""
        if v <= 0:
            raise ValueError('Timeout values must be positive')
        return v

    @field_validator('MAX_RETRIES')
    @classmethod
    def validate_non_negative_retries(cls, v: int) -> int:
        """Validate that retry count is non-negative."""
        if v < 0:
            raise ValueError('Max retries must be non-negative')
        return v

    @field_validator('RETRY_BACKOFF_FACTOR')
    @classmethod
    def validate_positive_backoff(cls, v: float) -> float:
        """Validate that backoff factor is positive."""
        if v <= 0:
            raise ValueError('Retry backoff factor must be positive')
        return v

    @field_validator('MAX_CONNECTIONS', 'MAX_KEEPALIVE_CONNECTIONS')
    @classmethod
    def validate_positive_connections(cls, v: int) -> int:
        """Validate that connection counts are positive."""
        if v <= 0:
            raise ValueError('Connection counts must be positive')
        return v
