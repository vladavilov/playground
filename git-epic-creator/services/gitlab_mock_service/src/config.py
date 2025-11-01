"""
GitLab mock service configuration settings.

This configuration reads OAuth credentials from environment variables to support
both mock and real GitLab OAuth flows. The environment variables should match
those used by gitlab-client-service for proper integration.

Environment Variables:
    GITLAB_OAUTH_CLIENT_ID: OAuth application client ID (from docker-compose.env)
    GITLAB_OAUTH_CLIENT_SECRET: OAuth application client secret (from docker-compose.env)
"""
from pydantic import Field
from configuration.base_config import BaseConfig


class GitLabMockSettings(BaseConfig):
    """
    Configuration for GitLab mock service.
    
    Inherits from BaseConfig which automatically reads environment variables
    with case-sensitive matching (GITLAB_OAUTH_CLIENT_ID, etc.).
    """
    
    # Service Configuration
    SERVICE_NAME: str = Field(default="gitlab-mock-service", description="Service name")
    HOST: str = Field(default="0.0.0.0", description="Service host")
    PORT: int = Field(default=8000, description="Service port")
    
    # OAuth Configuration
    # These values MUST match the credentials configured in docker-compose.env
    # for gitlab-client-service to successfully authenticate
    GITLAB_OAUTH_CLIENT_ID: str = Field(
        default="mock-gitlab-client-id",
        description="OAuth client ID - reads from environment variable GITLAB_OAUTH_CLIENT_ID"
    )
    GITLAB_OAUTH_CLIENT_SECRET: str = Field(
        default="mock-gitlab-secret",
        description="OAuth client secret - reads from environment variable GITLAB_OAUTH_CLIENT_SECRET"
    )
    
    # Mock Data Configuration
    MOCK_GROUP_ID: int = Field(default=1, description="Mock GitLab group ID")
    MOCK_PROJECT_ID: int = Field(default=1, description="Mock GitLab project ID")
    MOCK_USER_ID: int = Field(default=1, description="Mock user ID")
    MOCK_USERNAME: str = Field(default="mock-user", description="Mock username")
    MOCK_USER_EMAIL: str = Field(default="user@example.com", description="Mock user email")


def get_settings() -> GitLabMockSettings:
    """
    Return GitLab mock settings instance.
    
    Settings are loaded from environment variables (via BaseConfig)
    with fallback to defaults for local development.
    """
    return GitLabMockSettings()


settings = get_settings()

