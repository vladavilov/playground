"""
GitLab mock service configuration settings.
"""
from pydantic import Field
from configuration.base_config import BaseConfig


class GitLabMockSettings(BaseConfig):
    """Configuration for GitLab mock service."""
    
    # Service Configuration
    SERVICE_NAME: str = Field(default="gitlab-mock-service", description="Service name")
    HOST: str = Field(default="0.0.0.0", description="Service host")
    PORT: int = Field(default=8000, description="Service port")
    
    # OAuth Configuration
    GITLAB_OAUTH_CLIENT_ID: str = Field(default="mock-gitlab-client-id", description="Mock OAuth client ID")
    GITLAB_OAUTH_CLIENT_SECRET: str = Field(default="mock-gitlab-secret", description="Mock OAuth client secret")
    
    # Mock Data Configuration
    MOCK_GROUP_ID: int = Field(default=1, description="Mock GitLab group ID")
    MOCK_PROJECT_ID: int = Field(default=1, description="Mock GitLab project ID")
    MOCK_USER_ID: int = Field(default=1, description="Mock user ID")
    MOCK_USERNAME: str = Field(default="mock-user", description="Mock username")
    MOCK_USER_EMAIL: str = Field(default="user@example.com", description="Mock user email")


def get_settings() -> GitLabMockSettings:
    """Return GitLab mock settings instance."""
    return GitLabMockSettings()


settings = get_settings()

