"""
GitLab configuration settings.

Provides GitLab API access configuration for services that need to interact
with GitLab (project management, repository access, CI/CD integration, etc.).
"""

from functools import lru_cache
from pydantic import Field
from dotenv import load_dotenv

from .base_config import BaseConfig

load_dotenv()


class GitLabSettings(BaseConfig):
    """
    GitLab API configuration settings.
    
    Used by services that need to access GitLab API for:
    - Repository operations
    - Project management
    - Issue/MR tracking
    - CI/CD pipeline management
    """
    
    GITLAB_BASE_URL: str = Field(
        default="https://gitlab.com",
        description="Base URL of GitLab instance (e.g., https://gitlab.example.com)"
    )
    
    GITLAB_VERIFY_SSL: bool = Field(
        default=True,
        description="Verify SSL certificates for GitLab API calls"
    )
    
    GITLAB_CA_CERT_PATH: str = Field(
        default="",
        description="Path to custom CA certificate bundle for self-hosted GitLab with custom SSL certs"
    )
    
    GITLAB_TIMEOUT: float = Field(
        default=30.0,
        description="HTTP timeout for GitLab API calls in seconds"
    )
    
    GITLAB_MAX_RETRIES: int = Field(
        default=3,
        description="Maximum number of retries for transient GitLab API failures"
    )


@lru_cache()
def get_gitlab_settings() -> GitLabSettings:
    """
    Creates a cached instance of GitLabSettings.
    
    This ensures that the settings are loaded only once and reused across the application.
    
    Returns:
        GitLabSettings: Cached GitLab configuration instance
    """
    return GitLabSettings()

