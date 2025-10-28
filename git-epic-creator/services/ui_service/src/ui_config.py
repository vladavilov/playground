"""
UI service specific configuration settings.
"""

from functools import lru_cache
from pydantic import Field

from configuration.base_config import BaseConfig


class UiSettings(BaseConfig):
    """
    Pydantic-backed settings for UI service runtime configuration.
    """

    # Session/cookie configuration
    SESSION_SECRET_KEY: str = Field(default="", description="Secret key for session cookie HMAC")
    SESSION_COOKIE_NAME: str = Field(default="ui_session", description="Session cookie name")
    SESSION_MAX_AGE: int = Field(default=14 * 24 * 3600, description="Session max age (seconds)")
    SESSION_SAME_SITE: str = Field(default="lax", description="Session SameSite policy")
    ALLOW_INSECURE_SESSION: bool = Field(default=False, description="Allow ephemeral secret in dev")

    # GitLab Client Service URL (for proxying GitLab-related requests)
    GITLAB_CLIENT_BASE_URL: str = Field(default="http://gitlab-client-service:8000", description="Base URL of gitlab-client-service for proxying")


@lru_cache()
def get_ui_settings() -> UiSettings:
    """Return cached UI settings."""
    return UiSettings()


