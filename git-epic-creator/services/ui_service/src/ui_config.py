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

    # GitLab OAuth (managed in UI service)
    GITLAB_BASE_URL: str = Field(default="", description="Base URL of GitLab instance for server-side API calls")
    GITLAB_CLIENT_BASE_URL: str = Field(default="", description="Base URL of GitLab for browser redirects (falls back to GITLAB_BASE_URL)")
    GITLAB_OAUTH_CLIENT_ID: str = Field(default="", description="GitLab OAuth client id")
    GITLAB_OAUTH_CLIENT_SECRET: str = Field(default="", description="GitLab OAuth client secret")
    GITLAB_OAUTH_REDIRECT_URI: str = Field(default="", description="GitLab OAuth redirect URI")
    GITLAB_OAUTH_SCOPES: str = Field(default="read_api", description="Space/comma separated scopes")
    GITLAB_VERIFY_SSL: bool = Field(default=True, description="Verify TLS for GitLab calls")


@lru_cache()
def get_ui_settings() -> UiSettings:
    """Return cached UI settings."""
    return UiSettings()


