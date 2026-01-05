"""
Authentication service session (cookie) configuration.

The authentication service owns browser login/session management.
"""

from functools import lru_cache

from pydantic import Field

from configuration.base_config import BaseConfig


class SessionSettings(BaseConfig):
    """
    Pydantic-backed settings for browser session cookie runtime configuration.
    """

    SESSION_SECRET_KEY: str = Field(default="", description="Secret key for session cookie HMAC")
    SESSION_COOKIE_NAME: str = Field(default="ui_session", description="Session cookie name")
    SESSION_MAX_AGE: int = Field(default=14 * 24 * 3600, description="Session max age (seconds)")
    SESSION_SAME_SITE: str = Field(default="lax", description="Session SameSite policy")
    ALLOW_INSECURE_SESSION: bool = Field(default=False, description="Allow ephemeral secret in dev")


@lru_cache()
def get_session_settings() -> SessionSettings:
    """Return cached session settings."""
    return SessionSettings()


