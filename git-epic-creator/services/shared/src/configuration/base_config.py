"""
Base configuration settings for all services.
"""
from pydantic_settings import BaseSettings, SettingsConfigDict

class BaseConfig(BaseSettings):
    """
    Base configuration class that all other settings classes should inherit from.
    """
    model_config = SettingsConfigDict(
        env_file=".env",
        env_file_encoding='utf-8',
        case_sensitive=True,
        extra='ignore'
    ) 