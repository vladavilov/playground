from pydantic_settings import BaseSettings, SettingsConfigDict, PydanticBaseSettingsSource
from azure.identity import DefaultAzureCredential
from azure.appconfiguration.provider import load
import os
import logging
from typing import Any, Dict, Tuple, Type

class AzureAppConfigurationSource(PydanticBaseSettingsSource):
    """
    A settings source that loads variables from Azure App Configuration.
    """
    def __init__(self, settings_cls: Type[BaseSettings]):
        super().__init__(settings_cls)
        self.app_config = self._load_config()

    def _load_config(self) -> Dict[str, Any]:
        """Loads configuration from Azure App Configuration."""
        endpoint = os.environ.get("AZURE_APPCONFIG_ENDPOINT")
        if not endpoint:
            return {}
        
        try:
            credential = DefaultAzureCredential()
            config = load(endpoint=endpoint, credential=credential)
            return dict(config)
        except Exception:
            # Could log a warning here
            return {}

    def get_field_value(self, field, field_name):
        return self.app_config.get(field_name), field_name, False

    def __call__(self) -> Dict[str, Any]:
        return self.app_config


class Settings(BaseSettings):
    """
    Benzinga Adapter application settings.
    
    Defines the configuration parameters for the service.
    Priority: .env > environment variables > Azure App Configuration
    """
    API_PORT: int = 8000
    BENZINGA_API_TOKEN: str = "YOUR_TOKEN_HERE"
    AZURE_APPCONFIG_ENDPOINT: str = ""
    
    # Logging configuration
    LOG_LEVEL: str = "INFO"
    LOG_FORMAT: str = "%(asctime)s - %(name)s - %(levelname)s - %(message)s"
    LOG_DATE_FORMAT: str = "%Y-%m-%d %H:%M:%S"

    model_config = SettingsConfigDict(
        env_file=".env",
        env_file_encoding='utf-8',
        extra='ignore'
    )

    @classmethod
    def settings_customise_sources(
        cls,
        settings_cls: Type[BaseSettings],
        init_settings: PydanticBaseSettingsSource,
        env_settings: PydanticBaseSettingsSource,
        dotenv_settings: PydanticBaseSettingsSource,
        file_secret_settings: PydanticBaseSettingsSource,
    ) -> Tuple[PydanticBaseSettingsSource, ...]:

        return (
            dotenv_settings,
            env_settings,
            AzureAppConfigurationSource(settings_cls),
            init_settings,
            file_secret_settings,
        )

    def get_log_level(self) -> int:
        """Convert string log level to logging constant"""
        return getattr(logging, self.LOG_LEVEL.upper(), logging.INFO)

settings = Settings() 