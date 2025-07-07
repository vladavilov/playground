from pydantic_settings import BaseSettings, SettingsConfigDict, PydanticBaseSettingsSource
from azure.identity import DefaultAzureCredential
from azure.appconfiguration.provider import load
import os
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
    Sentiment API Service application settings.
    
    Defines the configuration parameters for the News Sentiment Score Service (NSSS).
    Priority: .env > environment variables > Azure App Configuration
    """
    API_PORT: int = 8000
    AZURE_APPCONFIG_ENDPOINT: str = ""
    
    # Cosmos DB Configuration
    AZURE_COSMOSDB_ENDPOINT: str = ""
    COSMOS_EMULATOR_ENDPOINT: str = "https://cosmosdb-emulator:8081"
    COSMOS_EMULATOR_KEY: str = "C2y6yDjf5/R+ob0N8A7Cgv30VRDJIWEHLM+4QDU5DE2nQ9nDuVTqobD4b8mGGyPMbIZnqyMsEcaGQy67XIw/Jw=="
    USE_LOCAL_FALLBACK: bool = False
    COSMOS_DATABASE_NAME: str = "NewsDatabase"
    COSMOS_CONTAINER_NAME: str = "EnrichedNewsEvents"

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

settings = Settings() 