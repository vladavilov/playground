from pydantic_settings import BaseSettings, SettingsConfigDict, PydanticBaseSettingsSource
from azure.identity import DefaultAzureCredential
from azure.appconfiguration.provider import load
import os
import logging
from typing import Any, Dict, Tuple, Type, List

logger = logging.getLogger(__name__)

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
        except Exception as e:
            logger.warning(f"Failed to load Azure App Configuration: {e}")
            return {}

    def get_field_value(self, field, field_name):
        return self.app_config.get(field_name), field_name, False

    def __call__(self) -> Dict[str, Any]:
        return self.app_config


class Settings(BaseSettings):
    """
    Configuration settings for the Data Ingestion Service Orchestrator.
    Supports both Azure cloud and local fallback modes.
    """
    ADAPTER_URLS: str
    
    # Azure Service Bus settings
    SERVICE_BUS_NAMESPACE: str = ""
    SERVICE_BUS_QUEUE_NAME: str
    
    # Local fallback settings
    USE_LOCAL_FALLBACK: bool = False
    LOCAL_SERVICE_BUS_PATH: str = "./data/local_queue"
    
    # Service Bus Emulator settings (for docker)
    SERVICE_BUS_EMULATOR_NAMESPACE: str = "localhost:5672"
    SERVICE_BUS_EMULATOR_CONNECTION_STRING: str = "Endpoint=sb://localhost:5672;SharedAccessKeyName=RootManageSharedAccessKey;SharedAccessKey=SAS_KEY_VALUE;UseDevelopmentEmulator=true"

    model_config = SettingsConfigDict(env_file=".env", env_file_encoding="utf-8")

    @property
    def adapter_urls_list(self) -> List[str]:
        return [url.strip() for url in self.ADAPTER_URLS.split(",") if url.strip()]
    
    @property
    def is_azure_available(self) -> bool:
        """Check if Azure services are configured and available."""
        if self.USE_LOCAL_FALLBACK:
            return False
            
        # Check if Azure Service Bus is configured
        if not self.SERVICE_BUS_NAMESPACE:
            return False
            
        # Try to authenticate with Azure
        try:
            credential = DefaultAzureCredential()
            # Try to get a token to verify Azure authentication
            credential.get_token("https://management.azure.com/.default")
            return True
        except Exception as e:
            logger.warning(f"Azure authentication failed: {e}")
            return False
    
    @property
    def service_bus_endpoint(self) -> str:
        """Get the appropriate Service Bus endpoint based on availability."""
        if self.is_azure_available:
            return f"{self.SERVICE_BUS_NAMESPACE}.servicebus.windows.net"
        else:
            return self.SERVICE_BUS_EMULATOR_NAMESPACE

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