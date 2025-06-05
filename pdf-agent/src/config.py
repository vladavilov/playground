import functools
from pydantic_settings import BaseSettings, SettingsConfigDict
from pathlib import Path

class AppSettings(BaseSettings):
    API_PORT: int = 8000
    LOG_LEVEL: str = "INFO"

    TEMP_FILE_DIR: Path = Path("./temp_files")

    # Azure OpenAI Settings (sensitive, no defaults, loaded from env)
    AZURE_OPENAI_API_KEY: str
    AZURE_OPENAI_ENDPOINT: str
    AZURE_OPENAI_LLM_DEPLOYMENT_NAME: str
    AZURE_OPENAI_EMBEDDINGS_DEPLOYMENT_NAME: str

    PROPERTY_GROUPS_CONFIG_PATH: Path = Path("config/property_groups.yaml")

    # Pydantic-settings configuration
    # This tells Pydantic to load variables from a .env file if it exists
    # and to treat environment variable names as case-insensitive (though by default they are case-sensitive on Linux/macOS and case-insensitive on Windows for pydantic-settings).
    model_config = SettingsConfigDict(env_file='.env', extra='ignore', case_sensitive=False)

@functools.lru_cache(maxsize=1)
def get_settings() -> AppSettings:
    """Initializes and returns the AppSettings instance, creating temp dir."""
    app_settings = AppSettings()
    app_settings.TEMP_FILE_DIR.mkdir(parents=True, exist_ok=True)
    return app_settings