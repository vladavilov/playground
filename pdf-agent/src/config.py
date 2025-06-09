import functools
import yaml
from pydantic import field_validator, Field
from pydantic_settings import BaseSettings, SettingsConfigDict
from pathlib import Path

class AppSettings(BaseSettings):
    API_PORT: int = 8000
    LOG_LEVEL: str = "INFO"

    TEMP_FILE_DIR: Path = Path("./temp_files")
    MAX_FILE_SIZE_MB: int = 10  # Max file size in megabytes

    # Azure OpenAI Settings
    AZURE_OPENAI_API_KEY: str
    
    # Chat Model Settings
    AZURE_OPENAI_CHAT_ENDPOINT: str
    AZURE_OPENAI_LLM_MODEL_NAME: str = "gpt-4o"
    
    # Local Embedding Model Settings
    EMBEDDING_MODEL_PATH: str = "./embedding_model"

    PROPERTY_GROUPS_CONFIG_PATH: Path = Path("config/property_groups.yaml")
    property_groups: list[dict] = Field(default_factory=list)

    @field_validator("property_groups", mode="before")
    def load_property_groups(cls, v, values):
        """Loads and parses the property groups YAML file."""
        config_path = values.data.get("PROPERTY_GROUPS_CONFIG_PATH")
        if config_path and config_path.exists():
            with open(config_path, "r") as f:
                config = yaml.safe_load(f)
                return config.get("property_groups", [])
        return []

    # Text chunking settings
    CHUNK_SIZE: int = 1000
    CHUNK_OVERLAP: int = 200

    # RAG settings
    TOP_K: int = 3

    # Pydantic-settings configuration
    # and to treat environment variable names as case-insensitive (though by default they are case-sensitive on Linux/macOS and case-insensitive on Windows for pydantic-settings).
    model_config = SettingsConfigDict(extra='ignore', case_sensitive=False)

@functools.lru_cache(maxsize=1)
def get_settings() -> AppSettings:
    """Initializes and returns the AppSettings instance, creating temp dir."""
    app_settings = AppSettings()
    app_settings.TEMP_FILE_DIR.mkdir(parents=True, exist_ok=True)
    return app_settings