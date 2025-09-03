"""
Shared configuration settings for all services.
"""
from functools import lru_cache
from pydantic import Field
from dotenv import load_dotenv
from .base_config import BaseConfig
from .postgres_config import PostgresSettings
from .neo4j_config import Neo4jSettings
from .redis_config import RedisSettings
from .celery_config import CelerySettings
from .http_client_config import HTTPClientSettings
from .blob_storage_config import BlobStorageSettings
from .llm_config import LlmConfig

# Explicitly load .env file at the module level.
load_dotenv()

class AppSettings(BaseConfig):
    """
    Holds the composed settings for the entire application.
    """

    API_PORT: int = Field(
        default=8000,
        description="The port the API will run on"
    )

    postgres: PostgresSettings = Field(default_factory=PostgresSettings)
    neo4j: Neo4jSettings = Field(default_factory=Neo4jSettings)
    redis: RedisSettings = Field(default_factory=RedisSettings)
    celery: CelerySettings = Field(default_factory=CelerySettings)
    http_client: HTTPClientSettings = Field(default_factory=HTTPClientSettings)
    blob_storage: BlobStorageSettings = Field(default_factory=BlobStorageSettings)
    llm: LlmConfig = Field(default_factory=LlmConfig)

@lru_cache()
def get_app_settings() -> AppSettings:
    """
    Creates a cached instance of AppSettings.
    This ensures that all settings are loaded only once and reused.
    """
    return AppSettings() 