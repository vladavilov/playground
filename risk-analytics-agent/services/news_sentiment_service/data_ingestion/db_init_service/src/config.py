"""Configuration settings for Cosmos DB initialization service."""

from pydantic_settings import BaseSettings


class Settings(BaseSettings):
    """Configuration settings for Cosmos DB initialization."""
    
    # Cosmos DB Configuration
    COSMOS_DB_NAME: str = "risk-analytics-db"
    CONTAINER_NAME: str = "enriched-news-events"
    
    # Azure Cosmos DB settings (for production)
    AZURE_COSMOSDB_ENDPOINT: str = ""
    
    # Local emulator settings
    USE_LOCAL_FALLBACK: bool = True
    COSMOS_EMULATOR_ENDPOINT: str = "https://cosmosdb-emulator:8081"
    COSMOS_EMULATOR_KEY: str = "C2y6yDjf5/R+ob0N8A7Cgv30VRDJIWEHLM+4QDU5DE2nQ9nDuVTqobD4b8mGGyPMbIZnqyMsEcaGQy67XIw/Jw=="
    
    # Initialization settings
    MAX_RETRIES: int = 30
    RETRY_DELAY: int = 10
    CONTAINER_THROUGHPUT: int = 400  # Minimum for emulator
    
    class Config:
        env_file = ".env"
        case_sensitive = True 