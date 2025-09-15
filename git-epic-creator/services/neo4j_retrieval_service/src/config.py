"""Service-specific configuration for Neo4j Retrieval Service."""

from functools import lru_cache
from pydantic import Field

from configuration.base_config import BaseConfig


class RetrievalSettings(BaseConfig):
    """Settings specific to the GraphRAG/Neo4j retrieval service."""

    # OpenAI-compatible LLM endpoints
    OAI_BASE_URL: str = Field(
        default="http://openai-mock-service:8000/v1",
        description="Base URL for OpenAI-compatible API",
    )
    OAI_KEY: str = Field(default="key", description="API key for OpenAI-compatible API")
    OAI_MODEL: str = Field(default="gpt-4.1", description="Chat model name")
    OAI_EMBED_MODEL: str = Field(
        default="text-embedding-3-small", description="Embedding model name"
    )
    OAI_TIMEOUT_SEC: float = Field(default=10.0, description="HTTP timeout in seconds")
    LLM_TEMPERATURE: float = Field(default=0.0, ge=0.0, le=2.0)

    # Neo4j connection
    NEO4J_URI: str = Field(default="bolt://neo4j:7687")
    NEO4J_USERNAME: str = Field(default="neo4j")
    NEO4J_PASSWORD: str = Field(default="neo4j123")
    NEO4J_DATABASE: str = Field(default="neo4j")

    # Index names
    GRAPHRAG_COMM_INDEX: str = Field(default="graphrag_comm_index")
    GRAPHRAG_CHUNK_INDEX: str = Field(default="graphrag_chunk_index")

    # Retrieval defaults
    RETRIEVAL_DEFAULT_TOP_K: int = Field(default=1, ge=1)


@lru_cache()
def get_retrieval_settings() -> RetrievalSettings:
    return RetrievalSettings()


