"""Service-specific configuration for Neo4j Retrieval Service."""

from functools import lru_cache
from pydantic import Field

from configuration.base_config import BaseConfig
from configuration.llm_config import LlmConfig
from configuration.neo4j_config import Neo4jSettings
from configuration.vector_index_config import VectorIndexEnv


class RetrievalSettings(BaseConfig):
    """Settings specific to the GraphRAG/Neo4j retrieval service.

    Reuses shared configuration modules for LLM, Neo4j, and vector index naming.
    """

    # Compose shared configs
    llm: LlmConfig = Field(default_factory=LlmConfig)
    neo4j: Neo4jSettings = Field(default_factory=Neo4jSettings)
    vector_index: VectorIndexEnv = Field(default_factory=VectorIndexEnv)

    # Service-local controls
    OAI_TIMEOUT_SEC: float = Field(default=10.0, description="HTTP/LLM client timeout in seconds")
    LLM_TEMPERATURE: float = Field(default=0.0, ge=0.0, le=2.0)


@lru_cache()
def get_retrieval_settings() -> RetrievalSettings:
    return RetrievalSettings()
