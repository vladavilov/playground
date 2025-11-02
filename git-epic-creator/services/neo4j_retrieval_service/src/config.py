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

    # Prompt size management
    MAX_CHUNKS_FOR_PROMPT: int = Field(
        default=20,
        ge=5,
        le=100,
        description="Maximum number of chunks to include in local executor prompt to prevent token overflow"
    )
    MAX_COMMUNITIES_FOR_PROMPT: int = Field(
        default=10,
        ge=3,
        le=50,
        description="Maximum number of communities to include in prompt contexts"
    )
    MAX_CHUNK_TEXT_LENGTH: int = Field(
        default=1500,
        ge=100,
        le=5000,
        description="Maximum characters per chunk text in prompts"
    )
    
    # Early exit configuration
    CONFIDENCE_THRESHOLD_EARLY_EXIT: float = Field(
        default=0.85,
        ge=0.0,
        le=1.0,
        description="Stop processing followups if confidence exceeds this threshold"
    )
    MIN_FOLLOWUPS_BEFORE_EXIT: int = Field(
        default=2,
        ge=1,
        le=10,
        description="Process at least N followups before allowing early exit"
    )
    MAX_EMPTY_FOLLOWUPS_BEFORE_EXIT: int = Field(
        default=2,
        ge=1,
        le=10,
        description="Stop processing if N consecutive followups return no data (empty communities/chunks)"
    )
    
    # Request caching (deduplication always enabled)
    REQUEST_CACHE_TTL_SEC: int = Field(
        default=300,
        ge=0,
        le=3600,
        description="TTL for cached retrieval results (0=disabled)"
    )


@lru_cache()
def get_retrieval_settings() -> RetrievalSettings:
    return RetrievalSettings()
