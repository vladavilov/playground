from functools import lru_cache
from pydantic import Field
from .base_config import BaseConfig


class VectorIndexEnv(BaseConfig):
    """Environment-backed settings for all vector indexes (shared + per-index)."""

    # Shared vector parameters (used by all indexes)
    VECTOR_INDEX_PROPERTY: str = Field(default="embedding", description="Embedding property name on nodes")
    VECTOR_INDEX_DIMENSIONS: int = Field(default=3072, description="Embedding vector dimensions")
    VECTOR_INDEX_SIMILARITY: str = Field(default="cosine", description="Similarity function (cosine/euclidean/dot_product)")

    # Chunk index
    CHUNK_VECTOR_INDEX_NAME: str = Field(default="graphrag_chunk_index", description="Chunk vector index name")
    CHUNK_VECTOR_INDEX_LABEL: str = Field(default="__Chunk__", description="Chunk node label")

    # Community index
    COMMUNITY_VECTOR_INDEX_NAME: str = Field(default="graphrag_comm_index", description="Community vector index name")
    COMMUNITY_VECTOR_INDEX_LABEL: str = Field(default="__Community__", description="Community node label")

    # Entity index
    ENTITY_VECTOR_INDEX_NAME: str = Field(default="graphrag_entity_index", description="Entity vector index name")
    ENTITY_VECTOR_INDEX_LABEL: str = Field(default="__Entity__", description="Entity node label")

@lru_cache()
def get_vector_index_env() -> VectorIndexEnv:
    """Cached loader for vector index environment settings."""
    return VectorIndexEnv()


__all__ = ["VectorIndexEnv", "get_vector_index_env"]


