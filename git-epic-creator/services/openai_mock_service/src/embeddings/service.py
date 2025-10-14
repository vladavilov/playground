from typing import List, Optional
import hashlib
import random
from configuration.vector_index_config import get_vector_index_env


class EmbeddingService:
    """Service for generating deterministic mock embeddings."""

    def __init__(self):
        pass

    def embed_texts(self, texts: List[str]) -> List[List[float]]:
        """
        Generate deterministic pseudo-random embeddings for a list of texts.

        Returns vectors of length VectorIndexEnv.VECTOR_INDEX_DIMENSIONS using a
        stable seed derived from the input text to make outputs reproducible.
        """
        vector_env = get_vector_index_env()
        target_dim = int(vector_env.VECTOR_INDEX_DIMENSIONS)

        vectors: List[List[float]] = []
        for text in texts:
            seed_bytes = hashlib.md5(str(text).encode("utf-8")).digest()
            seed_int = int.from_bytes(seed_bytes, byteorder="big", signed=False)
            rng = random.Random(seed_int)
            # Generate small float values centered near 0.0
            vec = [rng.uniform(-0.01, 0.01) for _ in range(target_dim)]
            vectors.append(vec)
        return vectors

    @staticmethod
    def target_dim_for_model(model_name: str) -> Optional[int]:
        """Get target dimension for specific model names."""
        name = (model_name or "").lower()
        # Align with common OpenAI embedding dims
        if "text-embedding-3-small" in name:
            return 1536
        if "text-embedding-3-large" in name:
            return 3072
        return None

    @staticmethod
    def fit_dim(vec: List[float], target_dim: int) -> List[float]:
        """Fit vector to target dimension by padding or truncating."""
        if target_dim <= 0:
            return vec
        length = len(vec)
        if length == target_dim:
            return vec
        if length > target_dim:
            return vec[:target_dim]
        # pad with zeros to target length
        return vec + [0.0] * (target_dim - length)

