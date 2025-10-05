from typing import List, Optional
from config import get_config
from embeddings.loader import load_embedder


class EmbeddingService:
    """Service for generating embeddings using local Jina model."""

    def __init__(self):
        self._embedder = None

    def embed_texts(self, texts: List[str]) -> List[List[float]]:
        """
        Generate embeddings for a list of texts.
        
        Args:
            texts: List of text strings to embed
            
        Returns:
            List of embedding vectors (list of floats)
        """
        if self._embedder is None:
            self._embedder = load_embedder()
        
        target_dim = int(get_config()["EMBEDDINGS_SIZE"] or "1536")
        
        # Return plain python lists to avoid numpy dependency overhead
        embeddings = self._embedder.encode(  # type: ignore[attr-defined]
            texts,
            show_progress_bar=False,
            convert_to_numpy=False,
            normalize_embeddings=False,
        )
        
        # Ensure primitives and fit to target dimension
        vectors: List[List[float]] = []
        for vec in embeddings:
            lst = [float(x) for x in vec]
            # Pad or truncate to target_dim
            if len(lst) < target_dim:
                lst = lst + [0.0] * (target_dim - len(lst))
            elif len(lst) > target_dim:
                lst = lst[:target_dim]
            vectors.append(lst)
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

