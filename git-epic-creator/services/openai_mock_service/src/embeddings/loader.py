import os
from typing import List
import structlog

logger = structlog.get_logger(__name__)


def load_embedder():
    """Lazy-load a local Jina embeddings model from models/embeddings/jina-embeddings-v2-base-en.

    Tries Sentence-Transformers first. If that fails (e.g., local snapshot lacks
    sentence-transformers specific files), falls back to raw Transformers with
    mean pooling. Returns an object exposing a callable "encode(texts: List[str]) -> List[List[float]]".
    Raises RuntimeError if the model cannot be loaded locally.
    """
    model_dir = os.path.join(os.getcwd(), "models", "embeddings", "jina-embeddings-v2-base-en")
    if not os.path.isdir(model_dir):
        raise RuntimeError(f"Embedding model directory not found: {model_dir}")

    # Attempt Sentence-Transformers loading path
    try:
        from sentence_transformers import SentenceTransformer  # type: ignore

        st_model = SentenceTransformer(
            model_dir,
            model_kwargs={"attn_implementation": "eager"},
        )
        # Truncate long inputs to speed up and avoid OOM
        st_model.max_seq_length = 10000
        logger.info("embedder_loaded", backend="sentence-transformers", path=model_dir)
        return st_model
    except Exception as exc:
        logger.warning("embedder_st_load_failed", error=str(exc))

    # Fallback: load with raw Transformers and implement mean pooling
    try:
        import torch  # type: ignore
        from transformers import AutoModel, AutoTokenizer  # type: ignore

        class TransformersMeanPoolEmbedder:
            def __init__(self, directory: str) -> None:
                self.tokenizer = AutoTokenizer.from_pretrained(directory, local_files_only=True)
                self.model = AutoModel.from_pretrained(directory, local_files_only=True)
                self.model.eval()

            def _mean_pool(self, last_hidden_state, attention_mask):
                # Mean pooling excluding padding tokens
                input_mask_expanded = attention_mask.unsqueeze(-1).expand(last_hidden_state.size()).float()
                sum_embeddings = torch.sum(last_hidden_state * input_mask_expanded, dim=1)
                sum_mask = torch.clamp(input_mask_expanded.sum(dim=1), min=1e-9)
                embeddings = sum_embeddings / sum_mask
                # L2 normalize
                embeddings = torch.nn.functional.normalize(embeddings, p=2, dim=1)
                return embeddings

            def encode(self, texts: List[str], show_progress_bar: bool = False, convert_to_numpy: bool = False, normalize_embeddings: bool = False):  # type: ignore[override]
                # normalize_embeddings handled inherently by L2 normalization above
                with torch.no_grad():
                    inputs = self.tokenizer(
                        texts,
                        padding=True,
                        truncation=True,
                        max_length=10000,
                        return_tensors="pt",
                    )
                    outputs = self.model(**inputs)
                    last_hidden_state = outputs.last_hidden_state
                    pooled = self._mean_pool(last_hidden_state, inputs["attention_mask"])  # [batch, hidden]
                    if convert_to_numpy:
                        return pooled.cpu().numpy()
                    return pooled.cpu().tolist()

        hf_embedder = TransformersMeanPoolEmbedder(model_dir)
        # Probe once to confirm it works
        _ = hf_embedder.encode(["test"], show_progress_bar=False, convert_to_numpy=False)
        logger.info("embedder_loaded", backend="transformers-mean-pool", path=model_dir)
        return hf_embedder
    except Exception as exc:
        logger.error("embedder_transformers_load_failed", error=str(exc))
        raise RuntimeError(f"Failed to load local embedding model from {model_dir}: {exc}")

