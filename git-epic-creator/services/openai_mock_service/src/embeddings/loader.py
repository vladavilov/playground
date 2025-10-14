import os
from typing import List
import structlog
import numpy as np  # type: ignore

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
    
    # Check for essential model files
    config_path = os.path.join(model_dir, "config.json")
    if not os.path.isfile(config_path):
        raise RuntimeError(
            f"Embedding model directory exists but config.json not found: {model_dir}. "
            "The model may not have been properly downloaded during build. "
            "Rebuild the image with PRECACHE_MODELS=true."
        )

    # Attempt Sentence-Transformers loading path
    try:
        from sentence_transformers import SentenceTransformer  # type: ignore

        st_model = SentenceTransformer(
            model_dir,
            model_kwargs={"attn_implementation": "eager"},
            trust_remote_code=True,
        )
        # Cap max sequence length to prevent excessive memory usage
        try:
            default_max = getattr(st_model, "max_seq_length", None)
            if isinstance(default_max, int) and default_max > 0:
                effective_max = min(default_max, 2048)
            else:
                effective_max = 2048
            # Prefer attribute assignment; SentenceTransformers also supports set_max_seq_length
            try:
                st_model.max_seq_length = effective_max  # type: ignore[attr-defined]
            except Exception:
                try:
                    set_max_len = getattr(st_model, "set_max_seq_length", None)
                    if callable(set_max_len):
                        set_max_len(effective_max)
                except Exception:
                    pass
            logger.info("embedder_max_seq_length_set", backend="sentence-transformers", value=effective_max)
        except Exception as _exc:
            logger.warning("embedder_max_seq_length_cap_failed", backend="sentence-transformers")
        logger.info("embedder_loaded", backend="sentence-transformers", path=model_dir)
        return st_model
    except Exception as exc:
        logger.warning("embedder_st_load_failed", error=str(exc), model_dir=model_dir)

    # Fallback: load with raw Transformers and implement mean pooling
    try:
        import torch  # type: ignore
        from transformers import AutoModel, AutoTokenizer  # type: ignore

        class TransformersMeanPoolEmbedder:
            def __init__(self, directory: str) -> None:
                self.tokenizer = AutoTokenizer.from_pretrained(directory, local_files_only=True)
                self.model = AutoModel.from_pretrained(directory, local_files_only=True)
                self.model.eval()
                # Derive a safe max length from model/tokenizer hints, capped at 2048
                max_pos = getattr(self.model.config, "max_position_embeddings", None)
                if isinstance(max_pos, int) and max_pos > 0:
                    self.max_length = min(max_pos, 2048)
                else:
                    t_max = getattr(self.tokenizer, "model_max_length", None)
                    # Some tokenizers use very large sentinel values; guard against those
                    if isinstance(t_max, int) and 0 < t_max < 100000:
                        self.max_length = min(t_max, 2048)
                    else:
                        self.max_length = 2048
                logger.info("embedder_max_seq_length_set", backend="transformers-mean-pool", value=self.max_length)

            def _mean_pool(self, last_hidden_state, attention_mask):
                # Mean pooling excluding padding tokens
                input_mask_expanded = attention_mask.unsqueeze(-1).expand(last_hidden_state.size()).float()
                sum_embeddings = torch.sum(last_hidden_state * input_mask_expanded, dim=1)
                sum_mask = torch.clamp(input_mask_expanded.sum(dim=1), min=1e-9)
                embeddings = sum_embeddings / sum_mask
                # L2 normalize
                embeddings = torch.nn.functional.normalize(embeddings, p=2, dim=1)
                return embeddings

            def encode(self, texts: List[str], show_progress_bar: bool = False, convert_to_numpy: bool = False, normalize_embeddings: bool = False, batch_size: int = 1):  # type: ignore[override]
                # normalize_embeddings handled inherently by L2 normalization above
                # Process in small batches to limit memory usage
                if not isinstance(batch_size, int) or batch_size <= 0:
                    batch_size = 1
                results = []
                with torch.no_grad():
                    for start in range(0, len(texts), batch_size):
                        batch_texts = texts[start:start + batch_size]
                        inputs = self.tokenizer(
                            batch_texts,
                            padding=True,
                            truncation=True,
                            max_length=self.max_length,
                            return_tensors="pt",
                        )
                        outputs = self.model(**inputs)
                        last_hidden_state = outputs.last_hidden_state
                        pooled = self._mean_pool(last_hidden_state, inputs["attention_mask"])  # [batch, hidden]
                        results.extend(pooled.cpu().tolist())
                if convert_to_numpy:
                    try:
                        return np.array(results)
                    except Exception:
                        return results
                return results

        hf_embedder = TransformersMeanPoolEmbedder(model_dir)
        # Probe once to confirm it works
        _ = hf_embedder.encode(["test"], show_progress_bar=False, convert_to_numpy=False)
        logger.info("embedder_loaded", backend="transformers-mean-pool", path=model_dir)
        return hf_embedder
    except Exception as exc:
        logger.error("embedder_transformers_load_failed", error=str(exc))
        raise RuntimeError(f"Failed to load local embedding model from {model_dir}: {exc}")

