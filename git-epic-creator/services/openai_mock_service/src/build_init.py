"""Build-time initialization: prepare embeddings model.

This script ensures the embeddings model is available locally by downloading it
via sentence-transformers and saving it into a target directory. It also warms
the HF cache for subsequent runs.
"""

import os
from pathlib import Path
from sentence_transformers import SentenceTransformer  # type: ignore
import structlog
logger = structlog.get_logger(__name__)

# Embeddings model repo and target directory (aligns with runtime expectations)
EMBED_MODEL_REPO = "jinaai/jina-embeddings-v2-base-en"
DEFAULT_TARGET_DIR = "/app/models/embeddings/jina-embeddings-v2-base-en"


def main() -> None:
    """Download and save the embeddings model for offline use."""
    target_dir = Path(os.getenv("EMBED_MODEL_DIR", DEFAULT_TARGET_DIR))
    target_dir.mkdir(parents=True, exist_ok=True)
    
    logger.info("build_init_started", model=EMBED_MODEL_REPO, target_dir=str(target_dir))
    
    try:
        # Download model from Hugging Face
        logger.info("downloading_model", model=EMBED_MODEL_REPO)
        model = SentenceTransformer(
            EMBED_MODEL_REPO, 
            model_kwargs={"attn_implementation": "eager"},
            trust_remote_code=True
        )
        
        # Save using save_pretrained() to ensure proper Hugging Face format
        # This creates config.json, tokenizer files, and model weights correctly
        logger.info("saving_model", target_dir=str(target_dir))
        model.save_pretrained(str(target_dir))
        
        # Verify the model was saved correctly by checking for essential files
        config_file = target_dir / "config.json"
        if not config_file.exists():
            raise RuntimeError(f"Model config.json not found after save in {target_dir}")
        
        logger.info("build_init_success", model=EMBED_MODEL_REPO, target_dir=str(target_dir))
        
    except Exception as exc:
        logger.error("build_init_failed", error=str(exc), model=EMBED_MODEL_REPO)
        raise RuntimeError(f"Failed to initialize embeddings model: {exc}")


if __name__ == "__main__":
    main()


