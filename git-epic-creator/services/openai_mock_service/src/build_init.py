"""Build-time initialization: prepare embeddings model.

This script ensures the embeddings model is available locally without fully
materializing the model in memory during the image build. It mirrors the
Hugging Face repository into the target directory and warms the HF cache for
any custom code dependencies required at runtime (offline).
"""

import os
from pathlib import Path
import structlog
from huggingface_hub import snapshot_download  # type: ignore
logger = structlog.get_logger(__name__)

# Embeddings model repo and target directory (aligns with runtime expectations)
EMBED_MODEL_REPO = "jinaai/jina-embeddings-v2-base-en"
IMPL_REPO = "jinaai/jina-bert-implementation"  # custom code referenced via trust_remote_code
DEFAULT_TARGET_DIR = "/app/models/embeddings/jina-embeddings-v2-base-en"


def main() -> None:
    """Download model files into image and warm cache for offline use."""
    target_dir = Path(os.getenv("EMBED_MODEL_DIR", DEFAULT_TARGET_DIR))
    target_dir.mkdir(parents=True, exist_ok=True)
    
    logger.info("build_init_started", model=EMBED_MODEL_REPO, target_dir=str(target_dir))
    
    try:
        # 1) Mirror the model repository to the desired target directory without loading the model into memory
        logger.info("snapshot_download_model_repo", repo=EMBED_MODEL_REPO, target=str(target_dir))
        snapshot_download(
            repo_id=EMBED_MODEL_REPO,
            local_dir=str(target_dir),
            local_dir_use_symlinks=False,
        )

        # 2) Warm cache for the custom implementation repo referenced by trust_remote_code
        #    so runtime can operate fully offline
        logger.info("snapshot_download_impl_repo", repo=IMPL_REPO)
        snapshot_download(repo_id=IMPL_REPO, local_dir_use_symlinks=False)

        # 3) Verify the model directory contains essential files
        config_file = target_dir / "config.json"
        if not config_file.exists():
            raise RuntimeError(f"Model config.json not found after snapshot in {target_dir}")

        logger.info("build_init_success", model=EMBED_MODEL_REPO, target_dir=str(target_dir))
        
    except Exception as exc:
        logger.error("build_init_failed", error=str(exc), model=EMBED_MODEL_REPO)
        raise RuntimeError(f"Failed to initialize embeddings model: {exc}")


if __name__ == "__main__":
    main()


