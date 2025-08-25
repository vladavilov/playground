"""Build-time initialization: prepare embeddings model.

This script ensures the embeddings model is available locally by downloading it
via sentence-transformers and saving it into a target directory. It also warms
the HF cache for subsequent runs.
"""

import os
from pathlib import Path
from sentence_transformers import SentenceTransformer  # type: ignore

# Embeddings model repo and target directory (aligns with runtime expectations)
EMBED_MODEL_REPO = "jinaai/jina-embeddings-v2-base-en"
DEFAULT_TARGET_DIR = "/models/embeddings/jina-embeddings-v2-base-en"


def main() -> None:
    target_dir = Path(os.getenv("EMBED_MODEL_DIR", DEFAULT_TARGET_DIR))
    target_dir.mkdir(parents=True, exist_ok=True)

    model = SentenceTransformer(EMBED_MODEL_REPO, model_kwargs={"attn_implementation": "eager"})
    # Save a local copy for offline/strict local dir loads
    model.save(str(target_dir))


if __name__ == "__main__":
    main()


