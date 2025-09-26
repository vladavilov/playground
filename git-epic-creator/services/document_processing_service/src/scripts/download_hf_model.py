from __future__ import annotations

import os
from huggingface_hub import snapshot_download


def main() -> int:
    repo_id = os.getenv("HF_REPO_ID", "HuggingFaceTB/SmolVLM-256M-Instruct")
    local_dir = os.getenv("HF_LOCAL_DIR", "/opt/hf-cache/models")

    os.makedirs(local_dir, exist_ok=True)
    try:
        snapshot_download(
            repo_id=repo_id,
            local_dir=local_dir,
            local_dir_use_symlinks=False,
            cache_dir=os.getenv("HUGGINGFACE_HUB_CACHE", None),
        )
        print(f"Model '{repo_id}' downloaded to '{local_dir}'")
        return 0
    except Exception as exc:
        print(f"Warning: failed to predownload model '{repo_id}': {exc}")
        return 0


if __name__ == "__main__":
    raise SystemExit(main())


