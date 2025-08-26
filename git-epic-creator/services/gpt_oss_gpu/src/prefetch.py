import os
import sys
from huggingface_hub import snapshot_download


def main() -> int:
    model_id = os.environ.get("MODEL_ID", "openai/gpt-oss-20b")
    local_dir = os.environ.get("HF_HOME", "/opt/hf_cache")
    token = os.environ.get("HF_TOKEN")

    os.makedirs(local_dir, exist_ok=True)
    snapshot_download(
        repo_id=model_id,
        local_dir=local_dir,
        local_dir_use_symlinks=False,
        token=token,
        allow_patterns=["original/*"],
    )
    return 0


if __name__ == "__main__":
    sys.exit(main())


