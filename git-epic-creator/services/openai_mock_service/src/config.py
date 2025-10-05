import os
from typing import Dict, Optional


def get_environment_variable(name: str, default: Optional[str] = None) -> Optional[str]:
    value = os.getenv(name)
    return value if value is not None else default


def get_config() -> Dict[str, Optional[str]]:
    return {
        "API_PORT": get_environment_variable("API_PORT", "8000"),
        "OAI_KEY": get_environment_variable("OAI_KEY"),
        "OAI_MODEL": get_environment_variable("OAI_MODEL", "gpt-4.1"),
        "OAI_EMBED_MODEL": get_environment_variable(
            "OAI_EMBED_MODEL", "text-embedding-3-large"
        ),
        "EMBEDDINGS_SIZE": get_environment_variable("VECTOR_INDEX_DIMENSIONS", "1536"),
    }

