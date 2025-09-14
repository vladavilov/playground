import os
import yaml
from pathlib import Path
from typing import Dict, Any


def _load_yaml(path: Path) -> Dict[str, Any]:
    if not path.exists():
        return {}
    with open(path, "r", encoding="utf-8") as f:
        return yaml.safe_load(f) or {}


def configure_settings_for_json(workspace: Path) -> Dict[str, Any]:
    """
    Build a GraphRAG settings dict configured for JSON inputs and Azure/OpenAI models.

    - Uses existing settings.yaml as a base if present (non-destructive merge)
    - Returns the final settings dict without writing to disk
    """
    settings_path = workspace / "settings.yaml"
    settings: Dict[str, Any] = _load_yaml(settings_path)

    # Models configuration
    models = settings.get("models", {})
    chat_model_id = "default_chat_model"
    embedding_model_id = "default_embedding_model"

    # Always configure Azure OpenAI via OAI_* envs
    azure_base = os.getenv("OAI_BASE_URL", "")
    azure_api_version = os.getenv("OAI_API_VERSION", "2024-02-15-preview")
    chat_model_name = os.getenv("OAI_MODEL", "gpt-4o")
    embed_model_name = os.getenv("OAI_EMBED_MODEL", "text-embedding-3-small")
    azure_chat_deployment = os.getenv("AZURE_OPENAI_CHAT_DEPLOYMENT", chat_model_name)
    azure_embed_deployment = os.getenv("AZURE_OPENAI_EMBED_DEPLOYMENT", embed_model_name)
    api_key_value = os.getenv("OAI_KEY", "")

    chat_cfg = {
        "type": "azure_openai_chat",
        "api_base": azure_base,
        "api_version": azure_api_version,
        "model": chat_model_name,
        "deployment_name": azure_chat_deployment,
        "auth_type": "api_key",
        "api_key": api_key_value,
    }
    embed_cfg = {
        "type": "azure_openai_embedding",
        "api_base": azure_base,
        "api_version": azure_api_version,
        "model": embed_model_name,
        "deployment_name": azure_embed_deployment,
        "auth_type": "api_key",
        "api_key": api_key_value,
    }
    models[chat_model_id] = chat_cfg
    models[embedding_model_id] = embed_cfg

    settings["models"] = models

    # Input configuration: configure JSON ingestion per GraphRAG docs (merge, don't clobber)
    input_cfg = settings.get("input", {}) or {}
    storage_cfg = input_cfg.get("storage", {}) or {}
    # Ensure storage remains pointed to input dir
    storage_cfg["type"] = storage_cfg.get("type", "file")
    storage_cfg["base_dir"] = storage_cfg.get("base_dir", "input")

    # Set JSON mode and mapping
    input_cfg["file_type"] = "json"
    input_cfg["file_pattern"] = ".*\\.json"
    input_cfg["text_column"] = "text"
    input_cfg["title_column"] = "title"
    # Keep only top-level columns that actually exist in the JSON document structure
    # Your JSON has top-level: title, text, metadata (dict). Preserve the nested dict as one column.
    input_cfg["metadata"] = ["metadata"]
    input_cfg["storage"] = storage_cfg
    settings["input"] = input_cfg

    # Vector store: use relative path so GraphRAG joins it against the project root
    settings["vector_store"] = {
        "default": {
            "type": "lancedb",
            "db_uri": "output/lancedb",
            "container_name": "default",
            "overwrite": True,
        }
    }

    embed_text_cfg = settings.get("embed_text", {}) or {}
    embed_text_cfg.setdefault("model_id", embedding_model_id)
    embed_text_cfg.setdefault("vector_store_id", "default")
    settings["embed_text"] = embed_text_cfg

    # Remove legacy or conflicting indexing block so CLI doesn't prefer defaults
    if "indexing" in settings:
        try:
            settings.pop("indexing")
        except Exception:
            pass

    return settings


