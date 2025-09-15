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

    # Wide defaults for rate limits/concurrency; can be overridden via env
    chat_requests_per_minute = int(os.getenv("OAI_CHAT_RPM", "6000"))
    chat_tokens_per_minute = int(os.getenv("OAI_CHAT_TPM", "400000"))
    chat_timeout_seconds = int(os.getenv("OAI_CHAT_TIMEOUT", "60"))
    chat_max_retries = int(os.getenv("OAI_CHAT_MAX_RETRIES", "3"))
    chat_max_concurrency = int(os.getenv("OAI_CHAT_CONCURRENCY", "4"))

    embed_requests_per_minute = int(os.getenv("OAI_EMBED_RPM", "6000"))
    embed_tokens_per_minute = int(os.getenv("OAI_EMBED_TPM", "400000"))
    embed_timeout_seconds = int(os.getenv("OAI_EMBED_TIMEOUT", "60"))
    embed_max_retries = int(os.getenv("OAI_EMBED_MAX_RETRIES", "3"))
    embed_max_concurrency = int(os.getenv("OAI_EMBED_CONCURRENCY", "4"))
    embed_max_batch_size = int(os.getenv("OAI_EMBED_BATCH", "64"))

    chat_cfg = {
        "type": "azure_openai_chat",
        "api_base": azure_base,
        "api_version": azure_api_version,
        "model": chat_model_name,
        "deployment_name": azure_chat_deployment,
        "auth_type": "api_key",
        "api_key": api_key_value,
        # Wide, explicit limits to avoid deadlocks in schedulers
        "requests_per_minute": chat_requests_per_minute,
        "tokens_per_minute": chat_tokens_per_minute,
        "timeout": chat_timeout_seconds,
        "max_retries": chat_max_retries,
        "max_concurrent_requests": chat_max_concurrency,
    }
    embed_cfg = {
        "type": "azure_openai_embedding",
        "api_base": azure_base,
        "api_version": azure_api_version,
        "model": embed_model_name,
        "deployment_name": azure_embed_deployment,
        "auth_type": "api_key",
        "api_key": api_key_value,
        # Wide, explicit limits to avoid deadlocks in schedulers
        "requests_per_minute": embed_requests_per_minute,
        "tokens_per_minute": embed_tokens_per_minute,
        "timeout": embed_timeout_seconds,
        "max_retries": embed_max_retries,
        "max_concurrent_requests": embed_max_concurrency,
        "max_batch_size": embed_max_batch_size,
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
    # Execution parameters for embedding job
    embed_text_cfg.setdefault("batch_size", int(os.getenv("EMBED_TEXT_BATCH_SIZE", "64")))
    embed_text_cfg.setdefault(
        "max_concurrent_requests", int(os.getenv("EMBED_TEXT_CONCURRENCY", "2"))
    )
    embed_text_cfg.setdefault("retry_max_attempts", int(os.getenv("EMBED_TEXT_RETRIES", "3")))
    embed_text_cfg.setdefault("request_timeout", int(os.getenv("EMBED_TEXT_TIMEOUT", "60")))
    settings["embed_text"] = embed_text_cfg

    # Remove legacy or conflicting indexing block so CLI doesn't prefer defaults
    if "indexing" in settings:
        try:
            settings.pop("indexing")
        except Exception:
            pass

    return settings


