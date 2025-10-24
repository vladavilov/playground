from typing import Dict, Any
from configuration.graphrag_config import get_graphrag_settings
from configuration.llm_config import get_llm_config


def configure_settings_for_json() -> Dict[str, Any]:
    """
    Build a GraphRAG settings dict configured for JSON inputs and Azure/OpenAI models.

    - Returns the final settings dict without writing to disk
    - Uses LiteLLM format (type: chat/embedding, model_provider: azure)
    - Uses separate model_name (for tiktoken) and deployment_name (for Azure API) for embeddings
    
    Reference: https://microsoft.github.io/graphrag/config/models/
    """
    settings: Dict[str, Any] = {}

    gr = get_graphrag_settings()
    llm = get_llm_config()
    models = settings.get("models", {})
    chat_model_id = "default_chat_model"
    embedding_model_id = "default_embedding_model"

    azure_base = llm.OAI_BASE_URL
    azure_api_version = llm.OAI_API_VERSION
    chat_model_name = llm.OAI_MODEL
    
    # Use separate embedding model name and deployment name for proper tiktoken usage
    embed_model_name = llm.OAI_EMBED_MODEL_NAME
    azure_embed_deployment = llm.embedding_deployment_name
    
    azure_chat_deployment = chat_model_name
    api_key_value = llm.OAI_KEY

    chat_cfg = {
        "type": "chat",  # LiteLLM generic type (not azure_openai_chat)
        "model_provider": "azure",  # Required for LiteLLM Azure integration
        "api_base": azure_base,
        "api_version": azure_api_version,
        "model": chat_model_name,
        "deployment_name": azure_chat_deployment,
        "auth_type": "api_key",
        "api_key": api_key_value,
        "model_supports_json": True,
        "async_mode": gr.GRAPHRAG_ASYNC_MODE,
        "temperature": gr.GRAPHRAG_LLM_TEMPERATURE,  # Deterministic extraction (0 = no randomness)
        "top_p": gr.GRAPHRAG_LLM_TOP_P,  # Nucleus sampling (1.0 = consider all tokens)
        "n": gr.GRAPHRAG_LLM_N,  # Number of completions (1 = single response)
    }
    embed_cfg = {
        "type": "embedding",  # LiteLLM generic type (not azure_openai_embedding)
        "model_provider": "azure",  # Required for LiteLLM Azure integration
        "api_base": azure_base,
        "api_version": azure_api_version,
        "model": embed_model_name,  # Model name for tiktoken
        "deployment_name": azure_embed_deployment,  # Deployment name for Azure API
        "auth_type": "api_key",
        "api_key": api_key_value,
        "async_mode": gr.GRAPHRAG_ASYNC_MODE, 
    }
    chat_cfg["parallelization_num_threads"] = gr.GRAPHRAG_LLM_THREAD_COUNT
    chat_cfg["parallelization_stagger"] = gr.GRAPHRAG_LLM_THREAD_STAGGER
    chat_cfg["concurrent_requests"] = gr.GRAPHRAG_LLM_CONCURRENT_REQUESTS
    chat_cfg["tokens_per_minute"] = gr.GRAPHRAG_LLM_TOKENS_PER_MINUTE
    chat_cfg["requests_per_minute"] = gr.GRAPHRAG_LLM_REQUESTS_PER_MINUTE

    embed_cfg["parallelization_num_threads"] = gr.GRAPHRAG_EMBEDDING_THREAD_COUNT
    embed_cfg["parallelization_stagger"] = gr.GRAPHRAG_EMBEDDING_THREAD_STAGGER
    embed_cfg["concurrent_requests"] = gr.GRAPHRAG_EMBEDDING_CONCURRENT_REQUESTS
    embed_cfg["tokens_per_minute"] = gr.GRAPHRAG_EMBEDDING_TOKENS_PER_MINUTE
    embed_cfg["requests_per_minute"] = gr.GRAPHRAG_EMBEDDING_REQUESTS_PER_MINUTE
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

    # Output configuration: write all artifacts under workspace/output
    output_cfg = settings.get("output", {}) or {}
    output_cfg["type"] = output_cfg.get("type", "file")
    output_cfg["base_dir"] = output_cfg.get("base_dir", "output")
    settings["output"] = output_cfg

    # Cache configuration: disable caching to avoid filesystem directory creation issues
    # Using 'none' type bypasses caching entirely, avoiding both file system and memory issues
    # This is acceptable since the pipeline runs once per project
    settings["cache"] = {"type": "none"}

    # Vector store: GraphRAG expects a mapping of named stores → VectorStoreConfig
    # Provide the expected default id used by the pipeline: "default_vector_store"
    settings["vector_store"] = {
        "default_vector_store": {
            "type": "lancedb",
            "db_uri": "output/lancedb",
            "container_name": "default",
            "overwrite": True,
        }
    }

    # Explicitly configure extract_graph for deterministic, reproducible extraction
    # Using temperature=0, top_p=1, n=1 ensures consistent LLM outputs across runs
    # This prevents schema drift where relationship dicts omit keys like 'description', 'source_id', 'weight'
    settings["extract_graph"] = {
        "model_id": chat_model_id,
        "max_gleanings": gr.GRAPHRAG_EXTRACT_MAX_GLEANINGS,
        "temperature": gr.GRAPHRAG_LLM_TEMPERATURE,  # 0 = deterministic (no randomness)
        "top_p": gr.GRAPHRAG_LLM_TOP_P,  # 1.0 = consider all tokens (no nucleus sampling)
        "n": gr.GRAPHRAG_LLM_N,  # 1 = single completion per request
    }

    return settings


