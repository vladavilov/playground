import os
import json
import re
import uuid
from datetime import datetime, timezone
from typing import Any, Dict, List, Optional

from fastapi import Depends, FastAPI, HTTPException, Request
from fastapi.responses import ORJSONResponse
from fastapi.exceptions import RequestValidationError
import structlog
import uvicorn
from sentence_transformers import SentenceTransformer


# Ensure Hugging Face cache dir uses HF_HOME to avoid deprecation warning
if os.getenv("HF_HOME") is None:
    os.environ.setdefault("HF_HOME", os.getenv("TRANSFORMERS_CACHE", "/models/hf-cache"))


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
    }


def require_authentication(request: Request) -> None:
    config = get_config()
    required_key = config["OAI_KEY"]
    if not required_key:
        return
    auth_header = request.headers.get("authorization")
    expected = f"Bearer {required_key}"
    if auth_header != expected:
        raise HTTPException(status_code=401, detail="Unauthorized")


def configure_logging() -> None:
    """Minimal structlog setup compatible with other services."""
    structlog.configure(
        processors=[
            structlog.processors.add_log_level,
            structlog.processors.TimeStamper(fmt="iso"),
            structlog.processors.JSONRenderer(),
        ],
    )


configure_logging()
logger = structlog.get_logger(__name__)


app = FastAPI(default_response_class=ORJSONResponse)




@app.exception_handler(RequestValidationError)
async def request_validation_exception_handler(
    request: Request, exc: RequestValidationError
) -> ORJSONResponse:
    return ORJSONResponse(status_code=400, content={"detail": "Bad Request"})


@app.get("/health")
async def health() -> Dict[str, str]:
    logger.info("health_check")
    return {"status": "ok"}


@app.get("/models", dependencies=[Depends(require_authentication)])
@app.get("/v1/models", dependencies=[Depends(require_authentication)])
async def list_models() -> Dict[str, Any]:
    config = get_config()
    models_list = [config["OAI_MODEL"], config["OAI_EMBED_MODEL"]]
    logger.info(
        "list_models",
        oai_model=config["OAI_MODEL"],
        oai_embed_model=config["OAI_EMBED_MODEL"],
        response_models=models_list,
    )
    return {
        "object": "list",
        "data": [
            {"id": config["OAI_MODEL"], "object": "model", "owned_by": "mock"},
            {
                "id": config["OAI_EMBED_MODEL"],
                "object": "model",
                "owned_by": "mock",
            },
        ],
    }


@app.post("/chat/completions", dependencies=[Depends(require_authentication)])
@app.post("/v1/chat/completions", dependencies=[Depends(require_authentication)])
async def chat_completions(body: Dict[str, Any]) -> Dict[str, Any]:
    config = get_config()
    model = body.get("model")
    messages = body.get("messages")
    if not model or messages is None:
        raise HTTPException(status_code=400, detail="Bad Request")
    user_prompt: str = ""
    try:
        # Build a tiny prompt from the last user message if available
        if isinstance(messages, list) and messages:
            for msg in reversed(messages):
                if isinstance(msg, dict) and msg.get("role") == "user":
                    user_prompt = str(msg.get("content", ""))
                    break
        max_tokens_raw: Optional[int] = None
        try:
            if isinstance(body, dict) and body.get("max_tokens") is not None:
                max_tokens_raw = int(body.get("max_tokens"))
        except Exception:
            max_tokens_raw = None

        generated = _generate_text_locally(
            user_prompt,
            max_new_tokens=max_tokens_raw,
        )
    except Exception as exc:  # Fallback to deterministic string
        logger.warning("chat_generation_failed", error=str(exc))
        generated = "{\n  \"nodes\": [],\n  \"relationships\": []\n}"

    logger.info("completion_output", text=generated)
    return {
        "id": "cmpl-mock-000",
        "object": "chat.completion",
        "created": 1700000000,
        "model": config["OAI_MODEL"],
        "choices": [
            {
                "index": 0,
                "message": {"role": "assistant", "content": generated},
                "finish_reason": "stop",
            }
        ],
        "usage": {"prompt_tokens": 3, "completion_tokens": 2, "total_tokens": 5},
    }


@app.post("/embeddings", dependencies=[Depends(require_authentication)])
@app.post("/v1/embeddings", dependencies=[Depends(require_authentication)])
async def embeddings(body: Dict[str, Any]) -> Dict[str, Any]:
    config = get_config()
    model = body.get("model")
    input_value = body.get("input")
    if not model or input_value is None:
        raise HTTPException(status_code=400, detail="Bad Request")

    items: List[str]
    if isinstance(input_value, list):
        items = input_value
    else:
        items = [input_value]
    try:
        vectors = _embed_texts_locally(items)
        data = [
            {"object": "embedding", "index": idx, "embedding": vec}
            for idx, vec in enumerate(vectors)
        ]
    except Exception as exc:
        logger.warning("embedding_failed_fallback", error=str(exc))
        vector = [0.001, 0.002, 0.003, 0.004, 0.005, 0.006, 0.007, 0.008]
        data = [
            {"object": "embedding", "index": idx, "embedding": vector}
            for idx, _ in enumerate(items)
        ]

    return {
        "object": "list",
        "data": data,
        "model": config["OAI_EMBED_MODEL"],
        "usage": {"prompt_tokens": 0, "total_tokens": 0},
    }


def _load_embedder():
    """Lazy-load sentence-transformers model from local directory.

    Returns the model or None if unavailable.
    """
    # Hardcoded long-context embedding model directory
    model_dir = os.path.join(os.getcwd(), "models", "embeddings", "jina-embeddings-v2-base-en")
    # Load from the locally downloaded snapshot; suppress attention warning via model_kw
    model = SentenceTransformer(
        model_dir,
        model_kwargs={"attn_implementation": "eager"},
    )
    # Truncate long inputs to speed up and avoid OOM
    # Hardcode 10k token window for embeddings
    model.max_seq_length = 10000
    return model

def _embed_texts_locally(texts: List[str]) -> List[List[float]]:
    embedder = _load_embedder()
    if embedder is None:
        raise RuntimeError("Embedder not available")
    # Return plain python lists to avoid numpy dependency overhead
    embeddings = embedder.encode(
        texts,
        show_progress_bar=False,
        convert_to_numpy=False,
        normalize_embeddings=False,
    )
    return [[float(x) for x in vec] for vec in embeddings]


def _generate_text_locally(
    prompt: str,
    *,
    max_new_tokens: Optional[int] = None,
) -> str:
    # Return a deterministic JSON graph with placeholders filled
    del max_new_tokens  # unused in mock

    # Try to extract a UUID from the prompt; fall back to a new UUID
    project_id: str
    try:
        uuid_match = re.search(
            r"\b[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[1-5][0-9a-fA-F]{3}-[89abAB][0-9a-fA-F]{3}-[0-9a-fA-F]{12}\b",
            prompt or "",
        )
        project_id = uuid_match.group(0) if uuid_match else str(uuid.uuid4())
    except Exception:
        project_id = str(uuid.uuid4())

    now_iso = datetime.now(timezone.utc).strftime("%Y-%m-%dT%H:%M:%SZ")

    title0 = "5235ce59-ea32-44bd-8d88-b988c399fcd6_concurrent_test_0.pdf"
    title1 = "57a9a7b9-b8a3-40e7-b88f-c477ce7eb09a_concurrent_test_1.pdf"

    payload: Dict[str, Any] = {
        "nodes": [
            {
                "id": "P",
                "label": "PROJECT",
                "properties": {
                    "project_id": project_id,
                    "title": f"Project {project_id[:8]}",
                    "creation_date": now_iso,
                    "modification_date": now_iso,
                },
            },
            {
                "id": "D0",
                "label": "DOCUMENT",
                "properties": {
                    "project_id": project_id,
                    "title": title0,
                    "file_name": title0,
                    "file_type": "pdf",
                    "content_type": "application/pdf",
                    "creation_date": now_iso,
                    "modification_date": now_iso,
                },
            },
            {
                "id": "D1",
                "label": "DOCUMENT",
                "properties": {
                    "project_id": project_id,
                    "title": title1,
                    "file_name": title1,
                    "file_type": "pdf",
                    "content_type": "application/pdf",
                    "creation_date": now_iso,
                    "modification_date": now_iso,
                },
            },
        ],
        "relationships": [
            {
                "type": "HAS_DOCUMENT",
                "start_node_id": "P",
                "end_node_id": "D0",
                "properties": {"source": "mock"},
            },
            {
                "type": "HAS_DOCUMENT",
                "start_node_id": "P",
                "end_node_id": "D1",
                "properties": {"source": "mock"},
            },
        ],
    }

    return json.dumps(payload, ensure_ascii=False, indent=2)


if __name__ == "__main__":
    cfg = get_config()
    uvicorn.run(app, host="0.0.0.0", port=int(cfg["API_PORT"]))


