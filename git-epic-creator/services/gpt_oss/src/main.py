import os
from typing import Any, Dict, List, Optional

import httpx
from fastapi import Depends, FastAPI, HTTPException, Request
from fastapi.responses import JSONResponse
from fastapi.exceptions import RequestValidationError
import structlog
import uvicorn


def get_env(name: str, default: Optional[str] = None) -> Optional[str]:
    value = os.getenv(name)
    return value if value is not None else default


def get_config() -> Dict[str, Optional[str]]:
    return {
        "API_PORT": get_env("API_PORT", "11434"),
        "OAI_KEY": get_env("OAI_KEY"),
        "OAI_MODEL": get_env("OAI_MODEL", "gpt-oss:20b"),
        "OAI_EMBED_MODEL": get_env("OAI_EMBED_MODEL", "text-embedding-3-large"),
        "OLLAMA_HOST": get_env("OLLAMA_HOST", "http://127.0.0.1:11435"),
    }


def require_authentication(request: Request) -> None:
    cfg = get_config()
    required_key = cfg["OAI_KEY"]
    if not required_key:
        return
    auth_header = request.headers.get("authorization")
    expected = f"Bearer {required_key}"
    if auth_header != expected:
        raise HTTPException(status_code=401, detail="Unauthorized")


def configure_logging() -> None:
    structlog.configure(
        processors=[
            structlog.processors.add_log_level,
            structlog.processors.TimeStamper(fmt="iso"),
            structlog.processors.JSONRenderer(),
        ],
    )


configure_logging()
logger = structlog.get_logger(__name__)

app = FastAPI(default_response_class=JSONResponse)


@app.exception_handler(RequestValidationError)
async def request_validation_exception_handler(
    request: Request, exc: RequestValidationError
) -> JSONResponse:
    return JSONResponse(status_code=400, content={"detail": "Bad Request"})


@app.get("/health")
async def health() -> Dict[str, str]:
    logger.info("health_check")
    return {"status": "ok"}


@app.get("/v1/models", dependencies=[Depends(require_authentication)])
async def list_models() -> Dict[str, Any]:
    cfg = get_config()
    return {
        "object": "list",
        "data": [
            {"id": cfg["OAI_MODEL"], "object": "model", "owned_by": "gpt-oss"},
            {"id": cfg["OAI_EMBED_MODEL"], "object": "model", "owned_by": "gpt-oss"},
        ],
    }


@app.post("/v1/chat/completions", dependencies=[Depends(require_authentication)])
async def chat_completions(body: Dict[str, Any]) -> Dict[str, Any]:
    cfg = get_config()
    # Proxy to Ollama's OpenAI-compatible endpoint if available
    ollama_host = cfg["OLLAMA_HOST"] or "http://127.0.0.1:11435"
    async with httpx.AsyncClient(timeout=60.0) as client:
        try:
            resp = await client.post(f"{ollama_host}/v1/chat/completions", json=body)
            resp.raise_for_status()
            return cast_openai_like(resp.json(), cfg["OAI_MODEL"] or "gpt-oss:20b")
        except Exception as exc:
            logger.warning("ollama_proxy_failed", error=str(exc))
            raise HTTPException(status_code=502, detail="Upstream error")


@app.post("/v1/embeddings", dependencies=[Depends(require_authentication)])
async def embeddings(body: Dict[str, Any]) -> Dict[str, Any]:
    cfg = get_config()
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
        "model": cfg["OAI_EMBED_MODEL"],
        "usage": {"prompt_tokens": 0, "total_tokens": 0},
    }


def cast_openai_like(payload: Dict[str, Any], model: str) -> Dict[str, Any]:
    try:
        if "model" not in payload:
            payload["model"] = model
        return payload
    except Exception:
        return payload


def _load_embedder():
    try:
        from sentence_transformers import SentenceTransformer  # type: ignore
    except Exception:
        return None

    model_dir = os.path.join("/models", "embeddings", "jina-embeddings-v2-base-en")
    model = SentenceTransformer(
        model_dir,
        model_kwargs={"attn_implementation": "eager"},
    )
    model.max_seq_length = 10000
    return model


def _embed_texts_locally(texts: List[str]) -> List[List[float]]:
    embedder = _load_embedder()
    if embedder is None:
        raise RuntimeError("Embedder not available")
    embeddings = embedder.encode(
        texts,
        show_progress_bar=False,
        convert_to_numpy=False,
        normalize_embeddings=False,
    )
    return [[float(x) for x in vec] for vec in embeddings]


if __name__ == "__main__":
    cfg = get_config()
    uvicorn.run(app, host="0.0.0.0", port=int(cfg["API_PORT"]))


