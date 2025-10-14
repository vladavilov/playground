from typing import Any, Dict, List
from fastapi import APIRouter, Depends, HTTPException
import structlog
from auth import require_authentication
from configuration.common_config import get_app_settings
from embeddings.service import EmbeddingService

logger = structlog.get_logger(__name__)
router = APIRouter()

# Initialize embedding service
embedding_service = EmbeddingService()


@router.post("/embeddings", dependencies=[Depends(require_authentication)])
@router.post("/v1/embeddings", dependencies=[Depends(require_authentication)])
async def embeddings(body: Dict[str, Any]) -> Dict[str, Any]:
    settings = get_app_settings()
    model = body.get("model")
    input_value = body.get("input")
    if not model or input_value is None:
        raise HTTPException(status_code=400, detail="Bad Request")

    items: List[str]
    # Normalize various input shapes to a list of strings, per OpenAI spec:
    # - str → [str]
    # - list[str] → list[str]
    # - list[list[str]] (pretokenized) → join tokens with space
    # - dict with "text" → [str(text)]
    # - any other types → coerced with str()
    def _coerce_to_str(x: Any) -> str:
        if isinstance(x, str):
            return x
        if isinstance(x, dict) and "text" in x:
            return str(x.get("text"))
        if isinstance(x, list):
            # Pre-tokenized or nested list: join tokens/items as text
            try:
                return " ".join(str(t) for t in x)
            except Exception:
                return str(x)
        return str(x)

    if isinstance(input_value, list):
        items = [_coerce_to_str(v) for v in input_value]
    else:
        items = [_coerce_to_str(input_value)]
    
    try:
        vectors = embedding_service.embed_texts(items)
        # Normalize vector dimensions to align with requested model where applicable
        target_dim = EmbeddingService.target_dim_for_model(str(model))
        if target_dim is not None:
            vectors = [EmbeddingService.fit_dim(vec, target_dim) for vec in vectors]
        data = [
            {"object": "embedding", "index": idx, "embedding": vec}
            for idx, vec in enumerate(vectors)
        ]
    except Exception as exc:
        logger.error("embedding_failed", error=str(exc))
        # Explicitly surface failure instead of returning small dummy vectors
        raise HTTPException(status_code=500, detail=f"Embedding model unavailable: {exc}")

    return {
        "object": "list",
        "data": data,
        "model": settings.llm.OAI_EMBED_MODEL,
        "usage": {"prompt_tokens": 0, "total_tokens": 0},
    }

