from typing import Any, Dict
from fastapi import APIRouter, Depends
import structlog
from auth import require_authentication
from configuration.common_config import get_app_settings

logger = structlog.get_logger(__name__)
router = APIRouter()


def ensure_model_in_body(deployment: str, body: Dict[str, Any]) -> Dict[str, Any]:
    """Ensure model field is present in body, using deployment name if missing."""
    if "model" not in body:
        return {**body, "model": deployment}
    return body


@router.get("/openai/deployments", dependencies=[Depends(require_authentication)])
async def list_deployments() -> Dict[str, Any]:
    """List Azure OpenAI deployments (compatible with models listing)."""
    settings = get_app_settings()
    logger.info(
        "list_deployments",
        oai_model=settings.llm.OAI_MODEL,
        oai_embed_model_name=settings.llm.OAI_EMBED_MODEL_NAME,
        oai_embed_deployment_name=settings.llm.embedding_deployment_name
    )
    return {
        "data": [
            {"id": settings.llm.OAI_MODEL, "model": settings.llm.OAI_MODEL, "object": "deployment"},
            {"id": settings.llm.embedding_deployment_name, "model": settings.llm.OAI_EMBED_MODEL_NAME, "object": "deployment"},
        ]
    }


@router.post("/openai/deployments/{deployment}/chat/completions", dependencies=[Depends(require_authentication)])
@router.post("/v1/openai/deployments/{deployment}/chat/completions", dependencies=[Depends(require_authentication)])
async def azure_chat_completions(deployment: str, body: Dict[str, Any]) -> Dict[str, Any]:
    """Azure OpenAI chat completions endpoint - delegates to standard chat handler."""
    # Import here to avoid circular dependency
    from routers.chat import chat_completions
    
    body_with_model = ensure_model_in_body(deployment, body)
    return await chat_completions(body_with_model)


@router.post("/openai/deployments/{deployment}/embeddings", dependencies=[Depends(require_authentication)])
@router.post("/v1/openai/deployments/{deployment}/embeddings", dependencies=[Depends(require_authentication)])
async def azure_embeddings(deployment: str, body: Dict[str, Any]) -> Dict[str, Any]:
    """Azure OpenAI embeddings endpoint - delegates to standard embeddings handler."""
    # Import here to avoid circular dependency
    from routers.embeddings import embeddings
    
    body_with_model = ensure_model_in_body(deployment, body)
    return await embeddings(body_with_model)

