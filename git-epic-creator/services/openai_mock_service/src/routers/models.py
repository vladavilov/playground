from typing import Any, Dict
from fastapi import APIRouter, Depends
import structlog
from auth import require_authentication
from configuration.common_config import get_app_settings

logger = structlog.get_logger(__name__)
router = APIRouter()


@router.get("/models", dependencies=[Depends(require_authentication)])
@router.get("/v1/models", dependencies=[Depends(require_authentication)])
async def list_models() -> Dict[str, Any]:
    settings = get_app_settings()
    models_list = [settings.llm.OAI_MODEL, settings.llm.OAI_EMBED_MODEL_NAME]
    logger.info(
        "list_models",
        oai_model=settings.llm.OAI_MODEL,
        oai_embed_model=settings.llm.OAI_EMBED_MODEL_NAME,
        response_models=models_list,
    )
    return {
        "object": "list",
        "data": [
            {"id": settings.llm.OAI_MODEL, "object": "model", "owned_by": "mock"},
            {
                "id": settings.llm.OAI_EMBED_MODEL_NAME,
                "object": "model",
                "owned_by": "mock",
            },
        ],
    }

