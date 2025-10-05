from typing import Any, Dict
from fastapi import APIRouter, Depends
import structlog
from auth import require_authentication
from config import get_config

logger = structlog.get_logger(__name__)
router = APIRouter()


@router.get("/models", dependencies=[Depends(require_authentication)])
@router.get("/v1/models", dependencies=[Depends(require_authentication)])
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

