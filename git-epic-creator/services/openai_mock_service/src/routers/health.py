from typing import Dict
from fastapi import APIRouter
import structlog

logger = structlog.get_logger(__name__)
router = APIRouter()


@router.get("/health")
async def health() -> Dict[str, str]:
    logger.info("health_check")
    return {"status": "ok"}

