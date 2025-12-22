"""Health endpoints for neo4j_retrieval_service.

Why this exists:
- `FastAPIFactory` does not consistently expose `/health/llm` and `/health/ready` across services.
- This service is commonly used via MCP; fast, explicit health checks help upstream routing.
"""

from __future__ import annotations

from typing import Any, Dict, Optional

import httpx
import structlog
from fastapi import APIRouter, Depends
from fastapi.responses import JSONResponse

from config import get_retrieval_settings
from utils.app_factory import get_redis_client_from_state

logger = structlog.get_logger(__name__)

health_router = APIRouter(prefix="/health", tags=["Health"])


def _build_models_url(base_url: Optional[str]) -> Optional[str]:
    if not base_url:
        return None
    base = base_url.rstrip("/")
    # If caller already provided a /v1 base, honor it.
    if base.endswith("/v1"):
        return f"{base}/models"
    return f"{base}/v1/models"


@health_router.get("/llm")
async def llm_health() -> Dict[str, Any]:
    """Check OpenAI-compatible endpoint reachability."""
    settings = get_retrieval_settings()
    url = _build_models_url(settings.llm.OAI_BASE_URL)

    if not url:
        return {"healthy": False, "error": "OAI_BASE_URL is not configured"}

    timeout = float(getattr(settings.llm, "LLM_TIMEOUT_SEC", 5.0))

    try:
        async with httpx.AsyncClient(timeout=timeout) as client:
            resp = await client.request("GET", url)
        return {"healthy": resp.status_code < 500, "status_code": resp.status_code}
    except Exception as exc:
        logger.warning("llm_health_check_failed", error=str(exc), error_type=type(exc).__name__)
        return {"healthy": False, "error": str(exc)}


@health_router.get("/ready")
async def ready(
    redis_client=Depends(get_redis_client_from_state),
) -> Dict[str, Any]:
    """Aggregate readiness.

    Ready iff:
    - Redis is reachable (when enabled by factory)
    - LLM endpoint is reachable
    """
    llm = await llm_health()

    redis_ok = False
    redis_details: Dict[str, Any] = {"healthy": False}
    try:
        if redis_client is None:
            redis_details = {"healthy": False, "error": "redis_client not configured"}
        else:
            pong = await redis_client.ping()
            redis_ok = bool(pong)
            redis_details = {"healthy": redis_ok}
    except Exception as exc:
        redis_details = {"healthy": False, "error": str(exc)}

    ready_flag = bool(llm.get("healthy")) and redis_ok
    payload = {
        "ready": ready_flag,
        "components": {
            "llm": llm,
            "redis": redis_details,
        },
    }
    return JSONResponse(status_code=(200 if ready_flag else 503), content=payload)

