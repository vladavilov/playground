from typing import Any, Dict
import time
import httpx
from fastapi import APIRouter

from ..config import get_retrieval_settings
from ..services.clients import get_neo4j_session


health_router = APIRouter(prefix="/health", tags=["Health"])


@health_router.get("/llm")
async def llm_health() -> Dict[str, Any]:
    settings = get_retrieval_settings()
    url = settings.OAI_BASE_URL.rstrip("/")
    start = time.perf_counter()
    try:
        async with httpx.AsyncClient(timeout=settings.OAI_TIMEOUT_SEC) as client:
            resp = await client.request("HEAD", url)
        latency_ms = (time.perf_counter() - start) * 1000.0
        return {
            "healthy": 200 <= resp.status_code < 400,
            "status_code": resp.status_code,
            "latency_ms": round(latency_ms, 2),
        }
    except Exception as e:  # noqa: BLE001
        latency_ms = (time.perf_counter() - start) * 1000.0
        return {"healthy": False, "error": str(e), "latency_ms": round(latency_ms, 2)}


@health_router.get("/neo4j")
async def neo4j_health() -> Dict[str, Any]:
    start = time.perf_counter()
    try:
        with get_neo4j_session() as session:
            list(session.run("RETURN 1 AS ok"))
        latency_ms = (time.perf_counter() - start) * 1000.0
        return {"healthy": True, "latency_ms": round(latency_ms, 2)}
    except Exception as e:  # noqa: BLE001
        latency_ms = (time.perf_counter() - start) * 1000.0
        return {"healthy": False, "error": str(e), "latency_ms": round(latency_ms, 2)}


@health_router.get("/ready")
async def readiness() -> Dict[str, Any]:
    from fastapi.responses import JSONResponse

    llm = await llm_health()
    neo = await neo4j_health()
    all_ok = bool(llm.get("healthy")) and bool(neo.get("healthy"))
    status_code = 200 if all_ok else 503
    payload = {"ready": all_ok, "components": {"llm": llm, "neo4j": neo}}
    return JSONResponse(payload, status_code=status_code)


