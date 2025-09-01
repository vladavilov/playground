"""AI Workflow Service FastAPI application.

Provides health endpoints including Redis (via FastAPIFactory) and GraphRAG connectivity,
plus an aggregated readiness endpoint.
"""

from typing import Any, Dict
import time
import structlog
import httpx
from routers.workflow_router import router as workflow_router
from fastapi import Depends, FastAPI
from utils.app_factory import FastAPIFactory, get_redis_client_from_state
import redis.asyncio as redis

from configuration.logging_config import configure_logging
from configuration.common_config import get_app_settings
from config import get_ai_workflow_settings


# Initialize logging before creating the app
configure_logging()
logger = structlog.get_logger(__name__)


app: FastAPI = FastAPIFactory.create_app(
    title="AI Workflow Service",
    description="Agentic AI workflow orchestration and health endpoints",
    version="1.0.0",
    enable_azure_auth=False,
    enable_docs_auth=False,
    enable_cors=True,
    enable_postgres=False,
    enable_neo4j=False,
    enable_redis=True,
)

app.include_router(workflow_router, prefix="/workflow", tags=["Workflow"])  # noqa: E402

@app.get("/health/graphrag", tags=["Health"])
async def graphrag_health() -> Dict[str, Any]:
    """Check reachability of GraphRAG base URL using a HEAD request.

    Returns a JSON payload with healthy flag, status code (if any),
    and latency in milliseconds. Does not raise on errors.
    """
    settings = get_ai_workflow_settings()
    url = settings.GRAPH_RAG_BASE_URL.rstrip("/")
    timeout = float(settings.GRAPH_RAG_TIMEOUT_SEC)

    start = time.perf_counter()
    try:
        async with httpx.AsyncClient(timeout=timeout) as client:
            resp = await client.request("HEAD", url)
        latency_ms = (time.perf_counter() - start) * 1000.0
        return {
            "healthy": 200 <= resp.status_code < 400,
            "status_code": resp.status_code,
            "latency_ms": round(latency_ms, 2),
        }
    except Exception as e:  # noqa: BLE001 - return error details for health
        latency_ms = (time.perf_counter() - start) * 1000.0
        return {
            "healthy": False,
            "error": str(e),
            "latency_ms": round(latency_ms, 2),
        }


@app.get("/health/ready", tags=["Health"])
async def readiness(
    redis_client: redis.Redis = Depends(get_redis_client_from_state),
) -> Dict[str, Any]:
    """Aggregate readiness across Redis and GraphRAG.

    - Redis: ping and basic info
    - GraphRAG: reuse graphrag health check logic via internal call

    Returns 200 if all components are healthy, else 503.
    """
    # Redis health
    redis_details: Dict[str, Any]
    try:
        ping_ok = await redis_client.ping()
        info = await redis_client.info()
        redis_details = {
            "healthy": bool(ping_ok),
            "version": info.get("redis_version"),
        }
    except Exception as e:  # noqa: BLE001
        redis_details = {"healthy": False, "error": str(e)}

    # GraphRAG health (call local function to avoid extra HTTP hop)
    graphrag_details = await graphrag_health()

    all_ok = bool(redis_details.get("healthy")) and bool(graphrag_details.get("healthy"))
    status_code = 200 if all_ok else 503

    # FastAPI will infer status_code=200 from return, but we want to set explicit code.
    # Return payload and let tests assert based on client response status.
    from fastapi.responses import JSONResponse

    payload = {
        "ready": all_ok,
        "components": {
            "redis": redis_details,
            "graphrag": graphrag_details,
        },
    }
    return JSONResponse(payload, status_code=status_code)


if __name__ == "__main__":
    settings = get_app_settings()
    import uvicorn

    uvicorn.run(app, host="0.0.0.0", port=settings.API_PORT)


