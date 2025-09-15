"""GraphRAG retrieval Service FastAPI application.

Provides health endpoints for LLM base URL and Neo4j connectivity,
plus an aggregated readiness endpoint.
"""

from typing import Any, Dict
import time
import structlog
import httpx
from fastapi import FastAPI
from utils.app_factory import FastAPIFactory
from .routers.retrieval_router import retrieval_router

from configuration.logging_config import configure_logging
from configuration.common_config import get_app_settings
from .config import get_retrieval_settings
from .services.clients import get_neo4j_session


# Initialize logging before creating the app
configure_logging()
logger = structlog.get_logger(__name__)


app: FastAPI = FastAPIFactory.create_app(
    title="GraphRAG retrieval Service",
    description="GraphRAG retrieval service, providing enhanced context for ai workflow service",
    version="1.0.0",
    enable_azure_auth=False,
    enable_docs_auth=False,
    enable_cors=True,
    enable_postgres=False,
    enable_neo4j=False,
    enable_redis=False,
)

app.include_router(retrieval_router, prefix="/retrieve", tags=["Retrieval"])

@app.get("/health/llm", tags=["Health"])
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


@app.get("/health/neo4j", tags=["Health"])
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


@app.get("/health/ready", tags=["Health"])
async def readiness() -> Dict[str, Any]:
    from fastapi.responses import JSONResponse

    llm = await llm_health()
    neo = await neo4j_health()
    all_ok = bool(llm.get("healthy")) and bool(neo.get("healthy"))
    status_code = 200 if all_ok else 503
    payload = {"ready": all_ok, "components": {"llm": llm, "neo4j": neo}}
    return JSONResponse(payload, status_code=status_code)

if __name__ == "__main__":
    settings = get_app_settings()
    import uvicorn

    uvicorn.run(app, host="0.0.0.0", port=settings.API_PORT)


