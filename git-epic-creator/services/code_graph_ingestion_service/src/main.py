"""Code Graph Ingestion Service FastAPI application.

- FastAPI app wiring via shared FastAPIFactory (Postgres enabled)
"""

from __future__ import annotations

import structlog
from fastapi import FastAPI

from configuration.logging_config import configure_logging
from configuration.common_config import get_app_settings
from utils.app_factory import FastAPIFactory
from observability.tracing import init_tracing

from api.routers.ingest import router as ingest_router

# Initialize logging before creating the app
configure_logging()
logger = structlog.get_logger(__name__)
init_tracing("code-graph-ingestion-service")

app: FastAPI = FastAPIFactory.create_app(
    title="Code Graph Ingestion Service",
    description="Deterministic multi-language repo ingestion and persistence via neo4j-repository-service",
    version="0.1.0",
    enable_cors=True,
    enable_postgres=True,
    enable_neo4j=False,
    enable_redis=False,
)

app.include_router(ingest_router)


if __name__ == "__main__":
    settings = get_app_settings()
    import uvicorn

    uvicorn.run(app, host="0.0.0.0", port=settings.API_PORT)

