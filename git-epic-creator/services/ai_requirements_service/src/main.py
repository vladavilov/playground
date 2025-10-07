"""AI Requirements Service FastAPI application.

Provides health endpoints including Redis (via FastAPIFactory) and GraphRAG connectivity,
plus an aggregated readiness endpoint.
"""
import structlog
from routers.workflow_router import router as workflow_router
from fastapi import FastAPI
from utils.app_factory import FastAPIFactory

from configuration.logging_config import configure_logging
from configuration.common_config import get_app_settings
from config import get_ai_requirements_settings


# Initialize logging before creating the app
configure_logging()
logger = structlog.get_logger(__name__)


app: FastAPI = FastAPIFactory.create_app(
    title="AI Requirements Service",
    description="Agentic AI requirements generation and health endpoints",
    version="1.0.0",
    enable_cors=True,
    enable_postgres=False,
    enable_neo4j=False,
    enable_redis=True,
)

app.include_router(workflow_router, prefix="/workflow", tags=["Workflow"])  # noqa: E402

if __name__ == "__main__":
    settings = get_app_settings()
    import uvicorn

    uvicorn.run(app, host="0.0.0.0", port=settings.API_PORT)


