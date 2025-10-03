"""AI Tasks Service FastAPI application.

Provides health endpoints including Redis (via FastAPIFactory) and external service connectivity,
plus the /tasks/generate endpoint for backlog generation.
"""
import structlog
from routers.tasks_router import router as tasks_router
from fastapi import FastAPI
from utils.app_factory import FastAPIFactory

from configuration.logging_config import configure_logging
from configuration.common_config import get_app_settings

# Initialize logging before creating the app
configure_logging()
logger = structlog.get_logger(__name__)


app: FastAPI = FastAPIFactory.create_app(
    title="AI Tasks Service",
    description="Agentic AI backlog generation orchestration and health endpoints",
    version="1.0.0",
    enable_cors=True,
    enable_postgres=False,
    enable_neo4j=False,
    enable_redis=True,
)

app.include_router(tasks_router, prefix="/tasks", tags=["Tasks"])

if __name__ == "__main__":
    settings = get_app_settings()
    import uvicorn

    uvicorn.run(app, host="0.0.0.0", port=settings.API_PORT)


