"""Project Management Service (FastAPI)."""

import uvicorn
import structlog

from configuration.logging_config import configure_logging
from configuration.common_config import get_app_settings
from utils.app_factory import FastAPIFactory

from routers.project_router import router as project_router

# Configure logging at application startup
configure_logging()
logger = structlog.get_logger(__name__)

app = FastAPIFactory.create_app(
    title="Project Management Service",
    description="CRUD operations with Azure AD auth and RBAC.",
    version="1.0.0",
    enable_azure_auth=True,
    enable_docs_auth=False,
    enable_cors=True,
    enable_postgres=True,
    enable_redis=True,
    enable_blob_storage=True,
)

# Include routers
app.include_router(project_router)

logger.info("Project Management Service ready")

if __name__ == "__main__":
    settings = get_app_settings()
    uvicorn.run(app, host="0.0.0.0", port=settings.API_PORT)
