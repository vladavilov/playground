import structlog
import uvicorn
from fastapi import FastAPI

from configuration.logging_config import configure_logging
from configuration.common_config import get_app_settings
from utils.app_factory import FastAPIFactory

from routers.gitlab_router import router as gitlab_router


configure_logging()
logger = structlog.get_logger(__name__)

app: FastAPI = FastAPIFactory.create_app(
    title="GitLab Client Service",
    description="Thin REST facade over GitLab with normalized models",
    version="1.0.0",
    enable_cors=True,
    enable_redis=True
)

app.include_router(gitlab_router)

logger.info("GitLab Client Service ready")

if __name__ == "__main__":
    settings = get_app_settings()
    uvicorn.run(app, host="0.0.0.0", port=settings.API_PORT)

