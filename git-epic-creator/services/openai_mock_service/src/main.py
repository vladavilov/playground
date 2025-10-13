import os
import structlog
import uvicorn
from fastapi import FastAPI
from fastapi.responses import ORJSONResponse

from configuration.logging_config import configure_logging
from configuration.common_config import get_app_settings
from utils.app_factory import FastAPIFactory
from config import get_config
from routers import health, models, chat, embeddings, azure


# Ensure Hugging Face cache dir uses HF_HOME to avoid deprecation warning
if os.getenv("HF_HOME") is None:
    os.environ.setdefault("HF_HOME", os.getenv("TRANSFORMERS_CACHE", "/models/hf-cache"))


# Configure shared logging
configure_logging()
logger = structlog.get_logger(__name__)


# Create app via shared factory (uses shared ErrorHandler and lifespan)
app: FastAPI = FastAPIFactory.create_app(
    title="OpenAI Mock Service",
    description="Mock of OpenAI endpoints for local development and tests",
    version="1.0.0",
    enable_cors=True,
)

# Default ORJSON response class for performance
app.default_response_class = ORJSONResponse

# Include routers
app.include_router(health.router)
app.include_router(models.router)
app.include_router(chat.router)
app.include_router(embeddings.router)
app.include_router(azure.router)


if __name__ == "__main__":
    # Prefer shared app settings, fallback to local config for compatibility
    try:
        settings = get_app_settings()
        port = int(settings.API_PORT)
    except Exception:
        cfg = get_config()
        port = int(cfg["API_PORT"])  # type: ignore[index]
    uvicorn.run(app, host="0.0.0.0", port=port)

