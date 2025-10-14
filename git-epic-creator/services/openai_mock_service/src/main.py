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
from embeddings.service import EmbeddingService


# Force offline mode and consistent cache directories before any model code runs
os.environ.setdefault("HF_HOME", os.getenv("TRANSFORMERS_CACHE", "/models/hf-cache"))
os.environ.setdefault("TRANSFORMERS_CACHE", os.environ.get("HF_HOME", "/models/hf-cache"))
os.environ.setdefault("HF_HUB_OFFLINE", "1")
os.environ.setdefault("TRANSFORMERS_OFFLINE", "1")


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


# Warm up embeddings model at startup to ensure quick first request
@app.on_event("startup")
async def _warmup_embeddings() -> None:
    try:
        service = EmbeddingService()
        _ = service.embed_texts(["warmup"])  # single short input, cached model init
        logger.info("embeddings_warmup_complete")
    except Exception as exc:
        # Do not crash app; embeddings endpoint will surface error if invoked
        logger.warning("embeddings_warmup_failed", error=str(exc))

if __name__ == "__main__":
    # Prefer shared app settings, fallback to local config for compatibility
    try:
        settings = get_app_settings()
        port = int(settings.API_PORT)
    except Exception:
        cfg = get_config()
        port = int(cfg["API_PORT"])  # type: ignore[index]
    uvicorn.run(app, host="0.0.0.0", port=port)

