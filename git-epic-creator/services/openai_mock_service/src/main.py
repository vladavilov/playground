import os
from fastapi import FastAPI, Request
from fastapi.responses import ORJSONResponse
from fastapi.exceptions import RequestValidationError
import structlog
import uvicorn

from config import get_config
from routers import health, models, chat, embeddings, azure


# Ensure Hugging Face cache dir uses HF_HOME to avoid deprecation warning
if os.getenv("HF_HOME") is None:
    os.environ.setdefault("HF_HOME", os.getenv("TRANSFORMERS_CACHE", "/models/hf-cache"))


def configure_logging() -> None:
    """Minimal structlog setup compatible with other services."""
    structlog.configure(
        processors=[
            structlog.processors.add_log_level,
            structlog.processors.TimeStamper(fmt="iso"),
            structlog.processors.JSONRenderer(),
        ],
    )


configure_logging()
logger = structlog.get_logger(__name__)

app = FastAPI(default_response_class=ORJSONResponse)


@app.exception_handler(RequestValidationError)
async def request_validation_exception_handler(
    request: Request, exc: RequestValidationError
) -> ORJSONResponse:
    return ORJSONResponse(status_code=400, content={"detail": "Bad Request"})


# Include routers
app.include_router(health.router)
app.include_router(models.router)
app.include_router(chat.router)
app.include_router(embeddings.router)
app.include_router(azure.router)


if __name__ == "__main__":
    cfg = get_config()
    uvicorn.run(app, host="0.0.0.0", port=int(cfg["API_PORT"]))

