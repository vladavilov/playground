"""Gateway control plane (SSE only)."""

from contextlib import asynccontextmanager

from fastapi import APIRouter, FastAPI
import structlog
from configuration.common_config import get_app_settings
from configuration.logging_config import configure_logging
from utils.app_factory import FastAPIFactory
from routers.sse_router import router as sse_router
from routers.sse_router import _broker as _sse_broker

configure_logging()
logger = structlog.get_logger(__name__)

app = FastAPIFactory.create_app(
    title="Gateway Control Plane",
    description="Redis Pub/Sub → SSE bridge for the frontend (served behind Envoy)",
    version="0.1.0",
    enable_cors=True,
    enable_postgres=False,
    enable_redis=True,
)

@asynccontextmanager
async def _ui_lifespan(_app: FastAPI):
    try:
        yield
    finally:
        try:
            await _sse_broker.stop()
        except Exception:
            pass

## Routers
app.include_router(sse_router)

# Register UI-specific lifespan without overriding app-level lifespan
app.include_router(APIRouter(lifespan=_ui_lifespan))

@app.get("/")
async def root():
    """Gateway-only service. Root should be handled by the reverse proxy/frontend."""
    return {"ok": True, "service": "gateway_control_plane_service", "mode": "sse-only"}

if __name__ == "__main__":
    settings = get_app_settings()
    import uvicorn
    uvicorn.run(app, host="0.0.0.0", port=settings.API_PORT)


