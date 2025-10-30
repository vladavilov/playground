"""FastAPI app serving static UI and SSE bridge to Redis pubsub."""

from fastapi.staticfiles import StaticFiles
from fastapi import APIRouter, FastAPI
from fastapi.responses import RedirectResponse
from contextlib import asynccontextmanager
import os
import structlog
import httpx
import logging
from starlette.middleware.sessions import SessionMiddleware
from configuration.common_config import get_app_settings
from configuration.azure_auth_config import get_azure_auth_settings
from ui_config import get_ui_settings
from configuration.logging_config import configure_logging
from utils.app_factory import FastAPIFactory
from routers.sse_router import router as sse_router
from routers.auth_router import router as auth_router
from routers.proxy_router import router as proxy_router

configure_logging()
logger = structlog.get_logger(__name__)

# Log mock mode status
mock_mode = os.getenv("MOCK_AI_SERVICES", "").lower() in ("true", "1", "yes")
if mock_mode:
    logger.warning("MOCK_AI_SERVICES enabled - using mock data for ai-requirements and ai-tasks services")
else:
    logger.info("MOCK_AI_SERVICES disabled - using real ai-requirements and ai-tasks services")

app = FastAPIFactory.create_app(
    title="UI Service",
    description="Serves static UI and SSE for project progress",
    version="0.1.0",
    enable_cors=True,
    enable_postgres=False,
    enable_redis=True,
)

ui_settings = get_ui_settings()
session_secret = (ui_settings.SESSION_SECRET_KEY or "").strip()
if not session_secret:
    if ui_settings.ALLOW_INSECURE_SESSION:
        import secrets
        session_secret = secrets.token_urlsafe(48)
        logger.warning("SESSION_SECRET_KEY not set; generated ephemeral dev secret")
    else:
        raise RuntimeError("SESSION_SECRET_KEY must be set for secure sessions")

app.add_middleware(
    SessionMiddleware,
    secret_key=session_secret,
    session_cookie=ui_settings.SESSION_COOKIE_NAME,
    max_age=int(ui_settings.SESSION_MAX_AGE),
    same_site=ui_settings.SESSION_SAME_SITE,
    https_only=not ui_settings.ALLOW_INSECURE_SESSION,
)

async def _startup_configure_msal_logging() -> None:
    """Configure MSAL logging integration."""
    try:
        # Configure MSAL logger
        msal_logger = logging.getLogger("msal")
        
        # Set appropriate log level
        log_level = os.getenv("MSAL_LOG_LEVEL", "INFO")
        msal_logger.setLevel(getattr(logging, log_level.upper(), logging.INFO))
        
        # Integrate with structlog
        handler = logging.StreamHandler()
        handler.setFormatter(logging.Formatter(
            "%(asctime)s - %(name)s - %(levelname)s - %(message)s"
        ))
        msal_logger.addHandler(handler)
        
        logger.info("MSAL logging configured", log_level=log_level)
    except Exception as e:
        logger.warning("Failed to configure MSAL logging", error=str(e))


async def _startup_store_azure_settings(_azure) -> None:
    """Store Azure auth settings on app state for MSAL."""
    app.state.azure_auth_settings = _azure
    logger.info("Azure auth settings stored on app.state")


async def _startup_init_upstream_http_client(http_settings) -> None:
    timeout = httpx.Timeout(
        connect=http_settings.CONNECTION_TIMEOUT,
        read=http_settings.READ_TIMEOUT,
        write=http_settings.CONNECTION_TIMEOUT,
        pool=http_settings.CONNECTION_TIMEOUT,
    )
    limits = httpx.Limits(
        max_connections=http_settings.MAX_CONNECTIONS,
        max_keepalive_connections=http_settings.MAX_KEEPALIVE_CONNECTIONS,
    )
    app.state.upstream_http_client = httpx.AsyncClient(timeout=timeout, limits=limits)
    logger.info("Upstream HTTP client initialized")

@asynccontextmanager
async def _ui_lifespan(_app: FastAPI):
    ui = ui_settings
    try:
        azure = get_azure_auth_settings()
    except Exception as e:
        azure = None
        logger.warning("Failed to read Azure auth settings", error=str(e))

    try:
        http_settings = get_app_settings().http_client
    except Exception as e:
        http_settings = None
        logger.warning("Failed to read HTTP client settings", error=str(e))

    steps = [
        ("configure_msal_logging", lambda: _startup_configure_msal_logging()),
        ("store_azure_settings", (lambda: _startup_store_azure_settings(azure)) if azure else None),
        ("init_upstream_http_client", (lambda: _startup_init_upstream_http_client(http_settings)) if http_settings else None),
    ]

    for name, step in steps:
        if step is None:
            continue
        try:
            await step()
        except Exception as e:
            logger.warning(f"Startup step failed: {name}", error=str(e))

    try:
        yield
    finally:
        try:
            client = getattr(app.state, "upstream_http_client", None)
            if client is not None:
                await client.aclose()
                app.state.upstream_http_client = None
                logger.info("Upstream HTTP client closed")
        except Exception:
            pass

## Routers
app.include_router(sse_router)
app.include_router(auth_router)
app.include_router(proxy_router)

# Register UI-specific lifespan without overriding app-level lifespan
app.include_router(APIRouter(lifespan=_ui_lifespan))

# Redirect root to default page
@app.get("/")
async def root():
    """Redirect root to the projects page."""
    return RedirectResponse(url="/pages/projects.html", status_code=302)

static_dir = os.path.join(os.path.dirname(__file__), "static")
app.mount("/", StaticFiles(directory=static_dir, html=True), name="static")

if __name__ == "__main__":
    settings = get_app_settings()
    import uvicorn
    uvicorn.run(app, host="0.0.0.0", port=settings.API_PORT)


