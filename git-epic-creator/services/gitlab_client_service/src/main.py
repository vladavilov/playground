import structlog
import uvicorn
from contextlib import asynccontextmanager
from fastapi import FastAPI
from authlib.integrations.starlette_client import OAuth
from starlette.middleware.sessions import SessionMiddleware

from configuration.logging_config import configure_logging
from configuration.common_config import get_app_settings
from utils.app_factory import FastAPIFactory
from config import get_gitlab_client_settings

from routers.gitlab_router import router as gitlab_router
from routers.gitlab_auth_router import router as gitlab_auth_router
from services.gitlab_token_manager import create_update_token_callback
from utils.redis_client import get_redis_client


configure_logging()
logger = structlog.get_logger(__name__)


@asynccontextmanager
async def gitlab_lifespan(app: FastAPI):
    """GitLab-specific startup/shutdown logic."""
    # Startup: Configure GitLab OAuth
    redis_client = get_redis_client()
    
    # Create update_token callback for automatic token refresh
    update_token = create_update_token_callback(redis_client)
    
    # Initialize OAuth with update_token callback
    oauth = OAuth(update_token=update_token)
    settings = get_gitlab_client_settings()
    
    if settings.GITLAB_OAUTH_CLIENT_ID and settings.GITLAB_OAUTH_CLIENT_SECRET:
        oauth.register(
            name="gitlab",
            client_id=settings.GITLAB_OAUTH_CLIENT_ID,
            client_secret=settings.GITLAB_OAUTH_CLIENT_SECRET,
            access_token_url=f"{settings.GITLAB_BASE_URL}/oauth/token",
            access_token_params=None,
            authorize_url=f"{settings.GITLAB_BASE_URL}/oauth/authorize",
            authorize_params=None,
            api_base_url=f"{settings.GITLAB_BASE_URL}/api/v4/",
            client_kwargs={
                'scope': settings.GITLAB_OAUTH_SCOPES,
            }
        )
        app.state.oauth = oauth
        app.state.gitlab_settings = settings
        logger.info(
            "GitLab OAuth configured with automatic token refresh",
            client_id=settings.GITLAB_OAUTH_CLIENT_ID[:10] + "...",
            redirect_uri=settings.GITLAB_OAUTH_REDIRECT_URI
        )
    else:
        logger.warning("GitLab OAuth not configured - missing CLIENT_ID or CLIENT_SECRET")
    
    yield
    
    # Shutdown: cleanup (if needed)
    logger.info("GitLab Client Service shutting down")


app: FastAPI = FastAPIFactory.create_app(
    title="GitLab Client Service",
    description="Thin REST facade over GitLab with normalized models",
    version="1.0.0",
    enable_cors=True,
    enable_redis=True,
    custom_lifespan=gitlab_lifespan
)

# Configure SessionMiddleware (required by Authlib OAuth flow)
settings = get_gitlab_client_settings()
session_secret = (settings.SESSION_SECRET_KEY or "").strip()
if not session_secret:
    if settings.ALLOW_INSECURE_SESSION:
        import secrets
        session_secret = secrets.token_urlsafe(48)
        logger.warning("SESSION_SECRET_KEY not set; generated ephemeral dev secret")
    else:
        raise RuntimeError("SESSION_SECRET_KEY must be set for OAuth to work")

app.add_middleware(
    SessionMiddleware,
    secret_key=session_secret,
    max_age=3600,  # 1 hour - OAuth flow is short-lived
    same_site="lax",
    https_only=not settings.ALLOW_INSECURE_SESSION,
)
logger.info("SessionMiddleware configured for OAuth flow")

app.include_router(gitlab_router)
app.include_router(gitlab_auth_router)

logger.info("GitLab Client Service ready")

if __name__ == "__main__":
    settings = get_app_settings()
    uvicorn.run(app, host="0.0.0.0", port=settings.API_PORT)

