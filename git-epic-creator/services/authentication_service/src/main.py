"""Authentication Service FastAPI application.

Provides centralized authentication services:
- Azure AD login + session management (MSAL)
- Envoy `ext_authz` for policy enforcement + S2S token injection
- S2S token minting for internal callers that still use the `/auth/s2s/mint` contract
"""

import structlog
from fastapi import FastAPI
from starlette.middleware.sessions import SessionMiddleware
from utils.app_factory import FastAPIFactory

from configuration.logging_config import configure_logging
from configuration.common_config import get_app_settings
from routers.token_router import router as token_router
from routers.browser_auth_router import router as browser_auth_router
from routers.authz_router import router as authz_router
from session_config import get_session_settings

# Initialize logging before creating the app
configure_logging()
logger = structlog.get_logger(__name__)

app: FastAPI = FastAPIFactory.create_app(
    title="Authentication Service",
    description="Centralized auth (MSAL sessions, ext_authz) and S2S JWT minting for microservices",
    version="1.0.0",
    enable_cors=True,
    enable_postgres=False,
    enable_neo4j=False,
    enable_redis=False,
)

session_settings = get_session_settings()
session_secret = (session_settings.SESSION_SECRET_KEY or "").strip()
if not session_secret:
    if session_settings.ALLOW_INSECURE_SESSION:
        import secrets

        session_secret = secrets.token_urlsafe(48)
        logger.warning("SESSION_SECRET_KEY not set; generated ephemeral dev secret")
    else:
        raise RuntimeError("SESSION_SECRET_KEY must be set for secure sessions")

app.add_middleware(
    SessionMiddleware,
    secret_key=session_secret,
    session_cookie=session_settings.SESSION_COOKIE_NAME,
    max_age=int(session_settings.SESSION_MAX_AGE),
    same_site=session_settings.SESSION_SAME_SITE,
    https_only=not session_settings.ALLOW_INSECURE_SESSION,
)

app.include_router(browser_auth_router)
app.include_router(authz_router)
app.include_router(token_router, prefix="/auth", tags=["Authentication"])

if __name__ == "__main__":
    settings = get_app_settings()
    import uvicorn

    uvicorn.run(app, host="0.0.0.0", port=settings.API_PORT)
