"""Authentication Service FastAPI application.

Provides centralized authentication services:
- Token exchange (Azure AD OAuth tokens → LOCAL JWT)
- S2S token minting for service-to-service authentication
"""

import structlog
from fastapi import FastAPI
from utils.app_factory import FastAPIFactory

from configuration.logging_config import configure_logging
from configuration.common_config import get_app_settings
from routers.token_router import router as token_router

# Initialize logging before creating the app
configure_logging()
logger = structlog.get_logger(__name__)

app: FastAPI = FastAPIFactory.create_app(
    title="Authentication Service",
    description="Centralized token exchange and S2S JWT minting for microservices",
    version="1.0.0",
    enable_cors=True,
    enable_postgres=False,
    enable_neo4j=False,
    enable_redis=False,
)

app.include_router(token_router, prefix="/auth", tags=["Authentication"])

if __name__ == "__main__":
    settings = get_app_settings()
    import uvicorn

    uvicorn.run(app, host="0.0.0.0", port=settings.API_PORT)
