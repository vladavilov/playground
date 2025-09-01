"""GraphRAG retrieval Service FastAPI application.
"""

import structlog
from fastapi import FastAPI
from utils.app_factory import FastAPIFactory
from routers.retrieval_router import retrieval_router

from configuration.logging_config import configure_logging
from configuration.common_config import get_app_settings


# Initialize logging before creating the app
configure_logging()
logger = structlog.get_logger(__name__)


app: FastAPI = FastAPIFactory.create_app(
    title="GraphRAG retrieval Service",
    description="GraphRAG retrieval service, providing enhanced context for ai workflow service",
    version="1.0.0",
    enable_azure_auth=False,
    enable_docs_auth=False,
    enable_cors=True,
    enable_postgres=False,
    enable_neo4j=False,
    enable_redis=False,
)

app.include_router(retrieval_router, prefix="/retrieve", tags=["Retrieval"])

if __name__ == "__main__":
    settings = get_app_settings()
    import uvicorn

    uvicorn.run(app, host="0.0.0.0", port=settings.API_PORT)


