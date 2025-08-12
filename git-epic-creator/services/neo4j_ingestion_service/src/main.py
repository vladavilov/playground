import uvicorn
import structlog
from fastapi import APIRouter

from configuration.logging_config import configure_logging
from utils.app_factory import FastAPIFactory

# Configure logging at application startup
configure_logging()
logger = structlog.get_logger(__name__)

app = FastAPIFactory.create_app(
    title="Neo4j Graph Ingestion Service",
    description="A microservice to ingest the Neo4j Graph with the data.",
    version="1.0.0",
    enable_azure_auth=False,
    enable_docs_auth=False,
    enable_cors=True,
    enable_neo4j=True,
)

router = APIRouter(prefix="/health", tags=["Health"])

@router.get("/celery")
def celery_health():
    return {"healthy": True, "service": "neo4j_ingestion_service"}

app.include_router(router)

if __name__ == "__main__":
    uvicorn.run(app, host="0.0.0.0", port=8000)
