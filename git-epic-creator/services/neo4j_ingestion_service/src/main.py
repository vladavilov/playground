import uvicorn
from fastapi import APIRouter, Depends
from fastapi.responses import JSONResponse
import structlog

from configuration.logging_config import configure_logging
from utils.neo4j_client import Neo4jClient
from utils.app_factory import FastAPIFactory, get_neo4j_client_from_state

from services.ingestion_service import Neo4jIngestionService

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
    enable_neo4j=True
)

neo4j_router = APIRouter()

def get_ingestion_service(
        client: Neo4jClient = Depends(get_neo4j_client_from_state)
    ) -> Neo4jIngestionService:

    """Get Neo4j ingestion service instance."""

    return Neo4jIngestionService(client)

@neo4j_router.post("/ingest")
async def init_neo4j(
    ingestion_service: Neo4jIngestionService = Depends(get_ingestion_service)
) -> JSONResponse:
    pass

if __name__ == "__main__":
    uvicorn.run(app, host="0.0.0.0", port=8000)
