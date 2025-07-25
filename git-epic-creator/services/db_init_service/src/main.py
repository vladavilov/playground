"""
Database initialization service for PostgreSQL.
Uses shared library components for FastAPI app creation and PostgreSQL operations.
"""
from configuration.common_config import get_app_settings
from configuration.logging_config import configure_logging
from utils.postgres_client import PostgresClient, get_postgres_client
from utils.app_factory import FastAPIFactory
from models.base import Base

from fastapi import Depends, APIRouter
import structlog
import uvicorn

# Configure logging at application startup
configure_logging()
logger = structlog.get_logger(__name__)

# Create FastAPI application using the factory
app = FastAPIFactory.create_app(
    title="Database Initialization Service",
    description="Service for initializing and managing PostgreSQL database schema",
    version="1.0.0",
    enable_azure_auth=False,  # Disable Azure auth for this service
    enable_docs_auth=False,   # Disable docs auth for this service
    enable_cors=True,         # Enable CORS for API access
    enable_postgres=True      # Enable PostgreSQL integration
)

# Create API router for database operations
db_router = APIRouter(prefix="/db", tags=["Database"])

@db_router.post("/init")
def init_db(postgres_client: PostgresClient = Depends(get_postgres_client)):
    """
    Initializes the database by creating all tables defined in the models.
    This is an idempotent operation; it will not recreate existing tables.
    
    Uses the shared library's PostgreSQL client for database operations.
    
    Args:
        postgres_client: PostgreSQL client from dependency injection
        
    Returns:
        dict: Initialization result
    """
    settings = postgres_client.settings

    logger.info(
        "Database initialization requested",
        host=settings.POSTGRES_HOST,
        port=settings.POSTGRES_PORT,
        db=settings.POSTGRES_DB
    )

    try:
        # Use the synchronous engine to create all tables
        Base.metadata.create_all(bind=postgres_client.sync_engine)

        logger.info("Database initialization successful")
        return {"status": "Database initialized successfully"}
    except (ConnectionError, TimeoutError) as e:
        logger.error("Database connection failed during initialization", error=str(e), exc_info=True)
        return {"status": "error", "detail": f"Connection error: {str(e)}"}
    except ImportError as e:
        logger.error("Model import error during initialization", error=str(e), exc_info=True)
        return {"status": "error", "detail": f"Model import error: {str(e)}"}
    except Exception as e:  # pylint: disable=broad-except
        # Still keep a general exception handler as a last resort, but with more specific logging
        logger.error("Database initialization failed", error=str(e), exc_info=True)
        return {"status": "error", "detail": f"Unexpected error: {str(e)}"}

app.include_router(db_router)

if __name__ == "__main__":
    settings = get_app_settings()
    uvicorn.run(app, host="0.0.0.0", port=settings.API_PORT)
