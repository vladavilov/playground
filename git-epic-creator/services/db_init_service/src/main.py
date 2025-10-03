"""
Database initialization service for PostgreSQL.
Uses shared library components for FastAPI app creation and PostgreSQL operations.
"""
from configuration.common_config import get_app_settings
from configuration.logging_config import configure_logging
from utils.postgres_client import PostgresClient, get_postgres_client
from utils.app_factory import FastAPIFactory
from models.project_db import Base, Project, ProjectMember

from fastapi import Depends, APIRouter
from utils.local_auth import get_local_user_verified, LocalUser
from utils.error_handler import ErrorHandler
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
    enable_cors=True,         # Enable CORS for API access
    enable_postgres=True  # Enable PostgreSQL integration
)

# Ensure global error handlers are registered (FastAPIFactory already does this,
# but keep explicit in case of direct app creation in tests)
ErrorHandler().register_exception_handlers(app)

# Create API router for database operations
db_router = APIRouter(prefix="/db", tags=["Database"])

@db_router.post("/init")
def init_db(
    postgres_client: PostgresClient = Depends(get_postgres_client),
    current_user: LocalUser = Depends(get_local_user_verified),
):
    """
    Initializes the database by creating all tables defined in the imported models.
    Creates 'projects' and 'project_members' tables from the shared models.
    This is an idempotent operation; it will not recreate existing tables.
    
    Uses the shared library's PostgreSQL client for database operations.
    
    Args:
        postgres_client: PostgreSQL client from dependency injection
        
    Returns:
        dict: Initialization result with list of tables created
    """
    client_settings = postgres_client.settings

    logger.info(
        "Database initialization requested",
        host=client_settings.POSTGRES_HOST,
        port=client_settings.POSTGRES_PORT,
        db=client_settings.POSTGRES_DB
    )

    try:
        models = [Project, ProjectMember]  # This registers the models with Base.metadata
        logger.info("Registered models", model_count=len(models))
        
        # Get list of tables that will be created
        table_names = [table.name for table in Base.metadata.tables.values()]
        logger.info("Creating database tables", tables=table_names)
        
        # Use the synchronous engine to create all tables
        Base.metadata.create_all(bind=postgres_client.sync_engine)

        logger.info("Database initialization successful", tables_created=table_names)
        return {
            "status": "Database initialized successfully", 
            "tables_created": table_names
        }
    except Exception as e:
        handler = ErrorHandler()
        return handler.format_generic_error(e)

app.include_router(db_router)

if __name__ == "__main__":
    settings = get_app_settings()
    uvicorn.run(app, host="0.0.0.0", port=settings.API_PORT)
