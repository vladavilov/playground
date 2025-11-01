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
from sqlalchemy import inspect
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
    Initializes the database by dropping and recreating all tables defined in the imported models.
    Creates 'projects' and 'project_members' tables from the shared models with latest schema.
    This operation drops existing tables to ensure schema matches the current model definitions.
    
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
        
        # Drop existing tables to ensure schema matches current models
        inspector = inspect(postgres_client.sync_engine)
        existing_tables = inspector.get_table_names()
        tables_to_drop = [table for table in table_names if table in existing_tables]
        
        if tables_to_drop:
            logger.info("Dropping existing tables to recreate with latest schema", tables=tables_to_drop)
            Base.metadata.drop_all(bind=postgres_client.sync_engine)
        
        # Create all tables with latest schema
        logger.info("Creating database tables", tables=table_names)
        Base.metadata.create_all(bind=postgres_client.sync_engine)

        logger.info("Database initialization successful", tables_created=table_names)
        return {
            "status": "Database initialized successfully", 
            "tables_created": table_names,
            "tables_dropped": tables_to_drop if tables_to_drop else []
        }
    except Exception as e:
        handler = ErrorHandler()
        return handler.format_generic_error(e)

app.include_router(db_router)

if __name__ == "__main__":
    settings = get_app_settings()
    uvicorn.run(app, host="0.0.0.0", port=settings.API_PORT)
