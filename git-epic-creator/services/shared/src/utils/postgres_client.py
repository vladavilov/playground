"""PostgreSQL client utilities and health checks."""

from functools import lru_cache
from contextlib import contextmanager
from typing import Generator
import structlog
from sqlalchemy import create_engine, MetaData, text
from sqlalchemy.ext.asyncio import create_async_engine, AsyncSession, async_sessionmaker
from sqlalchemy.orm import sessionmaker, Session
from configuration.postgres_config import PostgresSettings
from configuration.common_config import get_app_settings

logger = structlog.get_logger(__name__)

class PostgresClientFactory:
    """Create sync/async SQLAlchemy engines and sessions."""
    
    @staticmethod
    def create_sync_engine(settings: PostgresSettings):
        """Create synchronous SQLAlchemy engine from settings."""
        engine = create_engine(
            settings.DATABASE_URL,
            pool_size=settings.POSTGRES_POOL_SIZE,
            max_overflow=settings.POSTGRES_MAX_OVERFLOW,
            pool_timeout=settings.POSTGRES_POOL_TIMEOUT,
            pool_recycle=settings.POSTGRES_POOL_RECYCLE,
            pool_pre_ping=True,
            echo=False
        )
        
        logger.info(
            "PostgreSQL sync engine created",
            host=settings.POSTGRES_HOST,
            port=settings.POSTGRES_PORT,
            db=settings.POSTGRES_DB,
            user=settings.POSTGRES_USER
        )
        return engine
    
    @staticmethod
    def create_async_engine(settings: PostgresSettings):
        """Create asynchronous SQLAlchemy engine from settings."""
        engine = create_async_engine(
            settings.ASYNC_DATABASE_URL,
            pool_size=settings.POSTGRES_POOL_SIZE,
            max_overflow=settings.POSTGRES_MAX_OVERFLOW,
            pool_timeout=settings.POSTGRES_POOL_TIMEOUT,
            pool_recycle=settings.POSTGRES_POOL_RECYCLE,
            pool_pre_ping=True,
            echo=False
        )
        
        logger.info(
            "PostgreSQL async engine created",
            host=settings.POSTGRES_HOST,
            port=settings.POSTGRES_PORT,
            db=settings.POSTGRES_DB,
            user=settings.POSTGRES_USER
        )
        return engine
    
    @staticmethod
    def create_sync_session_factory(engine):
        """Create synchronous session factory from engine."""
        return sessionmaker(
            autocommit=False,
            autoflush=False,
            bind=engine,
            expire_on_commit=False  # Keep objects accessible after commit
        )

class PostgresClient:
    """Provides sync/async sessions and metadata."""
    
    def __init__(self, settings: PostgresSettings):
        """
        Initialize PostgreSQL client.
        
        Args:
            settings: PostgreSQL configuration settings (optional)
        """
        self.settings = settings
        self.sync_engine = PostgresClientFactory.create_sync_engine(settings)
        self.async_engine = PostgresClientFactory.create_async_engine(settings)
        self.sync_session_factory = PostgresClientFactory.create_sync_session_factory(self.sync_engine)
        self.metadata = MetaData(schema=self.settings.POSTGRES_SCHEMA)
        
        logger.info("PostgreSQL client initialized")
    
    @contextmanager
    def get_sync_session(self) -> Generator[Session, None, None]:
        """
        Get a synchronous session with automatic transaction management.
        
        The session will automatically:
        - Commit on successful completion
        - Rollback on any exception
        - Close in all cases
        
        Example:
            with postgres_client.get_sync_session() as session:
                project = session.query(Project).filter(...).first()
                project.status = "active"
                # Commit happens automatically here
        
        Yields:
            Session: SQLAlchemy session with active transaction
        """
        session = self.sync_session_factory()
        try:
            yield session
            session.commit()
            logger.debug("Database transaction committed successfully")
        except Exception as e:
            session.rollback()
            logger.warning("Database transaction rolled back", error=str(e))
            raise
        finally:
            session.close()

@lru_cache()
def get_postgres_client() -> PostgresClient:
    """
    Get a cached PostgreSQL client instance.
    Uses dependency injection pattern with settings.
    
    Returns:
        PostgresClient: Configured PostgreSQL client
    """
    settings = get_app_settings()
    return PostgresClient(settings.postgres)

class PostgresHealthChecker:
    """Health checks for PostgreSQL connections."""
    
    @staticmethod
    def check_health(client: PostgresClient) -> bool:
        """Check PostgreSQL connection health."""
        try:
            with client.get_sync_session() as session:
                session.execute("SELECT 1")
            logger.info("PostgreSQL health check passed")
            return True
        except Exception as e:
            logger.error("PostgreSQL health check failed", error=str(e))
            return False
    
    @staticmethod
    def check_health_with_details(client: PostgresClient) -> dict:
        """Check PostgreSQL connection health with details."""
        try:
            # Test basic connectivity
            with client.get_sync_session() as session:
                result = session.execute("SELECT version()")
                version = result.scalar()
                
                result = session.execute("SELECT count(*) FROM pg_stat_activity")
                connections = result.scalar()
            
            result = {
                "healthy": True,
                "version": version,
                "connections": connections,
                "database": client.settings.POSTGRES_DB,
                "host": client.settings.POSTGRES_HOST,
                "port": client.settings.POSTGRES_PORT
            }
            
            logger.info("PostgreSQL detailed health check passed", **result)
            return result
            
        except Exception as e:
            result = {
                "healthy": False,
                "error": str(e),
                "database": client.settings.POSTGRES_DB,
                "host": client.settings.POSTGRES_HOST,
                "port": client.settings.POSTGRES_PORT
            }
            logger.error("PostgreSQL detailed health check failed", **result)
            return result
