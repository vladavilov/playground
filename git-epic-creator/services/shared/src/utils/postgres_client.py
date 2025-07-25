"""
PostgreSQL database client utilities following SOLID principles.
Separates client creation from configuration.
"""

from functools import lru_cache
import structlog
from sqlalchemy import create_engine, MetaData, text
from sqlalchemy.ext.asyncio import create_async_engine, AsyncSession, async_sessionmaker
from sqlalchemy.orm import sessionmaker, Session
from configuration.postgres_config import PostgresSettings
from configuration.common_config import get_app_settings

logger = structlog.get_logger(__name__)

class PostgresClientFactory:
    """
    Factory class for creating PostgreSQL clients.
    Follows the Factory pattern and Single Responsibility Principle.
    """
    
    @staticmethod
    def create_sync_engine(settings: PostgresSettings):
        """
        Create a synchronous SQLAlchemy engine from settings.
        
        Args:
            settings: PostgreSQL configuration settings
            
        Returns:
            Engine: Configured SQLAlchemy engine
        """
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
        """
        Create an asynchronous SQLAlchemy engine from settings.
        
        Args:
            settings: PostgreSQL configuration settings
            
        Returns:
            AsyncEngine: Configured SQLAlchemy async engine
        """
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
        """
        Create a synchronous session factory from an engine.
        
        Args:
            engine: SQLAlchemy engine
            
        Returns:
            sessionmaker: Session factory
        """
        return sessionmaker(
            autocommit=False,
            autoflush=False,
            bind=engine
        )
    
    @staticmethod
    def create_async_session_factory(engine):
        """
        Create an asynchronous session factory from an engine.
        
        Args:
            engine: SQLAlchemy async engine
            
        Returns:
            async_sessionmaker: Async session factory
        """
        return async_sessionmaker(
            autocommit=False,
            autoflush=False,
            bind=engine,
            expire_on_commit=False,
            class_=AsyncSession
        )

class PostgresClient:
    """
    PostgreSQL client for database operations.
    Provides access to session factories and metadata.
    """
    
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
        self.async_session_factory = PostgresClientFactory.create_async_session_factory(self.async_engine)
        self.metadata = MetaData(schema=self.settings.POSTGRES_SCHEMA)
        
        logger.info("PostgreSQL client initialized")
    
    def get_sync_session(self) -> Session:
        """
        Get a synchronous database session.
        
        Returns:
            Session: SQLAlchemy session
        """
        return self.sync_session_factory()
    
    def get_async_session(self):
        """
        Get an asynchronous database session.
        For test compatibility, this returns a session that can be mocked.
        
        Returns:
            AsyncSession: SQLAlchemy async session
        """
        return self.async_session_factory()

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
    """
    Health checker for PostgreSQL connections.
    Follows Single Responsibility Principle.
    """
    
    @staticmethod
    def check_health(client: PostgresClient) -> bool:
        """
        Check PostgreSQL connection health.
        
        Args:
            client: PostgreSQL client instance
            
        Returns:
            bool: True if PostgreSQL is healthy, False otherwise
        """
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
        """
        Check PostgreSQL connection health with detailed information.
        
        Args:
            client: PostgreSQL client instance
            
        Returns:
            dict: Health check results with details
        """
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

# Convenience function for backward compatibility
def check_postgres_health(client: PostgresClient) -> bool:
    """
    Check PostgreSQL connection health.
    Convenience wrapper for PostgresHealthChecker.
    
    Args:
        client: PostgreSQL client instance
        
    Returns:
        bool: True if PostgreSQL is healthy, False otherwise
    """
    return PostgresHealthChecker.check_health(client)