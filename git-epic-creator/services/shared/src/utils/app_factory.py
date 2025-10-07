"""FastAPI application factory for standardized apps."""

from contextlib import asynccontextmanager
import structlog
from fastapi import FastAPI, Depends, Request
from fastapi.middleware.cors import CORSMiddleware
from utils.postgres_client import get_postgres_client, PostgresHealthChecker, PostgresClient
from utils.neo4j_client import get_neo4j_client, Neo4jHealthChecker, Neo4jClient
from utils.redis_client import get_redis_client
from utils.blob_storage import get_blob_storage_client, BlobStorageClient
from utils.error_handler import ErrorHandler
import redis.asyncio as redis

logger = structlog.get_logger(__name__)

class RedisHealthService:
    """Helper for Redis health checking."""

    def __init__(self, redis_client):
        self.redis_client = redis_client

    async def check_health_with_details(self) -> dict:
        """Check Redis health and return detailed information."""
        try:
            ping_ok = await self.redis_client.ping()
            if not ping_ok:
                return {"healthy": False, "error": "Redis ping failed"}

            info = await self.redis_client.info()
            result = {
                "healthy": True,
                "version": info.get("redis_version"),
            }

            # Optional useful diagnostics
            for key in ("connected_clients", "used_memory", "uptime_in_seconds"):
                if key in info:
                    result[key] = info[key]

            return result
        except Exception as e:
            return {"healthy": False, "error": str(e)}


def get_postgres_client_from_state(request: Request) -> PostgresClient:
    """Get PostgreSQL client from app state."""
    return request.app.state.postgres_client

def get_neo4j_client_from_state(request: Request) -> Neo4jClient:
    """Get Neo4j client from app state."""
    return request.app.state.neo4j_client

def get_redis_client_from_state(request: Request) -> redis.Redis:
    """Get Redis client from app state."""
    return request.app.state.redis_client

def get_blob_storage_client_from_state(request: Request) -> BlobStorageClient:
    """Get Blob Storage client from app state."""
    return request.app.state.blob_storage_client

# endregion

class FastAPIFactory:
    """Create FastAPI apps with shared configuration."""

    @staticmethod
    def create_app(
        title: str,
        description: str,
        version: str,
        enable_cors: bool = True,
        enable_postgres: bool = False,
        enable_neo4j: bool = False,
        enable_redis: bool = False,
        enable_blob_storage: bool = False,
        openapi_url: str = "/openapi.json",
        docs_url: str = "/docs",
        redoc_url: str = "/redoc"
    ) -> FastAPI:
        """Create a FastAPI application with standard configuration."""
        postgres_client: PostgresClient = get_postgres_client() if enable_postgres else None
        neo4j_client: Neo4jClient = get_neo4j_client() if enable_neo4j else None
        redis_client: redis.Redis = get_redis_client() if enable_redis else None
        blob_storage_client: BlobStorageClient = get_blob_storage_client() if enable_blob_storage else None

        # Create lifespan context manager
        @asynccontextmanager
        async def lifespan(app: FastAPI):
            yield

            # Shutdown logic
            if hasattr(app.state, 'neo4j_client'):
                try:
                    app.state.neo4j_client.close()
                    logger.info("Neo4j client closed on shutdown")
                except Exception as e:
                    logger.warning("Failed to close Neo4j client on shutdown", error=str(e))
            if hasattr(app.state, 'redis_client'):
                await app.state.redis_client.close()
                logger.info("Redis client closed on shutdown")
            logger.info("Application shutting down")

        app = FastAPI(
            title=title,
            description=description,
            version=version,
            openapi_url=openapi_url,
            docs_url=docs_url,
            redoc_url=redoc_url,
            lifespan=lifespan
        )

        # Register global exception handlers for consistent error responses
        ErrorHandler().register_exception_handlers(app)

        # Attach clients to app.state immediately after app creation
        # This ensures they're available for dependency injection and testing
        if postgres_client:
            app.state.postgres_client = postgres_client
            logger.info("PostgreSQL client attached to app.state")

        if neo4j_client:
            app.state.neo4j_client = neo4j_client
            logger.info("Neo4j client attached to app.state")

        if redis_client:
            app.state.redis_client = redis_client
            logger.info("Redis client attached to app.state")

        if blob_storage_client:
            app.state.blob_storage_client = blob_storage_client
            logger.info("Blob storage client attached to app.state")


        # Configure CORS if enabled (allow all by default or restrict via proxy)
        if enable_cors:
            app.add_middleware(
                CORSMiddleware,
                allow_origins=["*"],
                allow_credentials=True,
                allow_methods=["*"],
                allow_headers=["*"],
            )

        # Add health check endpoint
        @app.get("/health", tags=["Health"])
        def health_check():
            """
            Health check endpoint.
            
            Returns:
                dict: Health check result
            """
            return {"status": "ok"}


        if enable_postgres:
            @app.get("/health/postgres", tags=["Health"])
            def postgres_health_check(
                client: PostgresClient = Depends(get_postgres_client_from_state)
            ):
                """
                PostgreSQL health check endpoint.
                
                Returns:
                    dict: PostgreSQL health check result
                """
                return PostgresHealthChecker.check_health_with_details(client)

        if enable_neo4j:
            @app.get("/health/neo4j", tags=["Health"])
            def neo4j_health_check(
                client: Neo4jClient = Depends(get_neo4j_client_from_state)
            ):
                """
                Neo4j health check endpoint.
                
                Returns:
                    dict: Neo4j health check result
                """
                return Neo4jHealthChecker.check_health_with_details(client)

        if enable_redis:
            @app.get("/health/redis", tags=["Health"])
            async def redis_health_check(
                client: redis.Redis = Depends(get_redis_client_from_state)
            ):
                """
                Redis health check endpoint.
                
                Returns:
                    dict: Redis health check result
                """
                health_service = RedisHealthService(client)
                return await health_service.check_health_with_details()

        if enable_blob_storage:
            @app.get("/health/blob-storage", tags=["Health"])
            def blob_storage_health_check(
                client: BlobStorageClient = Depends(get_blob_storage_client_from_state)
            ):
                """
                Blob storage health check endpoint.
                
                Returns:
                    dict: Blob storage health check result
                """
                return {"status": "ok", "service": "blob_storage"}

        logger.info(
            "FastAPI application created",
            title=title,
            version=version,
            cors=enable_cors,
            postgres=enable_postgres,
            neo4j=enable_neo4j,
            redis=enable_redis,
            blob_storage=enable_blob_storage,
        )

        return app
