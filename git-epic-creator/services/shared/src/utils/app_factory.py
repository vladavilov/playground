"""
FastAPI application factory for creating standardized FastAPI applications.
"""

from contextlib import asynccontextmanager
import structlog
from fastapi import FastAPI, Depends, Request
from fastapi.middleware.cors import CORSMiddleware
from middleware.azure_auth_middleware import AzureAuthMiddleware, get_current_user, create_azure_scheme, set_azure_scheme
from configuration.azure_auth_config import get_azure_auth_settings
from utils.postgres_client import get_postgres_client, PostgresHealthChecker, PostgresClient
from utils.neo4j_client import get_neo4j_client, Neo4jHealthChecker, Neo4jClient
from utils.redis_client import get_redis_client
from utils.redis_abstractions import RedisHealthMixin
from utils.blob_storage import get_blob_storage_client, BlobStorageClient
import redis.asyncio as redis

logger = structlog.get_logger(__name__)

# region Dependencies for dependency injection


class RedisHealthService(RedisHealthMixin):
    """Helper class for Redis health checking using RedisHealthMixin."""
    
    def __init__(self, redis_client):
        self.redis_client = redis_client


def get_postgres_client_from_state(request: Request) -> PostgresClient:
    """Dependency to get PostgreSQL client from application state."""
    return request.app.state.postgres_client

def get_neo4j_client_from_state(request: Request) -> Neo4jClient:
    """Dependency to get Neo4j client from application state."""
    return request.app.state.neo4j_client

def get_redis_client_from_state(request: Request) -> redis.Redis:
    """Dependency to get Redis client from application state."""
    return request.app.state.redis_client

def get_blob_storage_client_from_state(request: Request) -> BlobStorageClient:
    """Dependency to get Blob Storage client from application state."""
    return request.app.state.blob_storage_client

# endregion

class FastAPIFactory:
    """
    Factory class for creating FastAPI applications.
    Follows the Factory pattern and Single Responsibility Principle.
    """

    @staticmethod
    def create_app(
        title: str,
        description: str,
        version: str,
        enable_azure_auth: bool = True,
        enable_docs_auth: bool = True,
        enable_cors: bool = True,
        enable_postgres: bool = False,
        enable_neo4j: bool = False,
        enable_redis: bool = False,
        enable_blob_storage: bool = False,
        openapi_url: str = "/openapi.json",
        docs_url: str = "/docs",
        redoc_url: str = "/redoc"
    ) -> FastAPI:
        """
        Create a FastAPI application with standard configuration.
        
        Args:
            title: Application title
            description: Application description
            version: Application version
            enable_azure_auth: Whether to enable Azure AD authentication
            enable_docs_auth: Whether to enable authentication for docs
            enable_cors: Whether to enable CORS
            enable_postgres: Whether to enable PostgreSQL integration
            enable_neo4j: Whether to enable Neo4j integration
            enable_redis: Whether to enable Redis integration
            enable_blob_storage: Whether to enable blob storage integration
            openapi_url: OpenAPI URL
            docs_url: Swagger UI URL
            redoc_url: ReDoc URL
            
        Returns:
            FastAPI: Configured FastAPI application
        """
        # Configure Azure AD authentication if enabled
        azure_middleware = None
        azure_settings = None
        postgres_client: PostgresClient = get_postgres_client() if enable_postgres else None
        neo4j_client: Neo4jClient = get_neo4j_client() if enable_neo4j else None
        redis_client: redis.Redis = get_redis_client() if enable_redis else None
        blob_storage_client: BlobStorageClient = get_blob_storage_client() if enable_blob_storage else None

        if enable_azure_auth:
            azure_settings = get_azure_auth_settings()

            # Configure Azure AD authentication scheme
            azure_scheme = create_azure_scheme(
                app_client_id=azure_settings.AZURE_CLIENT_ID,
                tenant_id=azure_settings.AZURE_TENANT_ID,
                scopes=azure_settings.SCOPES,
                openapi_authorization_url=azure_settings.OPENAPI_AUTHORIZATION_URL,
                openapi_token_url=azure_settings.OPENAPI_TOKEN_URL,
                openid_config_url=azure_settings.OPENID_CONFIG_URL
            )

            # Set the global azure scheme for dependency injection
            set_azure_scheme(azure_scheme)

            # Create Azure AD middleware
            azure_middleware = AzureAuthMiddleware(azure_scheme)

        # Create lifespan context manager
        @asynccontextmanager
        async def lifespan(app: FastAPI):
            # Startup logic
            if azure_middleware:
                try:
                    await azure_middleware.load_openid_config()
                    logger.info("OpenID configuration loaded on startup")
                except Exception as e:
                    logger.error(
                        "Failed to load OpenID configuration during startup. ",
                        error=str(e),
                        tenant_id=azure_settings.AZURE_TENANT_ID
                    )
                    raise
            yield

            # Shutdown logic
            if hasattr(app.state, 'neo4j_client'):
                app.state.neo4j_client.close()
                logger.info("Neo4j client closed on shutdown")
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

        # Configure Swagger UI authentication if enabled
        if enable_azure_auth and enable_docs_auth and azure_settings:
            app.swagger_ui_init_oauth = {
                "clientId": azure_settings.OPENAPI_CLIENT_ID,
                "appName": title,
                "usePkceWithAuthorizationCodeGrant": True,
                "scopes": list(azure_settings.SCOPES.keys())
            }

        # Configure CORS if enabled
        if enable_cors and azure_settings:
            app.add_middleware(
                CORSMiddleware,
                allow_origins=azure_settings.BACKEND_CORS_ORIGINS,
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

        # Add version endpoint
        @app.get("/version", tags=["Version"])
        def get_version():
            """
            Get application version.
            
            Returns:
                dict: Application version
            """
            return {"version": version}

        # Add user info endpoint if Azure AD authentication is enabled
        if enable_azure_auth:
            @app.get("/me", tags=["User"])
            async def get_user_info(current_user = Depends(get_current_user)):
                """
                Get current user information.
                
                Args:
                    current_user: Current authenticated user
                    
                Returns:
                    dict: User information
                """
                return {
                    "id": current_user.oid,
                    "username": current_user.preferred_username,
                    "roles": current_user.roles
                }

        # Add PostgreSQL health check endpoint if enabled
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

        # Add Neo4j health check endpoint if enabled
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

        # Add Redis health check endpoint if enabled
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

        # Add blob storage health check endpoint if enabled
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
            azure_auth=enable_azure_auth,
            docs_auth=enable_docs_auth,
            cors=enable_cors,
            postgres=enable_postgres,
            neo4j=enable_neo4j,
            redis=enable_redis,
            blob_storage=enable_blob_storage,
        )

        return app
