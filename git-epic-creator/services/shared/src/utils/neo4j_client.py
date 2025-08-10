"""Neo4j client utilities and health checks."""

from functools import lru_cache
import time
import structlog
from neo4j import GraphDatabase, Driver, Session
from neo4j.exceptions import ServiceUnavailable, SessionExpired
from configuration.neo4j_config import Neo4jSettings
from configuration.common_config import get_app_settings

logger = structlog.get_logger(__name__)

class Neo4jClientFactory:
    """Create Neo4j driver from settings."""
    
    @staticmethod
    def create_driver(settings: Neo4jSettings) -> Driver:
        """Create a Neo4j driver from settings."""
        driver = GraphDatabase.driver(
            settings.NEO4J_URI,
            auth=(settings.NEO4J_USERNAME, settings.NEO4J_PASSWORD),
            max_connection_lifetime=settings.NEO4J_CONNECTION_TIMEOUT,
            max_connection_pool_size=settings.NEO4J_MAX_CONNECTION_POOL_SIZE,
            max_transaction_retry_time=settings.NEO4J_MAX_TRANSACTION_RETRY_TIME
        )
        
        logger.info("Neo4j driver created", uri=settings.NEO4J_URI)
        return driver

class Neo4jClient:
    """Client for interacting with a Neo4j database."""

    def __init__(self, settings: Neo4jSettings):
        self.settings = settings
        self._driver = Neo4jClientFactory.create_driver(settings)
        
        logger.info("Neo4j client initialized")

    def __enter__(self):
        return self

    def __exit__(self, exc_type, exc_val, exc_tb):
        self.close()

    @property
    def driver(self) -> Driver:
        """Get the Neo4j driver."""
        return self._driver
    
    def close(self):
        """Close the Neo4j driver connections."""
        if self._driver:
            self._driver.close()
            logger.info("Neo4j driver closed")
        logger.info("Closing Neo4j client")
    
    def get_session(self, database: str = None) -> Session:
        """Get a Neo4j session."""
        return self.driver.session(database=database or self.settings.NEO4J_DATABASE)
    
    def get_async_session(self, database=None):
        """Get a Neo4j session for async-compatible mocking in tests."""
        return self.driver.session(database=database or self.settings.NEO4J_DATABASE)
    
    def execute_query_with_retry(self, query, parameters=None, database=None):
        """Execute a Cypher query with retry logic."""
        retry_count = 0
        last_error = None
        
        while retry_count < self.settings.NEO4J_MAX_RETRY_ATTEMPTS:
            try:
                with self.get_session(database) as session:
                    result = session.run(query, parameters or {})
                    return result.values()
            except (ServiceUnavailable, SessionExpired) as e:
                last_error = e
                retry_count += 1
                logger.warning(
                    "Neo4j query failed, retrying",
                    error=str(e),
                    retry_count=retry_count,
                    max_retries=self.settings.NEO4J_MAX_RETRY_ATTEMPTS
                )
                
                if retry_count < self.settings.NEO4J_MAX_RETRY_ATTEMPTS:
                    # Exponential backoff
                    wait_time = self.settings.NEO4J_RETRY_DELAY * (2 ** (retry_count - 1))
                    time.sleep(wait_time)
                else:
                    logger.error(
                        "Neo4j query failed after max retries",
                        error=str(e),
                        query=query
                    )
                    raise
            except Exception as e:
                logger.error(
                    "Neo4j query failed with unexpected error",
                    error=str(e),
                    query=query
                )
                raise
        
        if last_error:
            raise last_error
        
        return None

@lru_cache()
def get_neo4j_client() -> Neo4jClient:
    """Get a Neo4j client, creating it if necessary."""
    settings = get_app_settings()
    return Neo4jClient(settings.neo4j)


class Neo4jHealthChecker:
    """A class to check the health of the Neo4j database."""

    @staticmethod
    async def check_health(client: Neo4jClient) -> dict:
        """
        Check Neo4j connection health.
        
        Args:
            client: Neo4j client instance
            
        Returns:
            bool: True if Neo4j is healthy, False otherwise
        """
        try:
            with client.get_session() as session:
                result = session.run("RETURN 1 as n")
                record = result.single()
                assert record["n"] == 1
            
            logger.info("Neo4j health check passed")
            return True
        except Exception as e:
            logger.error("Neo4j health check failed", error=str(e))
            return False
    
    @staticmethod
    def check_health_with_details(client: Neo4jClient) -> dict:
        """Check Neo4j connection health with details."""
        try:
            with client.get_session() as session:
                # Basic connectivity test
                result = session.run("RETURN 1 as n")
                record = result.single()
                assert record["n"] == 1
                
                # Get Neo4j version
                result = session.run("CALL dbms.components() YIELD name, versions, edition RETURN name, versions, edition")
                record = result.single()
                
                # Get database information
                result = session.run("CALL db.info()")
                db_info = result.single()
            
            result = {
                "healthy": True,
                "name": record["name"],
                "version": record["versions"][0],
                "edition": record["edition"],
                "database": client.settings.NEO4J_DATABASE,
                "uri": client.settings.NEO4J_URI,
                "database_info": dict(db_info)
            }
            
            logger.info("Neo4j detailed health check passed", **result)
            return result
            
        except Exception as e:
            result = {
                "healthy": False,
                "error": str(e),
                "database": client.settings.NEO4J_DATABASE,
                "uri": client.settings.NEO4J_URI
            }
            logger.error("Neo4j detailed health check failed", **result)
            return result
