#!/usr/bin/env python3
"""
Cosmos DB Initialization Microservice

This microservice initializes the Cosmos DB emulator with the required database and container
for the risk analytics application. It uses the shared common library for consistency.
"""

import time
import logging
import sys
import urllib3
from azure.cosmos import exceptions

from config import Settings
from cosmos_db.cosmos_db_client import CosmosDBClient

# Disable SSL warnings for the emulator
urllib3.disable_warnings(urllib3.exceptions.InsecureRequestWarning)

logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(name)s - %(levelname)s - %(message)s'
)
logger = logging.getLogger(__name__)


def wait_for_cosmos_ready(settings: Settings) -> CosmosDBClient:
    """
    Wait for Cosmos DB to be ready and return a client.
    
    Args:
        settings: Configuration settings
        
    Returns:
        CosmosDBClient: Ready Cosmos DB client
        
    Raises:
        Exception: If Cosmos DB doesn't become ready within timeout
    """
    logger.info("Waiting for Cosmos DB to be ready...")
    
    for attempt in range(settings.MAX_RETRIES):
        try:
            # Try to create a client - this will test connectivity
            cosmos_client = CosmosDBClient(
                database_name=settings.COSMOS_DB_NAME,
                container_name=settings.CONTAINER_NAME
            )
            logger.info("Cosmos DB is ready and initialized!")
            return cosmos_client
            
        except Exception as e:
            logger.info(f"Attempt {attempt + 1}/{settings.MAX_RETRIES}: Cosmos DB not ready yet ({e})")
            if attempt < settings.MAX_RETRIES - 1:
                time.sleep(settings.RETRY_DELAY)
            else:
                raise Exception("Cosmos DB failed to become ready within the timeout period")


def verify_initialization(cosmos_client: CosmosDBClient, settings: Settings) -> bool:
    """
    Verify that the database and container were created successfully.
    
    Args:
        cosmos_client: Cosmos DB client
        settings: Configuration settings
        
    Returns:
        bool: True if verification successful, False otherwise
    """
    try:
        logger.info("Verifying database and container initialization...")
        
        # Try to query the container (this will fail if it doesn't exist)
        test_query = "SELECT VALUE COUNT(1) FROM c"
        result = cosmos_client.query_items(test_query)
        count = list(result)[0] if result else 0
        
        logger.info(f"Verification successful! Container exists and contains {count} items.")
        return True
        
    except Exception as e:
        logger.error(f"Verification failed: {e}")
        return False


def main():
    """Main initialization function."""
    logger.info("=== Cosmos DB Initialization Service Starting ===")
    
    try:
        # Load settings
        settings = Settings()
        logger.info(f"Loaded configuration for database: {settings.COSMOS_DB_NAME}")
        logger.info(f"Container: {settings.CONTAINER_NAME}")
        logger.info(f"Using local fallback: {settings.USE_LOCAL_FALLBACK}")
        
        # Wait for Cosmos DB to be ready and initialize it
        cosmos_client = wait_for_cosmos_ready(settings)
        
        # Verify initialization
        if verify_initialization(cosmos_client, settings):
            logger.info("=== Cosmos DB initialization completed successfully! ===")
            sys.exit(0)
        else:
            logger.error("=== Cosmos DB initialization verification failed! ===")
            sys.exit(1)
            
    except Exception as e:
        logger.error(f"=== Fatal error during Cosmos DB initialization: {e} ===")
        sys.exit(1)


if __name__ == "__main__":
    main() 