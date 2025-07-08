import os
import logging
from typing import List, Dict, Any
from datetime import datetime
from azure.cosmos import CosmosClient
from azure.cosmos.partition_key import PartitionKey
from azure.identity import DefaultAzureCredential

logger = logging.getLogger(__name__)

class CosmosDBClient:
    """
    A client for interacting with Azure Cosmos DB or Cosmos DB Emulator.
    
    This client uses DefaultAzureCredential for Azure authentication, and falls back
    to the Cosmos DB emulator when Azure services are not available.
    """
    def __init__(self, database_name: str, container_name: str):
        """
        Initializes the CosmosDBClient.

        Args:
            database_name (str): The name of the Cosmos DB database.
            container_name (str): The name of the container within the database.
        """
        self.database_name = database_name
        self.container_name = container_name
        
        self.client = self._initialize_client()
        self.database_client = self.client.get_database_client(database_name)
        self.container_client = self.database_client.get_container_client(container_name)
        
        self._ensure_database_and_container()

    def _initialize_client(self) -> CosmosClient:
        """
        Initialize the Cosmos client, trying Azure first, then emulator fallback.
        
        Returns:
            CosmosClient: Initialized Cosmos client
        """
        use_local_fallback = os.environ.get("USE_LOCAL_FALLBACK", "false").lower() == "true"
        
        if not use_local_fallback:
            azure_endpoint = os.environ.get("AZURE_COSMOSDB_ENDPOINT")
            if azure_endpoint:
                try:
                    logger.info("Attempting to connect to Azure Cosmos DB...")
                    credential = DefaultAzureCredential()
                    client = CosmosClient(url=azure_endpoint, credential=credential)
                    
                    list(client.list_databases())
                    logger.info("Successfully connected to Azure Cosmos DB")
                    return client
                    
                except Exception as e:
                    logger.warning(f"Failed to connect to Azure Cosmos DB: {e}")
        
        logger.info("Falling back to Cosmos DB emulator...")
        emulator_endpoint = os.environ.get("COSMOS_EMULATOR_ENDPOINT", "https://cosmosdb-emulator:8081")
        logger.info(f"Using emulator endpoint: {emulator_endpoint}")
        emulator_key = os.environ.get("COSMOS_EMULATOR_KEY", 
                                    "C2y6yDjf5/R+ob0N8A7Cgv30VRDJIWEHLM+4QDU5DE2nQ9nDuVTqobD4b8mGGyPMbIZnqyMsEcaGQy67XIw/Jw==")
        
        try:
            import time
            max_retries = 3
            retry_delay = 2
            
            for attempt in range(max_retries):
                try:
                    client = CosmosClient(
                        url=emulator_endpoint, 
                        credential=emulator_key,
                        connection_verify=False,  # Disable SSL verification for self-signed certificates
                        connection_timeout=30,    # Longer timeout for SSL handshake
                        request_timeout=60        # Longer request timeout for emulator
                    )
                    
                    # Test connection by listing databases
                    databases = list(client.list_databases())
                    logger.info(f"Successfully connected to Cosmos DB emulator at {emulator_endpoint}")
                    logger.info(f"Found {len(databases)} databases in emulator")
                    return client
                    
                except Exception as e:
                    logger.warning(f"Connection attempt {attempt + 1}/{max_retries} failed: {e}")
                    logger.warning(f"Error type: {type(e).__name__}")
                    if attempt < max_retries - 1:
                        logger.info(f"Retrying in {retry_delay} seconds...")
                        time.sleep(retry_delay)
                    else:
                        logger.error(f"All connection attempts failed. Last error: {e}")
                        raise e
            
        except Exception as e:
            logger.error(f"Failed to connect to Cosmos DB emulator: {e}")
            raise ValueError(f"Could not connect to either Azure Cosmos DB or emulator. Last error: {e}")

    def _ensure_database_and_container(self):
        """
        Ensure the database and container exist, creating them if necessary.
        This is especially important for the emulator which starts empty.
        """
        try:
            try:
                self.database_client.read()
                logger.debug(f"Database '{self.database_name}' already exists")
            except Exception:
                logger.info(f"Creating database '{self.database_name}'...")
                self.client.create_database(self.database_name)
                self.database_client = self.client.get_database_client(self.database_name)
            
            try:
                self.container_client.read()
                logger.debug(f"Container '{self.container_name}' already exists")
            except Exception:
                logger.info(f"Creating container '{self.container_name}'...")
                self.database_client.create_container(
                    id=self.container_name,
                    partition_key=PartitionKey(path="/id")
                )
                self.container_client = self.database_client.get_container_client(self.container_name)
                
        except Exception as e:
            logger.error(f"Failed to ensure database and container exist: {e}")
            raise

    def _serialize_datetimes(self, obj: Any) -> Any:
        """
        Recursively serialize datetime objects to ISO format strings.
        
        This method traverses dictionaries, lists, and other data structures,
        converting any datetime objects to ISO format strings for JSON serialization.
        
        Args:
            obj: The object to serialize (can be dict, list, datetime, or other types)
            
        Returns:
            The object with datetime instances converted to ISO format strings
        """
        if isinstance(obj, datetime):
            return obj.isoformat()
        elif isinstance(obj, dict):
            return {key: self._serialize_datetimes(value) for key, value in obj.items()}
        elif isinstance(obj, list):
            return [self._serialize_datetimes(item) for item in obj]
        elif isinstance(obj, tuple):
            return tuple(self._serialize_datetimes(item) for item in obj)
        else:
            # Return other types unchanged (str, int, float, bool, None, etc.)
            return obj

    def upsert_item(self, item: Dict[str, Any]) -> None:
        """
        Upserts an item into the specified container.
        
        Automatically serializes datetime objects to ISO format strings
        to ensure JSON compatibility with Cosmos DB.

        Args:
            item (Dict[str, Any]): The item to upsert. It must be a dictionary.
        """
        try:
            if 'id' not in item:
                if 'article_hash' in item:
                    item['id'] = item['article_hash']
                else:
                    import uuid
                    item['id'] = str(uuid.uuid4())
            
            # Serialize datetime objects to ISO format strings
            serialized_item = self._serialize_datetimes(item)
            
            self.container_client.upsert_item(body=serialized_item)
            logger.debug(f"Upserted item with id: {serialized_item['id']}")
            
        except Exception as e:
            logger.error(f"Failed to upsert item: {e}")
            raise

    def query_items(self, query: str, parameters: List[Dict[str, Any]] = None) -> List[Dict[str, Any]]:
        """
        Queries items from the container.

        Args:
            query (str): The SQL-like query to execute.
            parameters (List[Dict[str, Any]], optional): Parameters for the query. Defaults to None.

        Returns:
            List[Dict[str, Any]]: A list of items matching the query.
        """
        try:
            items = self.container_client.query_items(
                query=query,
                parameters=parameters,
                enable_cross_partition_query=True
            )
            result = list(items)
            logger.debug(f"Query returned {len(result)} items")
            return result
            
        except Exception as e:
            logger.error(f"Failed to query items: {e}")
            raise