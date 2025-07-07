import pytest
import os
import logging
from unittest.mock import patch, MagicMock

from cosmos_db.cosmos_db_client import CosmosDBClient

logging.basicConfig(level=logging.DEBUG)
logger = logging.getLogger(__name__)


class TestCosmosDBClientFallback:
    """Test suite for Cosmos DB client fallback mechanism."""
    
    def setup_method(self):
        """Setup for each test method."""
        # Clear environment variables to ensure clean state
        self.original_env = os.environ.copy()
        for key in ['AZURE_COSMOSDB_ENDPOINT', 'USE_LOCAL_FALLBACK', 'COSMOS_EMULATOR_ENDPOINT', 'COSMOS_EMULATOR_KEY']:
            os.environ.pop(key, None)
    
    def teardown_method(self):
        """Cleanup after each test method."""
        # Restore original environment
        os.environ.clear()
        os.environ.update(self.original_env)
    
    @patch('cosmos_db.cosmos_db_client.CosmosClient')
    @patch('cosmos_db.cosmos_db_client.DefaultAzureCredential')
    def test_azure_connection_success(self, mock_credential, mock_cosmos_client):
        """Test successful connection to Azure Cosmos DB."""
        # Setup
        os.environ['AZURE_COSMOSDB_ENDPOINT'] = 'https://test-cosmos.documents.azure.com:443/'
        
        mock_credential_instance = MagicMock()
        mock_credential.return_value = mock_credential_instance
        
        mock_client_instance = MagicMock()
        mock_cosmos_client.return_value = mock_client_instance
        
        # Mock successful database list (connection test)
        mock_client_instance.list_databases.return_value = []
        
        # Mock database and container clients
        mock_db_client = MagicMock()
        mock_container_client = MagicMock()
        mock_client_instance.get_database_client.return_value = mock_db_client
        mock_db_client.get_container_client.return_value = mock_container_client
        
        # Mock database and container existence checks
        mock_db_client.read.return_value = {}
        mock_container_client.read.return_value = {}
        
        # Test
        client = CosmosDBClient("test-db", "test-container")
        
        # Verify
        mock_credential.assert_called_once()
        mock_cosmos_client.assert_called_once_with(
            url='https://test-cosmos.documents.azure.com:443/',
            credential=mock_credential_instance
        )
        assert client.database_name == "test-db"
        assert client.container_name == "test-container"
    
    @patch('cosmos_db.cosmos_db_client.CosmosClient')
    @patch('cosmos_db.cosmos_db_client.DefaultAzureCredential')
    def test_azure_connection_failure_fallback_to_emulator(self, mock_credential, mock_cosmos_client):
        """Test fallback to emulator when Azure connection fails."""
        os.environ['AZURE_COSMOSDB_ENDPOINT'] = 'https://test-cosmos.documents.azure.com:443/'
        os.environ['COSMOS_EMULATOR_ENDPOINT'] = 'https://localhost:8081'
        os.environ['COSMOS_EMULATOR_KEY'] = 'test-key'
        
        mock_credential_instance = MagicMock()
        mock_credential.return_value = mock_credential_instance
        
        mock_azure_client = MagicMock()
        mock_emulator_client = MagicMock()
        
        mock_cosmos_client.side_effect = [
            Exception("Azure connection failed"),
            mock_emulator_client
        ]
        
        mock_emulator_client.list_databases.return_value = []
        
        mock_db_client = MagicMock()
        mock_container_client = MagicMock()
        mock_emulator_client.get_database_client.return_value = mock_db_client
        mock_db_client.get_container_client.return_value = mock_container_client
        
        mock_db_client.read.side_effect = Exception("Database not found")
        mock_container_client.read.side_effect = Exception("Container not found")
        mock_emulator_client.create_database.return_value = None
        mock_db_client.create_container.return_value = None
        
        client = CosmosDBClient("test-db", "test-container")
        
        assert mock_cosmos_client.call_count == 2
        
        first_call = mock_cosmos_client.call_args_list[0]
        assert first_call[1]['url'] == 'https://test-cosmos.documents.azure.com:443/'
        assert first_call[1]['credential'] == mock_credential_instance
        
        second_call = mock_cosmos_client.call_args_list[1]
        assert second_call[1]['url'] == 'https://localhost:8081'
        assert second_call[1]['credential'] == 'test-key'
    
    @patch('cosmos_db.cosmos_db_client.CosmosClient')
    def test_forced_local_fallback(self, mock_cosmos_client):
        """Test forced local fallback mode."""
        # Setup
        os.environ['USE_LOCAL_FALLBACK'] = 'true'
        os.environ['AZURE_COSMOSDB_ENDPOINT'] = 'https://test-cosmos.documents.azure.com:443/'
        os.environ['COSMOS_EMULATOR_ENDPOINT'] = 'https://localhost:8081'
        
        mock_client_instance = MagicMock()
        mock_cosmos_client.return_value = mock_client_instance
        mock_client_instance.list_databases.return_value = []
        
        # Mock database and container clients
        mock_db_client = MagicMock()
        mock_container_client = MagicMock()
        mock_client_instance.get_database_client.return_value = mock_db_client
        mock_db_client.get_container_client.return_value = mock_container_client
        
        # Mock database and container existence
        mock_db_client.read.return_value = {}
        mock_container_client.read.return_value = {}
        
        # Test
        client = CosmosDBClient("test-db", "test-container")
        
        # Verify only emulator was called (Azure skipped due to forced fallback)
        mock_cosmos_client.assert_called_once_with(
            url='https://localhost:8081',
            credential='C2y6yDjf5/R+ob0N8A7Cgv30VRDJIWEHLM+4QDU5DE2nQ9nDuVTqobD4b8mGGyPMbIZnqyMsEcaGQy67XIw/Jw=='
        )
    
    @patch('cosmos_db.cosmos_db_client.CosmosClient')
    def test_no_azure_endpoint_fallback_to_emulator(self, mock_cosmos_client):
        """Test fallback when no Azure endpoint is configured."""
        # Setup - no Azure endpoint
        os.environ['COSMOS_EMULATOR_ENDPOINT'] = 'https://localhost:8081'
        
        mock_client_instance = MagicMock()
        mock_cosmos_client.return_value = mock_client_instance
        mock_client_instance.list_databases.return_value = []
        
        # Mock database and container clients
        mock_db_client = MagicMock()
        mock_container_client = MagicMock()
        mock_client_instance.get_database_client.return_value = mock_db_client
        mock_db_client.get_container_client.return_value = mock_container_client
        
        # Mock database and container existence
        mock_db_client.read.return_value = {}
        mock_container_client.read.return_value = {}
        
        # Test
        client = CosmosDBClient("test-db", "test-container")
        
        # Verify only emulator was called
        mock_cosmos_client.assert_called_once_with(
            url='https://localhost:8081',
            credential='C2y6yDjf5/R+ob0N8A7Cgv30VRDJIWEHLM+4QDU5DE2nQ9nDuVTqobD4b8mGGyPMbIZnqyMsEcaGQy67XIw/Jw=='
        )
    
    @patch('cosmos_db.cosmos_db_client.CosmosClient')
    def test_both_azure_and_emulator_fail(self, mock_cosmos_client):
        """Test when both Azure and emulator connections fail."""
        # Setup
        os.environ['AZURE_COSMOSDB_ENDPOINT'] = 'https://test-cosmos.documents.azure.com:443/'
        
        # Mock both connections failing
        mock_cosmos_client.side_effect = Exception("Connection failed")
        
        # Test
        with pytest.raises(ValueError, match="Could not connect to either Azure Cosmos DB or emulator"):
            CosmosDBClient("test-db", "test-container")
    
    @patch('cosmos_db.cosmos_db_client.CosmosClient')
    def test_database_and_container_creation(self, mock_cosmos_client):
        """Test database and container creation when they don't exist."""
        # Setup
        os.environ['COSMOS_EMULATOR_ENDPOINT'] = 'https://localhost:8081'
        
        mock_client_instance = MagicMock()
        mock_cosmos_client.return_value = mock_client_instance
        mock_client_instance.list_databases.return_value = []
        
        # Mock database and container clients
        mock_db_client = MagicMock()
        mock_container_client = MagicMock()
        mock_client_instance.get_database_client.return_value = mock_db_client
        mock_db_client.get_container_client.return_value = mock_container_client
        
        # Mock database and container not existing
        mock_db_client.read.side_effect = Exception("Database not found")
        mock_container_client.read.side_effect = Exception("Container not found")
        
        # Test
        client = CosmosDBClient("test-db", "test-container")
        
        # Verify database and container creation was attempted
        mock_client_instance.create_database.assert_called_once_with("test-db")
        mock_db_client.create_container.assert_called_once_with(
            id="test-container",
            partition_key="/id"
        )
    
    @patch('cosmos_db.cosmos_db_client.CosmosClient')
    def test_upsert_item_with_auto_id_generation(self, mock_cosmos_client):
        """Test upserting items with automatic ID generation."""
        # Setup client
        os.environ['COSMOS_EMULATOR_ENDPOINT'] = 'https://localhost:8081'
        
        mock_client_instance = MagicMock()
        mock_cosmos_client.return_value = mock_client_instance
        mock_client_instance.list_databases.return_value = []
        
        mock_db_client = MagicMock()
        mock_container_client = MagicMock()
        mock_client_instance.get_database_client.return_value = mock_db_client
        mock_db_client.get_container_client.return_value = mock_container_client
        
        # Mock existing database and container
        mock_db_client.read.return_value = {}
        mock_container_client.read.return_value = {}
        
        client = CosmosDBClient("test-db", "test-container")
        
        # Test upserting item without ID (should generate from article_hash)
        item_with_hash = {"title": "Test", "article_hash": "abc123"}
        client.upsert_item(item_with_hash)
        
        # Verify ID was set from article_hash
        assert item_with_hash["id"] == "abc123"
        mock_container_client.upsert_item.assert_called_with(body=item_with_hash)
        
        # Test upserting item without ID or article_hash (should generate UUID)
        item_without_hash = {"title": "Test"}
        client.upsert_item(item_without_hash)
        
        # Verify ID was generated
        assert "id" in item_without_hash
        assert len(item_without_hash["id"]) > 0
    
    @patch('cosmos_db.cosmos_db_client.CosmosClient')
    def test_query_items(self, mock_cosmos_client):
        """Test querying items."""
        # Setup client
        os.environ['COSMOS_EMULATOR_ENDPOINT'] = 'https://localhost:8081'
        
        mock_client_instance = MagicMock()
        mock_cosmos_client.return_value = mock_client_instance
        mock_client_instance.list_databases.return_value = []
        
        mock_db_client = MagicMock()
        mock_container_client = MagicMock()
        mock_client_instance.get_database_client.return_value = mock_db_client
        mock_db_client.get_container_client.return_value = mock_container_client
        
        # Mock existing database and container
        mock_db_client.read.return_value = {}
        mock_container_client.read.return_value = {}
        
        # Mock query results
        mock_query_results = [{"id": "1", "title": "Test"}]
        mock_container_client.query_items.return_value = mock_query_results
        
        client = CosmosDBClient("test-db", "test-container")
        
        # Test query
        query = "SELECT * FROM c WHERE c.article_hash = 'abc123'"
        results = client.query_items(query)
        
        # Verify
        mock_container_client.query_items.assert_called_once_with(
            query=query,
            parameters=None,
            enable_cross_partition_query=True
        )
        assert results == mock_query_results 