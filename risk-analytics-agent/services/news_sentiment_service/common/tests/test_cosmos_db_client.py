import pytest
from unittest.mock import patch, MagicMock
from cosmos_db.cosmos_db_client import CosmosDBClient

DATABASE_NAME = "test_db"
CONTAINER_NAME = "test_container"

@pytest.fixture
def mock_cosmos_client():
    """Fixture to mock the Azure CosmosClient."""
    with patch('os.environ.get', return_value="https://dummy.documents.azure.com:443/"), \
         patch('cosmos_db.cosmos_db_client.CosmosClient') as mock_client_class:
        mock_client_instance = MagicMock()
        mock_database_client = MagicMock()
        mock_container_client = MagicMock()

        mock_client_class.return_value = mock_client_instance
        mock_client_instance.get_database_client.return_value = mock_database_client
        mock_database_client.get_container_client.return_value = mock_container_client
        
        yield mock_container_client

def test_cosmosdb_client_initialization(mock_cosmos_client):
    """Tests that the CosmosDBClient initializes correctly."""
    with patch('cosmos_db.cosmos_db_client.DefaultAzureCredential') as mock_credential:
        client = CosmosDBClient(DATABASE_NAME, CONTAINER_NAME)
        assert client.database_name == DATABASE_NAME
        assert client.container_name == CONTAINER_NAME
        mock_credential.assert_called_once()
        # We don't have the endpoint, so we can't test the CosmosClient call directly
        # but we can infer it was called by checking the fixture setup.

def test_upsert_item_calls_sdk(mock_cosmos_client):
    """Tests that the upsert_item method calls the underlying SDK's upsert_item."""
    with patch('cosmos_db.cosmos_db_client.DefaultAzureCredential'):
        client = CosmosDBClient(DATABASE_NAME, CONTAINER_NAME)
        test_item = {"id": "123", "data": "test"}
        client.upsert_item(test_item)
        mock_cosmos_client.upsert_item.assert_called_once_with(body=test_item)

def test_query_items_calls_sdk_and_returns_list(mock_cosmos_client):
    """Tests that query_items method calls the SDK and returns a list of results."""
    with patch('cosmos_db.cosmos_db_client.DefaultAzureCredential'):
        client = CosmosDBClient(DATABASE_NAME, CONTAINER_NAME)
        test_query = "SELECT * FROM c"
        expected_results = [{"id": "1"}, {"id": "2"}]
        mock_cosmos_client.query_items.return_value = expected_results
        
        results = client.query_items(test_query)
        
        mock_cosmos_client.query_items.assert_called_once_with(
            query=test_query,
            parameters=None,
            enable_cross_partition_query=True
        )
        assert results == expected_results

def test_query_items_with_parameters(mock_cosmos_client):
    """Tests that query_items passes parameters correctly to the SDK."""
    with patch('cosmos_db.cosmos_db_client.DefaultAzureCredential'):
        client = CosmosDBClient(DATABASE_NAME, CONTAINER_NAME)
        test_query = "SELECT * FROM c WHERE c.id = @id"
        test_params = [{"name": "@id", "value": "123"}]
        
        client.query_items(test_query, parameters=test_params)
        
        mock_cosmos_client.query_items.assert_called_once_with(
            query=test_query,
            parameters=test_params,
            enable_cross_partition_query=True
        ) 