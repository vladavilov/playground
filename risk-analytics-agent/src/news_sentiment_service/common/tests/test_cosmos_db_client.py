import pytest
from unittest.mock import patch, MagicMock
from datetime import datetime, timezone
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

# Datetime Serialization Tests
def test_upsert_item_serializes_datetime_objects(mock_cosmos_client):
    """Tests that datetime objects are serialized to ISO format strings."""
    with patch('cosmos_db.cosmos_db_client.DefaultAzureCredential'):
        client = CosmosDBClient(DATABASE_NAME, CONTAINER_NAME)
        
        test_datetime = datetime(2023, 12, 25, 10, 30, 45, tzinfo=timezone.utc)
        test_item = {
            "id": "test_datetime",
            "published_at": test_datetime,
            "ingested_at": test_datetime,
            "data": "test"
        }
        
        client.upsert_item(test_item)
        
        # Verify the call was made with serialized datetime
        call_args = mock_cosmos_client.upsert_item.call_args[1]['body']
        assert call_args["published_at"] == "2023-12-25T10:30:45+00:00"
        assert call_args["ingested_at"] == "2023-12-25T10:30:45+00:00"
        assert call_args["data"] == "test"  # Non-datetime fields unchanged
        assert call_args["id"] == "test_datetime"

def test_upsert_item_handles_nested_datetime_objects(mock_cosmos_client):
    """Tests that datetime objects in nested dictionaries are serialized."""
    with patch('cosmos_db.cosmos_db_client.DefaultAzureCredential'):
        client = CosmosDBClient(DATABASE_NAME, CONTAINER_NAME)
        
        test_datetime = datetime(2023, 12, 25, 10, 30, 45, tzinfo=timezone.utc)
        test_item = {
            "id": "nested_test",
            "metadata": {
                "created_at": test_datetime,
                "nested_data": {
                    "timestamp": test_datetime
                }
            },
            "events": [
                {"event_time": test_datetime, "name": "event1"},
                {"event_time": test_datetime, "name": "event2"}
            ]
        }
        
        client.upsert_item(test_item)
        
        call_args = mock_cosmos_client.upsert_item.call_args[1]['body']
        assert call_args["metadata"]["created_at"] == "2023-12-25T10:30:45+00:00"
        assert call_args["metadata"]["nested_data"]["timestamp"] == "2023-12-25T10:30:45+00:00"
        assert call_args["events"][0]["event_time"] == "2023-12-25T10:30:45+00:00"
        assert call_args["events"][1]["event_time"] == "2023-12-25T10:30:45+00:00"

def test_upsert_item_preserves_non_datetime_objects(mock_cosmos_client):
    """Tests that non-datetime objects are preserved unchanged."""
    with patch('cosmos_db.cosmos_db_client.DefaultAzureCredential'):
        client = CosmosDBClient(DATABASE_NAME, CONTAINER_NAME)
        
        test_item = {
            "id": "mixed_types",
            "string_field": "test_string",
            "int_field": 42,
            "float_field": 3.14,
            "bool_field": True,
            "null_field": None,
            "list_field": [1, 2, "three"],
            "dict_field": {"key": "value"}
        }
        
        client.upsert_item(test_item)
        
        call_args = mock_cosmos_client.upsert_item.call_args[1]['body']
        assert call_args == test_item  # Should be unchanged

def test_upsert_item_handles_enriched_news_event_model(mock_cosmos_client):
    """Tests serialization of EnrichedNewsEvent model data structure."""
    with patch('cosmos_db.cosmos_db_client.DefaultAzureCredential'):
        client = CosmosDBClient(DATABASE_NAME, CONTAINER_NAME)
        
        published_time = datetime(2023, 12, 25, 10, 30, 45, tzinfo=timezone.utc)
        ingested_time = datetime(2023, 12, 25, 11, 0, 0, tzinfo=timezone.utc)
        
        # Simulate EnrichedNewsEvent.model_dump() output
        enriched_event_dict = {
            "id": "test_event_123",
            "source": "Reuters",
            "published_at": published_time,
            "ingested_at": ingested_time,
            "event_type": "Credit_Rating_Upgrade",
            "entities": {
                "issuer_name": "Apple Inc.",
                "sector": "corporate",
                "state": None,
                "cusips": ["037833100"]
            },
            "sentiment": {
                "score": 0.7,
                "magnitude": 0.8
            },
            "source_credibility_tier": "TIER_2_PREMIUM_FINANCIAL",
            "summary_excerpt": "Apple credit upgraded by S&P",
            "raw_article_url": "https://example.com/article"
        }
        
        client.upsert_item(enriched_event_dict)
        
        call_args = mock_cosmos_client.upsert_item.call_args[1]['body']
        assert call_args["published_at"] == "2023-12-25T10:30:45+00:00"
        assert call_args["ingested_at"] == "2023-12-25T11:00:00+00:00"
        # Verify other fields remain unchanged
        assert call_args["entities"]["issuer_name"] == "Apple Inc."
        assert call_args["sentiment"]["score"] == 0.7

def test_upsert_item_handles_empty_and_edge_cases(mock_cosmos_client):
    """Tests edge cases for datetime serialization."""
    with patch('cosmos_db.cosmos_db_client.DefaultAzureCredential'):
        client = CosmosDBClient(DATABASE_NAME, CONTAINER_NAME)
        
        # Test empty dict
        client.upsert_item({})
        
        # Test dict with only non-datetime fields
        simple_item = {"id": "simple", "name": "test"}
        client.upsert_item(simple_item)
        
        # Test with None values
        none_item = {"id": "none_test", "datetime_field": None}
        client.upsert_item(none_item)
        
        # All should succeed without errors
        assert mock_cosmos_client.upsert_item.call_count == 3

def test_integration_original_error_scenario_fixed(mock_cosmos_client):
    """
    Integration test demonstrating the original 'Object of type datetime is not JSON serializable' error is fixed.
    
    This test replicates the exact scenario that caused the original error:
    EnrichedNewsEvent.model_dump() containing datetime objects being passed to upsert_item.
    """
    with patch('cosmos_db.cosmos_db_client.DefaultAzureCredential'):
        client = CosmosDBClient(DATABASE_NAME, CONTAINER_NAME)
        
        # This is exactly what happens in message_processor.py line 157
        # when an EnrichedNewsEvent.model_dump() is passed to upsert_item
        from datetime import datetime, timezone
        
        enriched_event_dict = {
            "id": "integration_test_123",
            "source": "Reuters",
            "published_at": datetime(2023, 12, 25, 10, 30, 45, tzinfo=timezone.utc),
            "ingested_at": datetime(2023, 12, 25, 11, 0, 0, tzinfo=timezone.utc),
            "event_type": "Credit_Rating_Upgrade",
            "entities": {
                "issuer_name": "Apple Inc.",
                "sector": "corporate",
                "state": None,
                "cusips": ["037833100"]
            },
            "sentiment": {
                "score": 0.7,
                "magnitude": 0.8
            },
            "source_credibility_tier": "TIER_2_PREMIUM_FINANCIAL",
            "summary_excerpt": "Apple credit upgraded by S&P",
            "raw_article_url": "https://example.com/article"
        }
        
        # This should NOT raise "Object of type datetime is not JSON serializable" anymore
        try:
            client.upsert_item(enriched_event_dict)
            integration_success = True
        except TypeError as e:
            if "not JSON serializable" in str(e):
                integration_success = False
            else:
                raise  # Re-raise if it's a different error
        
        # Verify the fix worked
        assert integration_success, "The original datetime serialization error should be fixed"
        
        # Verify the data was correctly serialized
        call_args = mock_cosmos_client.upsert_item.call_args[1]['body']
        assert call_args["published_at"] == "2023-12-25T10:30:45+00:00"
        assert call_args["ingested_at"] == "2023-12-25T11:00:00+00:00"
        assert call_args["entities"]["issuer_name"] == "Apple Inc."  # Non-datetime fields preserved 