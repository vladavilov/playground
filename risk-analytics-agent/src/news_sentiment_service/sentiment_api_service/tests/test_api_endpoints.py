import pytest
from datetime import datetime, timezone
from fastapi.testclient import TestClient
from unittest.mock import Mock, patch

from src.main import app


@pytest.fixture
def client():
    """Create a FastAPI test client."""
    return TestClient(app)


@pytest.fixture
def mock_cosmos_client():
    """Create a mock Cosmos DB client."""
    mock_client = Mock()
    return mock_client


@pytest.fixture
def sample_cosmos_events():
    """Sample events data as would be returned from Cosmos DB."""
    return [
        {
            "id": "event1",
            "source": "Bloomberg",
            "published_at": "2024-01-15T12:00:00Z",
            "ingested_at": "2024-01-15T12:00:00Z",
            "event_type": "Credit_Rating_Downgrade",
            "entities": {
                "issuer_name": "ABC Corp",
                "sector": "Technology",
                "state": None,
                "cusips": ["12345678X"]
            },
            "sentiment": {
                "score": -0.8,
                "magnitude": 0.9
            },
            "source_credibility_tier": "TIER_1_REGULATOR",
            "summary_excerpt": "S&P downgrades ABC Corp",
            "raw_article_url": "https://example.com/1"
        },
        {
            "id": "event2",
            "source": "Reuters",
            "published_at": "2024-01-15T10:00:00Z",
            "ingested_at": "2024-01-15T12:00:00Z",
            "event_type": "Earnings_Beat",
            "entities": {
                "issuer_name": "ABC Corp",
                "sector": "Technology",
                "state": None,
                "cusips": ["12345678X"]
            },
            "sentiment": {
                "score": 0.6,
                "magnitude": 0.7
            },
            "source_credibility_tier": "TIER_2_PREMIUM_FINANCIAL",
            "summary_excerpt": "ABC Corp beats earnings",
            "raw_article_url": "https://example.com/2"
        }
    ]


@patch('src.main.get_cosmos_client')
def test_realtime_sentiment_endpoint_success_with_cusip(mock_get_cosmos_client, client, sample_cosmos_events):
    """Test the realtime sentiment endpoint with valid CUSIP parameter."""
    # Setup mock
    mock_cosmos_client = Mock()
    mock_cosmos_client.query_items.return_value = sample_cosmos_events
    mock_get_cosmos_client.return_value = mock_cosmos_client
    
    # Make request
    response = client.get("/sentiment/realtime?cusip=12345678X")
    
    # Verify response
    assert response.status_code == 200
    response_data = response.json()
    
    assert "aggregated_sentiment_score" in response_data
    assert "contributing_articles_count" in response_data
    assert "articles" in response_data
    
    assert response_data["contributing_articles_count"] == 2
    assert isinstance(response_data["aggregated_sentiment_score"], float)
    assert isinstance(response_data["articles"], list)
    assert len(response_data["articles"]) == 2


@patch('src.main.get_cosmos_client')
def test_realtime_sentiment_endpoint_success_with_sector(mock_get_cosmos_client, client, sample_cosmos_events):
    """Test the realtime sentiment endpoint with valid sector parameter."""
    # Setup mock
    mock_cosmos_client = Mock()
    mock_cosmos_client.query_items.return_value = sample_cosmos_events
    mock_get_cosmos_client.return_value = mock_cosmos_client
    
    # Make request
    response = client.get("/sentiment/realtime?sector=Technology")
    
    # Verify response
    assert response.status_code == 200
    response_data = response.json()
    
    assert "aggregated_sentiment_score" in response_data
    assert "contributing_articles_count" in response_data
    assert "articles" in response_data
    assert response_data["contributing_articles_count"] == 2


@patch('src.main.get_cosmos_client')
def test_realtime_sentiment_endpoint_success_with_issuer_name(mock_get_cosmos_client, client, sample_cosmos_events):
    """Test the realtime sentiment endpoint with valid issuer_name parameter."""
    # Setup mock
    mock_cosmos_client = Mock()
    mock_cosmos_client.query_items.return_value = sample_cosmos_events
    mock_get_cosmos_client.return_value = mock_cosmos_client
    
    # Make request
    response = client.get("/sentiment/realtime?issuer_name=ABC%20Corp")
    
    # Verify response
    assert response.status_code == 200
    response_data = response.json()
    
    assert "aggregated_sentiment_score" in response_data
    assert "contributing_articles_count" in response_data
    assert "articles" in response_data
    assert response_data["contributing_articles_count"] == 2


@patch('src.main.get_cosmos_client')
def test_realtime_sentiment_endpoint_no_events_found(mock_get_cosmos_client, client):
    """Test the realtime sentiment endpoint when no events are found."""
    # Setup mock to return empty list
    mock_cosmos_client = Mock()
    mock_cosmos_client.query_items.return_value = []
    mock_get_cosmos_client.return_value = mock_cosmos_client
    
    # Make request
    response = client.get("/sentiment/realtime?cusip=NOTFOUND1")
    
    # Verify response
    assert response.status_code == 200
    response_data = response.json()
    
    assert response_data["aggregated_sentiment_score"] == 0.0
    assert response_data["contributing_articles_count"] == 0
    assert response_data["articles"] == []


def test_realtime_sentiment_endpoint_missing_parameters(client):
    """Test the realtime sentiment endpoint with missing required parameters."""
    # Make request without any parameters
    response = client.get("/sentiment/realtime")
    
    # Should return 422 validation error
    assert response.status_code == 422


@patch('src.main.get_cosmos_client')
def test_realtime_sentiment_endpoint_multiple_parameters(mock_get_cosmos_client, client):
    """Test the realtime sentiment endpoint with multiple valid parameters."""
    # Setup mock
    mock_cosmos_client = Mock()
    mock_cosmos_client.query_items.return_value = []
    mock_get_cosmos_client.return_value = mock_cosmos_client
    
    # Make request with multiple parameters
    response = client.get("/sentiment/realtime?cusip=12345678X&sector=Technology&issuer_name=ABC%20Corp")
    
    # Should be successful
    assert response.status_code == 200


@patch('src.main.get_cosmos_client')
def test_historical_sentiment_endpoint_success(mock_get_cosmos_client, client, sample_cosmos_events):
    """Test the historical sentiment endpoint with valid parameters."""
    # Setup mock
    mock_cosmos_client = Mock()
    mock_cosmos_client.query_items.return_value = sample_cosmos_events
    mock_get_cosmos_client.return_value = mock_cosmos_client
    
    # Make request
    response = client.get("/sentiment/historical?as_of_date=2024-01-15&cusip=12345678X")
    
    # Verify response
    assert response.status_code == 200
    response_data = response.json()
    
    assert "aggregated_sentiment_score" in response_data
    assert "contributing_articles_count" in response_data
    # Historical endpoint should NOT include articles list
    assert "articles" not in response_data
    
    assert response_data["contributing_articles_count"] == 2
    assert isinstance(response_data["aggregated_sentiment_score"], float)


@patch('src.main.get_cosmos_client')
def test_historical_sentiment_endpoint_with_sector(mock_get_cosmos_client, client, sample_cosmos_events):
    """Test the historical sentiment endpoint with sector parameter."""
    # Setup mock
    mock_cosmos_client = Mock()
    mock_cosmos_client.query_items.return_value = sample_cosmos_events
    mock_get_cosmos_client.return_value = mock_cosmos_client
    
    # Make request
    response = client.get("/sentiment/historical?as_of_date=2024-01-15&sector=Technology")
    
    # Verify response
    assert response.status_code == 200
    response_data = response.json()
    
    assert "aggregated_sentiment_score" in response_data
    assert "contributing_articles_count" in response_data
    assert response_data["contributing_articles_count"] == 2


@patch('src.main.get_cosmos_client')
def test_historical_sentiment_endpoint_with_issuer_name(mock_get_cosmos_client, client, sample_cosmos_events):
    """Test the historical sentiment endpoint with issuer_name parameter."""
    # Setup mock
    mock_cosmos_client = Mock()
    mock_cosmos_client.query_items.return_value = sample_cosmos_events
    mock_get_cosmos_client.return_value = mock_cosmos_client
    
    # Make request
    response = client.get("/sentiment/historical?as_of_date=2024-01-15&issuer_name=ABC%20Corp")
    
    # Verify response
    assert response.status_code == 200
    response_data = response.json()
    
    assert "aggregated_sentiment_score" in response_data
    assert "contributing_articles_count" in response_data
    assert response_data["contributing_articles_count"] == 2


def test_historical_sentiment_endpoint_missing_as_of_date(client):
    """Test the historical sentiment endpoint without required as_of_date parameter."""
    # Make request without as_of_date
    response = client.get("/sentiment/historical?cusip=12345678X")
    
    # Should return 422 validation error
    assert response.status_code == 422


def test_historical_sentiment_endpoint_missing_entity_parameters(client):
    """Test the historical sentiment endpoint without entity parameters."""
    # Make request with only as_of_date
    response = client.get("/sentiment/historical?as_of_date=2024-01-15")
    
    # Should return 422 validation error (at least one entity parameter required)
    assert response.status_code == 422


def test_historical_sentiment_endpoint_invalid_date_format(client):
    """Test the historical sentiment endpoint with invalid date format."""
    # Make request with invalid date format
    response = client.get("/sentiment/historical?as_of_date=invalid-date&cusip=12345678X")
    
    # Should return 422 validation error
    assert response.status_code == 422


@patch('src.main.get_cosmos_client')
def test_historical_sentiment_endpoint_no_events_found(mock_get_cosmos_client, client):
    """Test the historical sentiment endpoint when no events are found."""
    # Setup mock to return empty list
    mock_cosmos_client = Mock()
    mock_cosmos_client.query_items.return_value = []
    mock_get_cosmos_client.return_value = mock_cosmos_client
    
    # Make request
    response = client.get("/sentiment/historical?as_of_date=2024-01-15&cusip=NOTFOUND1")
    
    # Verify response
    assert response.status_code == 200
    response_data = response.json()
    
    assert response_data["aggregated_sentiment_score"] == 0.0
    assert response_data["contributing_articles_count"] == 0


def test_health_endpoint(client):
    """Test the health check endpoint."""
    response = client.get("/health")
    
    assert response.status_code == 200
    response_data = response.json()
    assert response_data == {"status": "ok"}


@patch('src.main.get_cosmos_client')
def test_cosmos_db_query_construction_realtime(mock_get_cosmos_client, client):
    """Test that Cosmos DB queries are properly constructed for realtime endpoint."""
    # Setup mock
    mock_cosmos_client = Mock()
    mock_cosmos_client.query_items.return_value = []
    mock_get_cosmos_client.return_value = mock_cosmos_client
    
    # Make request
    client.get("/sentiment/realtime?cusip=12345678X")
    
    # Verify that query_items was called with the right parameters
    mock_cosmos_client.query_items.assert_called_once()
    call_args = mock_cosmos_client.query_items.call_args
    query = call_args[0][0]
    assert "SELECT * FROM c" in query
    assert "WHERE" in query


@patch('src.main.get_cosmos_client')
def test_cosmos_db_query_construction_historical(mock_get_cosmos_client, client):
    """Test that Cosmos DB queries are properly constructed for historical endpoint."""
    # Setup mock
    mock_cosmos_client = Mock()
    mock_cosmos_client.query_items.return_value = []
    mock_get_cosmos_client.return_value = mock_cosmos_client
    
    # Make request
    client.get("/sentiment/historical?as_of_date=2024-01-15&cusip=12345678X")
    
    # Verify that query_items was called with the right parameters
    mock_cosmos_client.query_items.assert_called_once()
    call_args = mock_cosmos_client.query_items.call_args
    query = call_args[0][0]
    assert "SELECT * FROM c" in query
    assert "WHERE" in query
    assert "@end_of_date" in query 