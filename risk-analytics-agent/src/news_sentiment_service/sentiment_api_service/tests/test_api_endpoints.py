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
    # Updated to expect direct date values instead of parameterized queries
    assert "2024-01-15T23:59:59Z" in query

@patch('src.main.get_cosmos_client')
def test_query_always_includes_global_market_realtime(mock_get_cosmos_client, client):
    """Test that global_market sector events are always included in realtime queries."""
    # Setup mock
    mock_cosmos_client = Mock()
    mock_cosmos_client.query_items.return_value = []
    mock_get_cosmos_client.return_value = mock_cosmos_client
    
    # Make request with specific cusip
    client.get("/sentiment/realtime?cusip=12345678X")
    
    # Verify query includes global_market condition
    mock_cosmos_client.query_items.assert_called_once()
    call_args = mock_cosmos_client.query_items.call_args
    query = call_args[0][0]
    
    # Should include global_market sector condition
    assert "c.entities.sector = 'global_market'" in query or "c.entities.sector = @global_market_sector" in query


@patch('src.main.get_cosmos_client')
def test_query_always_includes_global_market_historical(mock_get_cosmos_client, client):
    """Test that global_market sector events are always included in historical queries."""
    # Setup mock
    mock_cosmos_client = Mock()
    mock_cosmos_client.query_items.return_value = []
    mock_get_cosmos_client.return_value = mock_cosmos_client
    
    # Make request with specific sector
    client.get("/sentiment/historical?as_of_date=2024-01-15&sector=Technology")
    
    # Verify query includes global_market condition
    mock_cosmos_client.query_items.assert_called_once()
    call_args = mock_cosmos_client.query_items.call_args
    query = call_args[0][0]
    
    # Should include global_market sector condition
    assert "c.entities.sector = 'global_market'" in query or "c.entities.sector = @global_market_sector" in query


@patch('src.main.get_cosmos_client')
def test_query_uses_or_logic_multiple_parameters(mock_get_cosmos_client, client):
    """Test that query uses OR logic when multiple parameters are provided."""
    # Setup mock
    mock_cosmos_client = Mock()
    mock_cosmos_client.query_items.return_value = []
    mock_get_cosmos_client.return_value = mock_cosmos_client
    
    # Make request with multiple parameters
    client.get("/sentiment/realtime?cusip=12345678X&sector=Technology&issuer_name=ABC%20Corp")
    
    # Verify query uses OR logic instead of AND
    mock_cosmos_client.query_items.assert_called_once()
    call_args = mock_cosmos_client.query_items.call_args
    query = call_args[0][0]
    
    # Should contain OR conditions for entity parameters
    # Should NOT contain AND between cusip, sector, and issuer_name
    and_count_between_entities = 0
    or_count_between_entities = 0
    
    # Check for OR logic patterns (updated for direct query format)
    if "ARRAY_CONTAINS(c.entities.cusips, '12345678X') OR" in query:
        or_count_between_entities += 1
    if "c.entities.sector = 'Technology' OR" in query:
        or_count_between_entities += 1
    if "OR c.entities.issuer_name = 'ABC Corp'" in query:
        or_count_between_entities += 1
        
    # Check for AND logic patterns (should not exist between entities)
    if "ARRAY_CONTAINS(c.entities.cusips, '12345678X') AND c.entities.sector" in query:
        and_count_between_entities += 1
    if "c.entities.sector = 'Technology' AND c.entities.issuer_name" in query:
        and_count_between_entities += 1
    
    # Should use OR logic, not AND logic between entity conditions
    assert or_count_between_entities > 0, f"Expected OR logic in query: {query}"
    assert and_count_between_entities == 0, f"Unexpected AND logic between entities in query: {query}"


@patch('src.main.get_cosmos_client')
def test_query_uses_or_logic_two_parameters(mock_get_cosmos_client, client):
    """Test that query uses OR logic when two parameters are provided."""
    # Setup mock
    mock_cosmos_client = Mock()
    mock_cosmos_client.query_items.return_value = []
    mock_get_cosmos_client.return_value = mock_cosmos_client
    
    # Make request with cusip and sector
    client.get("/sentiment/realtime?cusip=12345678X&sector=Technology")
    
    # Verify query uses OR logic
    mock_cosmos_client.query_items.assert_called_once()
    call_args = mock_cosmos_client.query_items.call_args
    query = call_args[0][0]
    
    # Should contain OR between cusip and sector, not AND (updated for direct query format)
    cusip_and_sector_and = "ARRAY_CONTAINS(c.entities.cusips, '12345678X') AND c.entities.sector = 'Technology'" in query
    cusip_or_sector = ("ARRAY_CONTAINS(c.entities.cusips, '12345678X') OR c.entities.sector = 'Technology'" in query or 
                       "c.entities.sector = 'Technology' OR ARRAY_CONTAINS(c.entities.cusips, '12345678X')" in query)
    
    assert not cusip_and_sector_and, f"Found unexpected AND logic in query: {query}"
    assert cusip_or_sector, f"Expected OR logic between cusip and sector in query: {query}"


@patch('src.main.get_cosmos_client')
@patch('src.main.logger')
def test_info_logging_in_realtime_endpoint(mock_logger, mock_get_cosmos_client, client):
    """Test that info logging is implemented throughout the realtime endpoint."""
    # Setup mocks
    mock_cosmos_client = Mock()
    mock_cosmos_client.query_items.return_value = []
    mock_get_cosmos_client.return_value = mock_cosmos_client
    
    # Make request
    client.get("/sentiment/realtime?cusip=12345678X")
    
    # Verify that info logging was called
    # Should have calls for: parameter validation, query building, data fetching, sentiment calculation, response generation
    info_calls = [call for call in mock_logger.info.call_args_list]
    assert len(info_calls) >= 3, f"Expected at least 3 info log calls, got {len(info_calls)}"


@patch('src.main.get_cosmos_client')
@patch('src.main.logger')
def test_info_logging_in_historical_endpoint(mock_logger, mock_get_cosmos_client, client):
    """Test that info logging is implemented throughout the historical endpoint."""
    # Setup mocks
    mock_cosmos_client = Mock()
    mock_cosmos_client.query_items.return_value = []
    mock_get_cosmos_client.return_value = mock_cosmos_client
    
    # Make request
    client.get("/sentiment/historical?as_of_date=2024-01-15&sector=Technology")
    
    # Verify that info logging was called
    info_calls = [call for call in mock_logger.info.call_args_list]
    assert len(info_calls) >= 3, f"Expected at least 3 info log calls, got {len(info_calls)}"


@patch('src.main.get_cosmos_client')
def test_global_market_events_contribute_to_sentiment_score(mock_get_cosmos_client, client):
    """Test that global_market events are included in sentiment score calculation."""
    # Setup mock with both specific and global market events
    global_market_event = {
        "id": "global_event1",
        "source": "Bloomberg",
        "published_at": "2024-01-15T14:00:00Z",
        "ingested_at": "2024-01-15T14:00:00Z",
        "event_type": "Federal_Reserve_Policy",
        "entities": {
            "issuer_name": "",
            "sector": "global_market",
            "state": None,
            "cusips": []
        },
        "sentiment": {
            "score": -0.5,
            "magnitude": 0.8
        },
        "source_credibility_tier": "TIER_1_REGULATOR",
        "summary_excerpt": "Fed raises rates",
        "raw_article_url": "https://example.com/global1"
    }
    
    specific_event = {
        "id": "specific_event1",
        "source": "Reuters",
        "published_at": "2024-01-15T13:00:00Z",
        "ingested_at": "2024-01-15T13:00:00Z",
        "event_type": "Earnings_Beat",
        "entities": {
            "issuer_name": "ABC Corp",
            "sector": "Technology",
            "state": None,
            "cusips": ["12345678X"]
        },
        "sentiment": {
            "score": 0.7,
            "magnitude": 0.6
        },
        "source_credibility_tier": "TIER_2_PREMIUM_FINANCIAL",
        "summary_excerpt": "ABC Corp beats earnings",
        "raw_article_url": "https://example.com/specific1"
    }
    
    mock_cosmos_client = Mock()
    mock_cosmos_client.query_items.return_value = [global_market_event, specific_event]
    mock_get_cosmos_client.return_value = mock_cosmos_client
    
    # Make request for specific entity
    response = client.get("/sentiment/realtime?cusip=12345678X")
    
    # Verify successful response
    assert response.status_code == 200
    response_data = response.json()
    
    # Should have 2 contributing articles (global + specific)
    assert response_data["contributing_articles_count"] == 2
    
    # Should have both article URLs
    assert len(response_data["articles"]) == 2
    assert "https://example.com/global1" in response_data["articles"]
    assert "https://example.com/specific1" in response_data["articles"]


def test_build_cosmos_query_function_includes_global_market():
    """Test that _build_cosmos_query function properly includes global_market conditions."""
    from src.main import _build_cosmos_query
    
    # Test with cusip parameter
    query, params = _build_cosmos_query("12345678X", None, None)
    
    # Should include global_market sector condition
    assert "global_market" in query.lower()
    
    # Test with sector parameter  
    query, params = _build_cosmos_query(None, "Technology", None)
    
    # Should include both requested sector and global_market
    assert "global_market" in query.lower()
    
    # Test with issuer_name parameter
    query, params = _build_cosmos_query(None, None, "ABC Corp")
    
    # Should include global_market sector condition
    assert "global_market" in query.lower()


def test_sql_injection_protection():
    """Test that SQL injection attempts are properly escaped in direct queries."""
    from src.main import _build_cosmos_query
    
    # Test SQL injection in cusip
    malicious_cusip = "'; DROP TABLE c; --"
    query, params = _build_cosmos_query(malicious_cusip, None, None)
    
    # Should escape single quotes to prevent injection
    assert "''; DROP TABLE c; --'" in query
    assert "DROP TABLE c" not in query.replace("''; DROP TABLE c; --'", "")
    
    # Test SQL injection in sector
    malicious_sector = "Technology'; DELETE FROM c WHERE 1=1; --"
    query, params = _build_cosmos_query(None, malicious_sector, None)
    
    # Should escape single quotes
    assert "Technology''; DELETE FROM c WHERE 1=1; --'" in query
    
    # Test SQL injection in issuer_name
    malicious_issuer = "ABC'; UPDATE c SET sentiment = null; --"
    query, params = _build_cosmos_query(None, None, malicious_issuer)
    
    # Should escape single quotes
    assert "ABC''; UPDATE c SET sentiment = null; --'" in query 