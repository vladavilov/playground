import pytest
from fastapi.testclient import TestClient
from unittest.mock import patch, MagicMock, AsyncMock
import httpx
from datetime import datetime, timezone
import hashlib

from main import app, strip_html
from models.models import RawNewsArticle

client = TestClient(app)

MOCK_BENZINGA_RESPONSE = [
    {
        "updated": "Tue, 01 Jan 2024 12:00:00 -0000",
        "title": "Major Muni Bond Update",
        "body": "<p>The market saw activity.</p>",
        "url": "https://www.benzinga.com/news/12345/major-muni-bond-update",
        "id": "12345"
    }
]

MOCK_BENZINGA_RESPONSE_MULTIPLE = [
    {
        "updated": "Mon, 15 Jan 2024 10:00:00 -0000",
        "title": "Article 1: Before Range",
        "body": "Body 1",
        "url": "http://test.com/1",
        "id": "1"
    },
    {
        "updated": "Mon, 15 Jan 2024 12:00:00 -0000",
        "title": "Article 2: In Range",
        "body": "Body 2",
        "url": "http://test.com/2",
        "id": "2"
    },
    {
        "updated": "Mon, 15 Jan 2024 14:00:00 -0000",
        "title": "Article 3: In Range",
        "body": "Body 3",
        "url": "http://test.com/3",
        "id": "3"
    },
    {
        "updated": "Mon, 15 Jan 2024 16:00:00 -0000",
        "title": "Article 4: After Range",
        "body": "Body 4",
        "url": "http://test.com/4",
        "id": "4"
    }
]

@pytest.fixture
def mock_httpx_client():
    """
    Mocks the httpx.AsyncClient used by the main application to call the
    external Benzinga API.
    """
    with patch("main.httpx.AsyncClient") as mock_client_class:
        # This is the mock for the response from Benzinga
        mock_response = MagicMock()
        mock_response.status_code = 200
        mock_response.json.return_value = MOCK_BENZINGA_RESPONSE
        mock_response.headers = {"content-type": "application/json"}

        # This is the mock for the client instance
        mock_instance = MagicMock()
        # The `get` method must be an AsyncMock to be awaitable
        mock_instance.get = AsyncMock(return_value=mock_response)
        
        # When the class is used as a context manager (`async with ...`),
        # its __aenter__ should return our instance.
        mock_client_class.return_value.__aenter__.return_value = mock_instance
        yield mock_instance

def test_get_news_with_date_filters(mock_httpx_client):
    """
    Tests that the /news endpoint correctly filters articles based on
    `dateFrom` and `dateTo` query parameters.
    """
    # Configure the mock response to return a list of articles
    mock_response = MagicMock()
    mock_response.status_code = 200
    mock_response.json.return_value = MOCK_BENZINGA_RESPONSE_MULTIPLE
    mock_response.headers = {"content-type": "application/json"}
    mock_httpx_client.get.return_value = mock_response

    # Define the time window for the test
    date_from = "2024-01-15T11:00:00"
    date_to = "2024-01-15T15:00:00"

    with patch("main.settings") as mock_settings:
        mock_settings.BENZINGA_API_TOKEN = "test_token"

        response = client.get(f"/news?dateFrom={date_from}&dateTo={date_to}")
    
    # Assertions
    assert response.status_code == 200
    response_data = response.json()
    
    assert len(response_data) == 2
    assert response_data[0]["title"] == "Article 2: In Range"
    assert response_data[1]["title"] == "Article 3: In Range"
    
    # Verify that the Benzinga API was called with the correct `updatedSince`
    mock_httpx_client.get.assert_awaited_once()
    call_args, call_kwargs = mock_httpx_client.get.call_args
    
    # `dateFrom` should be converted to a UTC timestamp for the `updatedSince` param
    expected_timestamp = int(datetime.fromisoformat(date_from).replace(tzinfo=timezone.utc).timestamp())
    assert call_kwargs["params"]["updatedSince"] == expected_timestamp

def test_get_news_success(mock_httpx_client):
    """
    Tests the /news endpoint for a successful transformation.
    This is a synchronous test that calls an async endpoint.
    """
    with patch("main.settings") as mock_settings:
        mock_settings.BENZINGA_API_TOKEN = "test_token"

        response = client.get("/news")

    assert response.status_code == 200
    
    mock_httpx_client.get.assert_awaited_once()
    call_args, call_kwargs = mock_httpx_client.get.call_args
    assert call_args[0] == "https://api.benzinga.com/api/v2/news"
    assert call_kwargs["params"]["token"] == "test_token"
    
    response_data = response.json()
    assert len(response_data) == 1
    
    article = RawNewsArticle(**response_data[0])
    assert article.source_name == "Benzinga"
    assert article.title == MOCK_BENZINGA_RESPONSE[0]["title"]
    assert article.article_text == "The market saw activity."
    
    # Verify hash calculation
    expected_hash = hashlib.md5((article.title + article.article_text).encode()).hexdigest()
    assert article.article_hash == expected_hash

def test_get_news_api_connection_error(mock_httpx_client):
    """
    Tests that the /news endpoint handles an httpx.RequestError gracefully.
    """
    mock_httpx_client.get.side_effect = httpx.RequestError("Test connection error")

    with patch("main.settings") as mock_settings:
        mock_settings.BENZINGA_API_TOKEN = "test_token"

        response = client.get("/news")

    assert response.status_code == 503
    assert "Error connecting to Benzinga API" in response.json()["detail"]

def test_get_news_http_status_error(mock_httpx_client):
    """
    Tests that the /news endpoint handles httpx.HTTPStatusError gracefully.
    """
    mock_response = MagicMock()
    mock_response.status_code = 429
    mock_response.text = "Rate limit exceeded"
    
    mock_httpx_client.get.side_effect = httpx.HTTPStatusError(
        "Rate limit exceeded", 
        request=MagicMock(), 
        response=mock_response
    )

    with patch("main.settings") as mock_settings:
        mock_settings.BENZINGA_API_TOKEN = "test_token"

        response = client.get("/news")

    assert response.status_code == 502
    assert "Benzinga API error: 429" in response.json()["detail"]

def test_get_news_empty_response(mock_httpx_client):
    """
    Tests the /news endpoint when the external API returns an empty list.
    """
    mock_response = MagicMock()
    mock_response.status_code = 200
    mock_response.json.return_value = []
    mock_response.headers = {"content-type": "application/json"}
    mock_httpx_client.get.return_value = mock_response

    with patch("main.settings") as mock_settings:
        mock_settings.BENZINGA_API_TOKEN = "test_token"

        response = client.get("/news")

    assert response.status_code == 200
    assert response.json() == []

def test_get_news_article_missing_updated_field(mock_httpx_client):
    """
    Tests that articles missing the 'updated' field are skipped.
    """
    mock_response_data = [
        {
            "title": "Article with updated field",
            "body": "Content 1",
            "url": "http://test.com/1",
            "updated": "Tue, 01 Jan 2024 12:00:00 -0000",
            "id": "1"
        },
        {
            "title": "Article without updated field",
            "body": "Content 2",
            "url": "http://test.com/2",
            "id": "2"
            # Missing 'updated' field
        }
    ]
    
    mock_response = MagicMock()
    mock_response.status_code = 200
    mock_response.json.return_value = mock_response_data
    mock_response.headers = {"content-type": "application/json"}
    mock_httpx_client.get.return_value = mock_response

    with patch("main.settings") as mock_settings:
        mock_settings.BENZINGA_API_TOKEN = "test_token"

        response = client.get("/news")

    assert response.status_code == 200
    response_data = response.json()
    
    # Should only return the article with the 'updated' field
    assert len(response_data) == 1
    assert response_data[0]["title"] == "Article with updated field"

def test_get_news_invalid_date_format(mock_httpx_client):
    """
    Tests that articles with invalid date formats are skipped.
    """
    mock_response_data = [
        {
            "title": "Article with valid date",
            "body": "Content 1",
            "url": "http://test.com/1",
            "updated": "Tue, 01 Jan 2024 12:00:00 -0000",
            "id": "1"
        },
        {
            "title": "Article with invalid date",
            "body": "Content 2",
            "url": "http://test.com/2",
            "updated": "invalid-date-format",
            "id": "2"
        }
    ]
    
    mock_response = MagicMock()
    mock_response.status_code = 200
    mock_response.json.return_value = mock_response_data
    mock_response.headers = {"content-type": "application/json"}
    mock_httpx_client.get.return_value = mock_response

    with patch("main.settings") as mock_settings:
        mock_settings.BENZINGA_API_TOKEN = "test_token"

        response = client.get("/news")

    assert response.status_code == 200
    response_data = response.json()
    
    # Should only return the article with the valid date
    assert len(response_data) == 1
    assert response_data[0]["title"] == "Article with valid date"

def test_get_news_timezone_handling_naive_datetime(mock_httpx_client):
    """
    Tests that naive datetimes in dateFrom/dateTo are correctly assumed to be UTC.
    """
    mock_response = MagicMock()
    mock_response.status_code = 200
    mock_response.json.return_value = MOCK_BENZINGA_RESPONSE
    mock_response.headers = {"content-type": "application/json"}
    mock_httpx_client.get.return_value = mock_response

    # Use naive datetime (no timezone info)
    date_from = "2024-01-01T10:00:00"  # Naive datetime
    
    with patch("main.settings") as mock_settings:
        mock_settings.BENZINGA_API_TOKEN = "test_token"

        response = client.get(f"/news?dateFrom={date_from}")
    
    assert response.status_code == 200
    
    # Verify that the naive datetime was converted to UTC timestamp
    call_args, call_kwargs = mock_httpx_client.get.call_args
    expected_timestamp = int(datetime.fromisoformat(date_from).replace(tzinfo=timezone.utc).timestamp())
    assert call_kwargs["params"]["updatedSince"] == expected_timestamp

def test_get_news_teaser_fallback(mock_httpx_client):
    """
    Tests that teaser is used as fallback when body is empty.
    """
    mock_response_data = [
        {
            "title": "Article with body",
            "body": "<p>Full body content</p>",
            "url": "http://test.com/1",
            "updated": "Tue, 01 Jan 2024 12:00:00 -0000",
            "id": "1"
        },
        {
            "title": "Article with empty body",
            "body": "",
            "teaser": "This is the teaser content",
            "url": "http://test.com/2",
            "updated": "Tue, 01 Jan 2024 13:00:00 -0000",
            "id": "2"
        }
    ]
    
    mock_response = MagicMock()
    mock_response.status_code = 200
    mock_response.json.return_value = mock_response_data
    mock_response.headers = {"content-type": "application/json"}
    mock_httpx_client.get.return_value = mock_response

    with patch("main.settings") as mock_settings:
        mock_settings.BENZINGA_API_TOKEN = "test_token"

        response = client.get("/news")

    assert response.status_code == 200
    response_data = response.json()
    
    assert len(response_data) == 2
    assert response_data[0]["article_text"] == "Full body content"
    assert response_data[1]["article_text"] == "This is the teaser content"

def test_strip_html_function():
    """
    Tests the strip_html function directly.
    """
    # Test normal HTML stripping
    html_content = "<p>Hello <b>world</b>!</p>"
    result = strip_html(html_content)
    assert result == "Hello world!"
    
    # Test with nested HTML
    complex_html = "<div><p>Paragraph 1</p><ul><li>Item 1</li><li>Item 2</li></ul></div>"
    result = strip_html(complex_html)
    assert "Paragraph 1" in result
    assert "Item 1" in result
    assert "Item 2" in result
    assert "<" not in result
    assert ">" not in result

def test_strip_html_error_handling():
    """
    Tests that strip_html handles errors gracefully.
    """
    with patch("main.BeautifulSoup") as mock_bs:
        mock_bs.side_effect = Exception("BeautifulSoup error")
        
        # Should return original content on error
        result = strip_html("<p>Test content</p>")
        assert result == "<p>Test content</p>"

@patch("main.strip_html", side_effect=Exception("Unexpected internal error"))
def test_get_news_unexpected_error(mock_strip_html, mock_httpx_client):
    """
    Tests the /news endpoint's handling of individual article processing errors.
    The endpoint should log the error and continue processing other articles.
    """
    with patch("main.settings") as mock_settings:
        mock_settings.BENZINGA_API_TOKEN = "test_token"

        response = client.get("/news")

    # The endpoint should succeed even if individual articles fail to process
    assert response.status_code == 200
    
    # The response should be an empty list since the article failed to process
    response_data = response.json()
    assert response_data == []
   
def test_health_check():
    """
    Tests the /health endpoint.
    """
    response = client.get("/health")
    assert response.status_code == 200
    assert response.json() == {"status": "ok"}

# ============================================================================
# JSON RESPONSE VALIDATION TESTS
# ============================================================================

def test_get_news_empty_response_body(mock_httpx_client):
    """
    Tests that the /news endpoint handles empty response bodies gracefully.
    This should return a 502 Bad Gateway error with meaningful message.
    """
    mock_response = MagicMock()
    mock_response.status_code = 200
    mock_response.content = b""  # Empty response body
    mock_response.text = ""
    mock_response.headers = {"content-type": "application/json"}
    mock_response.json.side_effect = ValueError("Expecting value: line 1 column 1 (char 0)")
    mock_httpx_client.get.return_value = mock_response

    with patch("main.settings") as mock_settings:
        mock_settings.BENZINGA_API_TOKEN = "test_token"

        response = client.get("/news")

    assert response.status_code == 502
    assert "empty response body" in response.json()["detail"]

def test_get_news_html_error_response(mock_httpx_client):
    """
    Tests that the /news endpoint handles HTML error pages gracefully.
    This often happens when APIs return error pages instead of JSON.
    """
    mock_response = MagicMock()
    mock_response.status_code = 200
    mock_response.content = b"<html><body>Error: Service unavailable</body></html>"
    mock_response.text = "<html><body>Error: Service unavailable</body></html>"
    mock_response.headers = {"content-type": "text/html"}
    mock_response.json.side_effect = ValueError("Expecting value: line 1 column 1 (char 0)")
    mock_httpx_client.get.return_value = mock_response

    with patch("main.settings") as mock_settings:
        mock_settings.BENZINGA_API_TOKEN = "test_token"

        response = client.get("/news")

    assert response.status_code == 502
    assert "invalid JSON response" in response.json()["detail"]

def test_get_news_malformed_json_response(mock_httpx_client):
    """
    Tests that the /news endpoint handles malformed JSON gracefully.
    """
    mock_response = MagicMock()
    mock_response.status_code = 200
    mock_response.content = b'{"articles": [{"title": "Test", "body": "incomplete'
    mock_response.text = '{"articles": [{"title": "Test", "body": "incomplete'
    mock_response.headers = {"content-type": "application/json"}
    mock_response.json.side_effect = ValueError("Expecting ',' delimiter: line 1 column 45 (char 44)")
    mock_httpx_client.get.return_value = mock_response

    with patch("main.settings") as mock_settings:
        mock_settings.BENZINGA_API_TOKEN = "test_token"

        response = client.get("/news")

    assert response.status_code == 502
    assert "invalid JSON response" in response.json()["detail"]

def test_get_news_unexpected_json_structure(mock_httpx_client):
    """
    Tests that the /news endpoint handles unexpected JSON structures gracefully.
    E.g., when API returns an error object instead of articles array.
    """
    mock_response = MagicMock()
    mock_response.status_code = 200
    mock_response.content = b'{"error": "Rate limit exceeded", "code": 429}'
    mock_response.text = '{"error": "Rate limit exceeded", "code": 429}'
    mock_response.headers = {"content-type": "application/json"}
    mock_response.json.return_value = {"error": "Rate limit exceeded", "code": 429}
    mock_httpx_client.get.return_value = mock_response

    with patch("main.settings") as mock_settings:
        mock_settings.BENZINGA_API_TOKEN = "test_token"

        response = client.get("/news")

    assert response.status_code == 502
    assert "invalid response format" in response.json()["detail"]

def test_get_news_non_json_content_type(mock_httpx_client):
    """
    Tests that the /news endpoint handles non-JSON content types gracefully.
    """
    mock_response = MagicMock()
    mock_response.status_code = 200
    mock_response.content = b"Plain text error message"
    mock_response.text = "Plain text error message"
    mock_response.headers = {"content-type": "text/plain"}
    mock_response.json.side_effect = ValueError("Expecting value: line 1 column 1 (char 0)")
    mock_httpx_client.get.return_value = mock_response

    with patch("main.settings") as mock_settings:
        mock_settings.BENZINGA_API_TOKEN = "test_token"

        response = client.get("/news")

    assert response.status_code == 502
    assert "invalid JSON response" in response.json()["detail"]

def test_get_news_all_articles_fail_processing(mock_httpx_client):
    """
    Tests that the endpoint returns empty list when all articles fail to process.
    """
    mock_response_data = [
        {
            "title": "Article 1",
            "body": "Content 1",
            "url": "http://test.com/1",
            "updated": "invalid-date-1",
            "id": "1"
        },
        {
            "title": "Article 2",
            "body": "Content 2",
            "url": "http://test.com/2",
            "updated": "invalid-date-2",
            "id": "2"
        }
    ]
    
    mock_response = MagicMock()
    mock_response.status_code = 200
    mock_response.json.return_value = mock_response_data
    mock_response.headers = {"content-type": "application/json"}
    mock_httpx_client.get.return_value = mock_response

    with patch("main.settings") as mock_settings:
        mock_settings.BENZINGA_API_TOKEN = "test_token"

        response = client.get("/news")

    assert response.status_code == 200
    assert response.json() == []

def test_get_news_partial_article_processing_failure(mock_httpx_client):
    """
    Tests that the endpoint continues processing when some articles fail.
    """
    mock_response_data = [
        {
            "title": "Valid Article",
            "body": "Valid content",
            "url": "http://test.com/1",
            "updated": "Tue, 01 Jan 2024 12:00:00 -0000",
            "id": "1"
        },
        {
            "title": "Invalid Article",
            "body": "Invalid content",
            "url": "http://test.com/2",
            "updated": "invalid-date",
            "id": "2"
        },
        {
            "title": "Another Valid Article",
            "body": "More valid content",
            "url": "http://test.com/3",
            "updated": "Tue, 01 Jan 2024 13:00:00 -0000",
            "id": "3"
        }
    ]
    
    mock_response = MagicMock()
    mock_response.status_code = 200
    mock_response.json.return_value = mock_response_data
    mock_response.headers = {"content-type": "application/json"}
    mock_httpx_client.get.return_value = mock_response

    with patch("main.settings") as mock_settings:
        mock_settings.BENZINGA_API_TOKEN = "test_token"

        response = client.get("/news")

    assert response.status_code == 200
    response_data = response.json()
    
    # Should return only the valid articles
    assert len(response_data) == 2
    assert response_data[0]["title"] == "Valid Article"
    assert response_data[1]["title"] == "Another Valid Article"

def test_get_news_hash_calculation_consistency(mock_httpx_client):
    """
    Tests that article hash is calculated consistently.
    """
    mock_response_data = [
        {
            "title": "Test Article",
            "body": "<p>Test content</p>",
            "url": "http://test.com/1",
            "updated": "Tue, 01 Jan 2024 12:00:00 -0000",
            "id": "1"
        }
    ]
    
    mock_response = MagicMock()
    mock_response.status_code = 200
    mock_response.json.return_value = mock_response_data
    mock_response.headers = {"content-type": "application/json"}
    mock_httpx_client.get.return_value = mock_response

    with patch("main.settings") as mock_settings:
        mock_settings.BENZINGA_API_TOKEN = "test_token"

        response = client.get("/news")

    assert response.status_code == 200
    response_data = response.json()
    
    article = response_data[0]
    title = article["title"]
    article_text = article["article_text"]
    
    # Calculate expected hash
    expected_hash = hashlib.md5((title + article_text).encode()).hexdigest()
    assert article["article_hash"] == expected_hash 