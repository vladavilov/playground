from datetime import datetime, timezone
from unittest.mock import AsyncMock, MagicMock, patch
import hashlib

import pytest
from httpx import Response

from main import run_ingestion_cycle
from models.models import RawNewsArticle


@pytest.fixture
def mock_settings():
    """Fixture for mock settings."""
    settings = MagicMock()
    settings.adapter_urls_list = ["http://mock-adapter-1/news", "http://mock-adapter-2/news"]
    return settings


@pytest.fixture
def mock_service_bus_client():
    """Fixture for mock ServiceBusClient."""
    client = MagicMock()
    client.send_message = MagicMock()
    return client


def create_mock_article(unique_id: int) -> RawNewsArticle:
    """Helper to create a mock news article."""
    return RawNewsArticle(
        article_text=f"This is article {unique_id}.",
        source_name="MockSource",
        publication_time=datetime.now(timezone.utc),
        title=f"Article {unique_id}",
        url=f"http://mock.com/{unique_id}",
        article_hash=f"hash_{unique_id}"
    )


@pytest.mark.asyncio
@patch("main.httpx.AsyncClient")
async def test_run_ingestion_cycle_success(
    mock_async_client, mock_settings, mock_service_bus_client
):
    """
    Tests a successful ingestion cycle where articles are fetched from adapters and enqueued.
    No duplicate checking or Cosmos DB operations should occur.
    """
    mock_article = create_mock_article(1)
    success_response = Response(
        200,
        json=[mock_article.model_dump_json()],
        request=MagicMock()
    )
    empty_response = Response(200, json=[], request=MagicMock())
    
    mock_async_client.return_value.__aenter__.return_value.get.side_effect = [
        success_response,
        empty_response
    ]

    await run_ingestion_cycle(mock_settings, mock_service_bus_client)

    # Verify httpx was called for each adapter
    assert mock_async_client.return_value.__aenter__.return_value.get.call_count == len(mock_settings.adapter_urls_list)

    # Verify message was sent for the article (no duplicate checking)
    mock_service_bus_client.send_message.assert_called_once()
    sent_message = mock_service_bus_client.send_message.call_args[0][0]
    
    # Verify the article hash is calculated and included
    expected_hash = hashlib.sha256(f"{mock_article.title}:{mock_article.source_name}".encode('utf-8')).hexdigest()
    assert expected_hash in sent_message


@pytest.mark.asyncio
@patch("main.httpx.AsyncClient")
async def test_run_ingestion_cycle_multiple_articles(
    mock_async_client, mock_settings, mock_service_bus_client
):
    """
    Tests that multiple articles from the same adapter are all enqueued without duplicate checking.
    """
    mock_article_1 = create_mock_article(1)
    mock_article_2 = create_mock_article(2)
    mock_response = Response(
        200, 
        json=[mock_article_1.model_dump_json(), mock_article_2.model_dump_json()],
        request=MagicMock()
    )
    empty_response = Response(200, json=[], request=MagicMock())
    
    mock_async_client.return_value.__aenter__.return_value.get.side_effect = [
        mock_response,
        empty_response
    ]

    await run_ingestion_cycle(mock_settings, mock_service_bus_client)

    # Verify httpx was called for each adapter
    assert mock_async_client.return_value.__aenter__.return_value.get.call_count == len(mock_settings.adapter_urls_list)

    # Verify both articles were enqueued
    assert mock_service_bus_client.send_message.call_count == 2


@pytest.mark.asyncio
@patch("main.httpx.AsyncClient")
async def test_run_ingestion_cycle_adapter_failure(
    mock_async_client, mock_settings, mock_service_bus_client
):
    """
    Tests that the cycle continues even if one adapter fails.
    """
    # First adapter fails, second succeeds
    mock_article = create_mock_article(3)
    success_response = Response(200, json=[mock_article.model_dump_json()], request=MagicMock())
    failure_response = Response(500, request=MagicMock())
    
    mock_async_client.return_value.__aenter__.return_value.get.side_effect = [
        failure_response,
        success_response
    ]

    await run_ingestion_cycle(mock_settings, mock_service_bus_client)

    # Verify httpx was called for both adapters
    assert mock_async_client.return_value.__aenter__.return_value.get.call_count == 2

    # Verify that the article from the successful adapter was still processed
    mock_service_bus_client.send_message.assert_called_once()


@pytest.mark.asyncio
@patch("main.httpx.AsyncClient")
async def test_run_ingestion_cycle_all_adapters_empty(
    mock_async_client, mock_settings, mock_service_bus_client
):
    """
    Tests that no messages are sent when all adapters return empty responses.
    """
    empty_response = Response(200, json=[], request=MagicMock())
    
    mock_async_client.return_value.__aenter__.return_value.get.return_value = empty_response

    await run_ingestion_cycle(mock_settings, mock_service_bus_client)

    # Verify httpx was called for each adapter
    assert mock_async_client.return_value.__aenter__.return_value.get.call_count == len(mock_settings.adapter_urls_list)

    # Verify no messages were sent
    mock_service_bus_client.send_message.assert_not_called()


@pytest.mark.asyncio
@patch("main.httpx.AsyncClient")
async def test_run_ingestion_cycle_request_timeout(
    mock_async_client, mock_settings, mock_service_bus_client
):
    """
    Tests that request timeouts are handled gracefully.
    """
    import httpx
    
    mock_async_client.return_value.__aenter__.return_value.get.side_effect = [
        httpx.TimeoutException("Request timed out"),
        Response(200, json=[], request=MagicMock())
    ]

    await run_ingestion_cycle(mock_settings, mock_service_bus_client)

    # Verify httpx was called for both adapters despite timeout
    assert mock_async_client.return_value.__aenter__.return_value.get.call_count == 2

    # Verify no messages were sent due to timeout and empty response
    mock_service_bus_client.send_message.assert_not_called()


@pytest.mark.asyncio
@patch("main.httpx.AsyncClient")
async def test_run_ingestion_cycle_invalid_json_response(
    mock_async_client, mock_settings, mock_service_bus_client
):
    """
    Tests that invalid JSON responses are handled gracefully.
    """
    mock_article = create_mock_article(1)
    
    # First adapter returns invalid JSON, second returns valid article
    invalid_response = Response(200, json="invalid_json_structure", request=MagicMock())
    valid_response = Response(200, json=[mock_article.model_dump_json()], request=MagicMock())
    
    mock_async_client.return_value.__aenter__.return_value.get.side_effect = [
        invalid_response,
        valid_response
    ]

    await run_ingestion_cycle(mock_settings, mock_service_bus_client)

    # Verify httpx was called for both adapters
    assert mock_async_client.return_value.__aenter__.return_value.get.call_count == 2

    # Verify only the valid article was processed
    mock_service_bus_client.send_message.assert_called_once()
