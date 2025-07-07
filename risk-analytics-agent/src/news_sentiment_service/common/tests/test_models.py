from datetime import datetime
import pytest
from pydantic import ValidationError

# This import will fail until Task 1.1 is implemented
from models.models import (
    RawNewsArticle,
    EnrichedNewsEvent,
    RealTimeSentimentResponse,
    HistoricalSentimentResponse
)

# Test data for successful creation
RAW_ARTICLE_DATA = {
    "article_text": "Some news content.",
    "source_name": "Test News",
    "publication_time": datetime.now(),
    "title": "A Test Title",
    "url": "http://example.com/test",
    "article_hash": "some_hash_value",
}

ENRICHED_EVENT_DATA = {
    "id": "event_123",
    "source": "Test News",
    "published_at": datetime.now(),
    "ingested_at": datetime.now(),
    "event_type": "General_News",
    "entities": {
        "issuer_name": "Test Corp",
        "sector": "Technology",
        "state": "CA",
        "cusips": ["12345ABCDE"]
    },
    "sentiment": {
        "score": 0.5,
        "magnitude": 0.8
    },
    "source_credibility_tier": "TIER_2_PREMIUM_FINANCIAL",
    "summary_excerpt": "A brief summary.",
    "raw_article_url": "http://example.com/test"
}


def test_raw_news_article_successful_creation():
    """Tests that RawNewsArticle can be created with valid data."""
    article = RawNewsArticle(**RAW_ARTICLE_DATA)
    assert article.title == RAW_ARTICLE_DATA["title"]
    assert article.article_hash == RAW_ARTICLE_DATA["article_hash"]

def test_raw_news_article_missing_field_fails():
    """Tests that RawNewsArticle creation fails if a required field is missing."""
    invalid_data = RAW_ARTICLE_DATA.copy()
    del invalid_data["title"]
    with pytest.raises(ValidationError):
        RawNewsArticle(**invalid_data)

def test_enriched_news_event_successful_creation():
    """Tests that EnrichedNewsEvent can be created with valid nested data."""
    event = EnrichedNewsEvent(**ENRICHED_EVENT_DATA)
    assert event.id == ENRICHED_EVENT_DATA["id"]
    assert event.entities.issuer_name == "Test Corp"
    assert event.sentiment.score == 0.5

def test_enriched_news_event_nullable_field():
    """Tests that a nullable field in EnrichedNewsEvent can be None."""
    data = ENRICHED_EVENT_DATA.copy()
    data["entities"]["state"] = None
    event = EnrichedNewsEvent(**data)
    assert event.entities.state is None


def test_enriched_news_event_missing_nested_field_fails():
    """Tests that EnrichedNewsEvent creation fails if a required nested field is missing."""
    invalid_data = ENRICHED_EVENT_DATA.copy()
    del invalid_data["sentiment"]["score"]
    with pytest.raises(ValidationError):
        EnrichedNewsEvent(**invalid_data)

def test_real_time_sentiment_response_successful_creation():
    """Tests that RealTimeSentimentResponse can be created with valid data."""
    data = {
        "aggregated_sentiment_score": 0.75,
        "contributing_articles_count": 5,
        "articles": ["http://a.com/1", "http://b.com/2"]
    }
    response = RealTimeSentimentResponse(**data)
    assert response.aggregated_sentiment_score == 0.75
    assert len(response.articles) == 2

def test_historical_sentiment_response_successful_creation():
    """Tests that HistoricalSentimentResponse can be created with valid data."""
    data = {
        "aggregated_sentiment_score": -0.25,
        "contributing_articles_count": 10
    }
    response = HistoricalSentimentResponse(**data)
    assert response.aggregated_sentiment_score == -0.25
    assert response.contributing_articles_count == 10 