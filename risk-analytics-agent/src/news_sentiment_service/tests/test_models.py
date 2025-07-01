import sys
from pathlib import Path

root = Path(__file__).resolve().parents[4]  # project root (playground)
service_src = root / "risk-analytics-agent" / "src"
if str(service_src) not in sys.path:
    sys.path.insert(0, str(service_src))

import pytest
from datetime import datetime
from uuid import UUID

from news_sentiment_service.src.common.models import RawNewsArticle, EnrichedNewsEvent  # type: ignore


def test_raw_news_article_basic_instantiation():
    article = RawNewsArticle(
        article_text="Example body",
        source_name="Example Source",
        publication_time=datetime.utcnow(),
        title="Example Title",
        url="https://example.com/article",
        article_hash="deadbeef",
    )
    assert article.article_text == "Example body"
    assert article.source_name == "Example Source"
    assert isinstance(article.publication_time, datetime)
    assert article.title == "Example Title"
    assert article.url.startswith("https://")
    assert article.article_hash == "deadbeef"


@pytest.mark.parametrize(
    "missing_field",
    [
        "article_text",
        "source_name",
        "publication_time",
        "title",
        "url",
        "article_hash",
    ],
)
def test_raw_news_article_missing_required_fields(missing_field):
    valid_data = {
        "article_text": "body",
        "source_name": "src",
        "publication_time": datetime.utcnow(),
        "title": "title",
        "url": "https://example.com",
        "article_hash": "hash",
    }
    valid_data.pop(missing_field)
    with pytest.raises(Exception):
        RawNewsArticle(**valid_data)


def test_enriched_news_event_basic_instantiation():
    event = EnrichedNewsEvent(
        source="Example Source",
        published_at=datetime.utcnow(),
        ingested_at=datetime.utcnow(),
        event_type="General_News",
        entities={
            "issuer_name": "Issuer X",
            "sector": "Utilities",
            "state": None,
            "cusips": ["123456AB9"],
        },
        sentiment={
            "score": 0.1,
            "magnitude": 0.5,
        },
        source_credibility_tier="TIER_3_GENERAL_FINANCIAL",
        summary_excerpt="An example summary.",
        raw_article_url="https://example.com/article",
    )
    # Ensure UUID id auto-generated
    assert isinstance(UUID(str(event.id)), UUID)
    # Round-trip JSON serialization
    as_dict = event.dict()
    reconstructed = EnrichedNewsEvent(**as_dict)
    assert reconstructed == event


@pytest.mark.parametrize(
    "bad_score",
    [-1.1, 1.1],
)
def test_enriched_news_event_score_bounds(bad_score):
    kwargs = dict(
        source="s",
        published_at=datetime.utcnow(),
        ingested_at=datetime.utcnow(),
        event_type="General_News",
        entities={
            "issuer_name": None,
            "sector": "global_other",
            "state": None,
            "cusips": [],
        },
        sentiment={
            "score": bad_score,
            "magnitude": 0.5,
        },
        source_credibility_tier="TIER_5_SYNDICATED",
        summary_excerpt="summary",
        raw_article_url="https://example.com",
    )
    with pytest.raises(Exception):
        EnrichedNewsEvent(**kwargs) 