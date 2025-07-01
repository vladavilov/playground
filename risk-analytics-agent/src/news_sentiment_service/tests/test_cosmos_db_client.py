import sys
from pathlib import Path
from datetime import datetime, timedelta

import pytest

root = Path(__file__).resolve().parents[4]
service_src = root / "risk-analytics-agent" / "src"
if str(service_src) not in sys.path:
    sys.path.insert(0, str(service_src))

from news_sentiment_service.src.common.models import EnrichedNewsEvent  # type: ignore
from news_sentiment_service.src.common.cosmos_db import InMemoryCosmosDBClient  # type: ignore


def _sample_event(**overrides):
    now = datetime.utcnow()
    base_kwargs = dict(
        source="Bloomberg",
        published_at=now - timedelta(minutes=5),
        ingested_at=now,
        event_type="General_News",
        entities={
            "issuer_name": "Issuer",
            "sector": "Utilities",
            "state": None,
            "cusips": [],
        },
        sentiment={"score": 0.2, "magnitude": 0.3},
        source_credibility_tier="TIER_2_PREMIUM_FINANCIAL",
        summary_excerpt="excerpt",
        raw_article_url="https://example.com",
    )
    base_kwargs.update(overrides)
    return EnrichedNewsEvent(**base_kwargs)


def test_insert_and_hash_check():
    client = InMemoryCosmosDBClient()
    event = _sample_event()
    assert client.article_hash_exists(event.id) is False  # using id as hash surrogate
    client.insert_enriched_news_event(event)
    assert client.article_hash_exists(event.id) is True


def test_get_last_ingested_timestamp():
    client = InMemoryCosmosDBClient()
    t1 = datetime.utcnow() - timedelta(hours=1)
    t2 = datetime.utcnow()
    ev1 = _sample_event(source="Reuters", published_at=t1 - timedelta(minutes=1), ingested_at=t1)
    ev2 = _sample_event(source="Reuters", published_at=t2 - timedelta(minutes=1), ingested_at=t2)
    client.insert_enriched_news_event(ev1)
    client.insert_enriched_news_event(ev2)
    ts = client.get_last_ingested_timestamp("Reuters")
    assert ts == ev2.ingested_at


def test_last_ingested_unknown_source():
    client = InMemoryCosmosDBClient()
    assert client.get_last_ingested_timestamp("Nonexistent") is None 