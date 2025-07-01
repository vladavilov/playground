"""Unit tests for DIS-Bazinga ingestion orchestration.

  • RTN-FR-01b – watermark handling.
  • RTN-FR-01a – deduplication via ``article_hash``.
  • RTN-FR-01  – queuing of unique articles to Service Bus.
"""

from __future__ import annotations

from datetime import datetime, timezone, timedelta
from typing import Iterable, Protocol

from news_sentiment_service.src.common.cosmos_db.cosmos_db_client import (
    InMemoryCosmosDBClient,
)
from news_sentiment_service.src.common.models import RawNewsArticle
from news_sentiment_service.src.common.service_bus.service_bus_client import (
    InMemoryServiceBusClient,
)
from news_sentiment_service.src.data_ingestion_bazinga.orchestrator import (
    run_ingestion_cycle,
)


class _StubProvider(Protocol):
    source_name: str

    def __call__(self, since: datetime | None) -> Iterable[RawNewsArticle]:
        ...


class StubBazingaAdapter:
    source_name = "bazinga"

    def __init__(self, articles: Iterable[RawNewsArticle]):
        self._articles = list(articles)

    def fetch_articles(self, since: datetime | None):  # noqa: D401
        if since is None:
            return self._articles
        return [a for a in self._articles if a.publication_time > since]


def _make_article(idx: int, published_offset: int = 0) -> RawNewsArticle:
    ts = datetime.now(tz=timezone.utc) + timedelta(minutes=published_offset)
    return RawNewsArticle(
        article_text=f"body-{idx}",
        source_name="bazinga",
        publication_time=ts,
        title=f"title-{idx}",
        url=f"https://example.com/{idx}",
        article_hash=f"hash-{idx}",
    )


def test_watermark_filters_old_articles():
    """Articles published before last watermark should be ignored (RTN-FR-01b)."""

    cosmos = InMemoryCosmosDBClient()
    past_ts = datetime.now(tz=timezone.utc)
    cosmos._last_ts_by_source["bazinga"] = past_ts  # type: ignore[attr-defined]

    old_article = _make_article(1, published_offset=-10)
    new_article = _make_article(2, published_offset=+10)
    adapter = StubBazingaAdapter([old_article, new_article])

    bus = InMemoryServiceBusClient()

    run_ingestion_cycle(
        cosmos_client=cosmos, service_bus=bus, provider_adapter=adapter
    )

    msgs = bus.receive_messages(10)
    assert len(msgs) == 1
    assert msgs[0]["article_hash"] == new_article.article_hash


def test_deduplication_skips_existing_hash():
    """Duplicate articles should not be queued (RTN-FR-01a)."""

    cosmos = InMemoryCosmosDBClient()
    cosmos._events_by_hash["hash-1"] = object()  # type: ignore[index]

    dup_article = _make_article(1)
    unique_article = _make_article(2)
    adapter = StubBazingaAdapter([dup_article, unique_article])

    bus = InMemoryServiceBusClient()

    run_ingestion_cycle(cosmos_client=cosmos, service_bus=bus, provider_adapter=adapter)

    msgs = bus.receive_messages(10)
    assert len(msgs) == 1
    assert msgs[0]["article_hash"] == unique_article.article_hash


def test_all_unique_articles_are_queued():
    """All unique articles should be enqueued (RTN-FR-01)."""

    articles = [_make_article(i) for i in range(3)]
    adapter = StubBazingaAdapter(articles)
    bus = InMemoryServiceBusClient()

    run_ingestion_cycle(service_bus=bus, provider_adapter=adapter)

    msgs = bus.receive_messages(10)
    assert len(msgs) == 3
    hashes = {msg["article_hash"] for msg in msgs}
    assert hashes == {a.article_hash for a in articles}
