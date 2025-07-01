from __future__ import annotations

import logging
from datetime import datetime, timezone
from typing import Iterable, List, NoReturn, Protocol

from ..common.cosmos_db.cosmos_db_client import CosmosDBClient, InMemoryCosmosDBClient
from ..common.models import RawNewsArticle
from ..common.service_bus.service_bus_client import (
    InMemoryServiceBusClient,
    ServiceBusClient,
)

logger = logging.getLogger(__name__)


class ProviderAdapter(Protocol):
    source_name: str

    def fetch_articles(self, since: datetime | None) -> Iterable[RawNewsArticle]:
        """Return an iterable of articles published *after* ``since`` (UTC).

        If *since* is *None*, the adapter should fetch a reasonable default
        window (e.g., last X minutes)."""


class _NoOpBazingaAdapter:
    source_name = "bazinga"

    def fetch_articles(
        self, since: datetime | None
    ) -> Iterable[RawNewsArticle]:  # noqa: D401
        logger.debug("[BazingaAdapter] fetch_articles called with since=%s", since)
        return []


def run_ingestion_cycle(
    *,
    cosmos_client: CosmosDBClient | None = None,
    service_bus: ServiceBusClient | None = None,
    provider_adapter: ProviderAdapter | None = None,
) -> NoReturn:  # pragma: no cover
    """Run a single ingestion cycle for the Bazinga provider."""

    cosmos_client = cosmos_client or InMemoryCosmosDBClient()
    service_bus = service_bus or InMemoryServiceBusClient()
    provider_adapter = provider_adapter or _NoOpBazingaAdapter()

    source_name = provider_adapter.source_name

    last_ts = cosmos_client.get_last_ingested_timestamp(source_name)
    logger.info("Last ingested timestamp for %s: %s", source_name, last_ts)

    raw_articles: List[RawNewsArticle] = list(provider_adapter.fetch_articles(last_ts))
    logger.info(
        "Fetched %d raw article(s) from provider %s.", len(raw_articles), source_name
    )

    new_count = 0
    skipped_count = 0

    for article in raw_articles:
        if cosmos_client.article_hash_exists(article.article_hash):
            logger.debug("Duplicate article skipped (hash=%s)", article.article_hash)
            skipped_count += 1
            continue

        service_bus.send_message(article.dict())
        new_count += 1
        logger.debug("Queued new article (hash=%s)", article.article_hash)

    logger.info(
        "Cycle summary → queued: %d | duplicates: %d | total fetched: %d",
        new_count,
        skipped_count,
        len(raw_articles),
    )
