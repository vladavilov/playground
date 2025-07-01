"""Abstract and in-memory implementations of Cosmos DB client.

The goal is to provide a swappable interface. Production code can later
replace :class:`InMemoryCosmosDBClient` with an Azure SDK–backed client
without changing consumer code.
"""
from __future__ import annotations

from abc import ABC, abstractmethod
from datetime import datetime
from typing import Dict

from ..models import EnrichedNewsEvent


class CosmosDBClient(ABC):
    """Interface for minimal Cosmos DB operations required by the services."""

    # ------------------------------------------------------------------
    # Deduplication helpers
    # ------------------------------------------------------------------
    @abstractmethod
    def article_hash_exists(self, article_hash: str) -> bool:
        """Return ``True`` if an *EnrichedNewsEvent* with *article_hash* already exists."""

    # ------------------------------------------------------------------
    # Watermark helpers
    # ------------------------------------------------------------------
    @abstractmethod
    def get_last_ingested_timestamp(self, source_name: str) -> datetime | None:
        """Return the *ingested_at* timestamp of the most recent article for *source_name*.

        If the source is unknown, return *None*.
        """

    # ------------------------------------------------------------------
    # Persistence helpers
    # ------------------------------------------------------------------
    @abstractmethod
    def insert_enriched_news_event(self, event: EnrichedNewsEvent) -> None:
        """Persist *event* to the store."""


class InMemoryCosmosDBClient(CosmosDBClient):
    """Simple in-memory stand-in for Cosmos DB.

    Not thread-safe; intended only for unit testing.
    """

    def __init__(self) -> None:
        self._events_by_hash: Dict[str, EnrichedNewsEvent] = {}
        self._last_ts_by_source: Dict[str, datetime] = {}

    # ------------------------------------------------------------------
    # Interface implementations
    # ------------------------------------------------------------------
    def article_hash_exists(self, article_hash: str) -> bool:  # noqa: D401
        return article_hash in self._events_by_hash

    def get_last_ingested_timestamp(self, source_name: str) -> datetime | None:  # noqa: D401
        return self._last_ts_by_source.get(source_name)

    def insert_enriched_news_event(self, event: EnrichedNewsEvent) -> None:  # noqa: D401
        # Use event.id as hash surrogate if not provided in event.
        article_hash = getattr(event, "article_hash", None) or event.id
        self._events_by_hash[article_hash] = event

        # Update watermark per source
        last_ts = self._last_ts_by_source.get(event.source)
        if last_ts is None or event.ingested_at > last_ts:
            self._last_ts_by_source[event.source] = event.ingested_at

    # ------------------------------------------------------------------
    # Convenience helpers (non-interface)
    # ------------------------------------------------------------------
    def clear(self) -> None:
        """Reset internal state (used by tests)."""
        self._events_by_hash.clear()
        self._last_ts_by_source.clear() 