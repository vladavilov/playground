from __future__ import annotations

from datetime import datetime
from typing import List, Optional
from uuid import uuid4

from pydantic import BaseModel, Field, HttpUrl, validator

__all__ = [
    "RawNewsArticle",
    "EnrichedNewsEvent",
]


class RawNewsArticle(BaseModel):
    """Input schema representing a raw news article prior to enrichment."""

    article_text: str = Field(..., description="Full text body of the news article.")
    source_name: str = Field(..., description="Name of the news source or publisher.")
    publication_time: datetime = Field(..., description="Original publication timestamp (UTC).")
    title: str = Field(..., description="Headline or title of the article.")
    url: HttpUrl = Field(..., description="Canonical URL pointing to the article.")
    article_hash: str = Field(
        ..., description="Deterministic MD5 hash of title + content, used for deduplication."
    )

    class Config:
        anystr_strip_whitespace = True
        frozen = True  # make it hashable & immutable once created


class Entities(BaseModel):
    """Nested model capturing entity extraction results."""

    issuer_name: Optional[str]
    sector: str
    state: Optional[str]
    cusips: List[str] = Field(default_factory=list)


class Sentiment(BaseModel):
    """Nested model capturing sentiment analysis output."""

    score: float = Field(..., ge=-1.0, le=1.0, description="Sentiment polarity between -1 and 1.")
    magnitude: float = Field(..., ge=0.0, description="Sentiment magnitude (intensity).")


class EnrichedNewsEvent(BaseModel):
    """Output schema produced after processing a raw news article."""

    id: str = Field(default_factory=lambda: uuid4().hex)
    source: str
    published_at: datetime
    ingested_at: datetime
    event_type: str
    entities: Entities
    sentiment: Sentiment
    source_credibility_tier: str
    summary_excerpt: str
    raw_article_url: HttpUrl

    # Validators / post-processing
    @validator("ingested_at")
    def _ingested_not_before_published(cls, v: datetime, values):  # noqa: N805
        published = values.get("published_at")
        if published and v < published:
            raise ValueError("ingested_at cannot be before published_at")
        return v

    class Config:
        anystr_strip_whitespace = True
        frozen = True
        json_encoders = {datetime: lambda v: v.isoformat()} 