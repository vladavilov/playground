from datetime import datetime
from typing import List, Optional
from pydantic import BaseModel, Field

class RawNewsArticle(BaseModel):
    """
    Represents a raw news article, the basic input to the system.
    """
    article_text: str
    source_name: str
    publication_time: datetime
    title: str
    url: str
    article_hash: str = Field(..., description="MD5 hash of title + content, for deduplication")

class Entities(BaseModel):
    """
    Represents the structured entities extracted from a news article.
    """
    issuer_name: str
    sector: str
    state: Optional[str] = None
    cusips: List[str]

class Sentiment(BaseModel):
    """
    Represents the sentiment analysis results for a news article.
    """
    score: float
    magnitude: float

class EnrichedNewsEvent(BaseModel):
    """
    The standard schema for a single, enriched news article. This is the core data model.
    """
    id: str
    source: str
    published_at: datetime
    ingested_at: datetime
    event_type: str
    entities: Entities
    sentiment: Sentiment
    source_credibility_tier: str
    summary_excerpt: str
    raw_article_url: str

class RealTimeSentimentResponse(BaseModel):
    """
    The response model for the real-time sentiment API endpoint.
    """
    aggregated_sentiment_score: float
    contributing_articles_count: int
    articles: List[str]

class HistoricalSentimentResponse(BaseModel):
    """
    The response model for the historical sentiment API endpoint.
    """
    aggregated_sentiment_score: float
    contributing_articles_count: int 