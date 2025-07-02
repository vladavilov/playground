"""Cosmos DB client abstractions."""

from services.news_sentiment_service.common.cosmos_db.cosmos_db_client import CosmosDBClient, InMemoryCosmosDBClient

__all__ = [
    "CosmosDBClient",
    "InMemoryCosmosDBClient",
] 