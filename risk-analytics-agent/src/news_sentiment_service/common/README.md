# News Sentiment Common

Shared library for News Sentiment services providing data models, database, and messaging clients.

## Components

### Data Models (`models.py`)
- `RawNewsArticle` - Input news article schema
- `EnrichedNewsEvent` - Core enriched article data model  
- `Entities` - Extracted financial entities (issuer, sector, state, CUSIPs)
- `Sentiment` - Sentiment analysis results (score, magnitude)
- `RealTimeSentimentResponse` / `HistoricalSentimentResponse` - API response models

### CosmosDB Client (`cosmos_db/`)
Azure CosmosDB client with emulator fallback. Auto-creates databases/containers.

```python
from news_sentiment_common.cosmos_db import CosmosDBClient

client = CosmosDBClient("news_db", "articles")
client.upsert_item({"id": "123", "title": "News Article"})
results = client.query_items("SELECT * FROM c WHERE c.source = @source", 
                           [{"name": "@source", "value": "bloomberg"}])
```

### Service Bus Client (`service_bus/`)
Azure Service Bus client with emulator fallback for message queuing.

```python
from news_sentiment_common.service_bus import ServiceBusClient

client = ServiceBusClient(queue_name="news-processing")
client.send_message("process this article")
client.receive_messages(callback=process_message, max_messages=10)
```

## Environment Variables

**Azure (Production):**
- `AZURE_COSMOSDB_ENDPOINT`
- `AZURE_SERVICEBUS_NAMESPACE`

**Local Development:**
- `USE_LOCAL_FALLBACK=true`
- `COSMOS_EMULATOR_ENDPOINT=http://localhost:8081`
- `SERVICE_BUS_EMULATOR_CONNECTION_STRING`

## Installation

```bash
pip install -e .[dev]
```

## Dependencies
- `azure-cosmos` - CosmosDB operations
- `azure-servicebus` - Message queuing  
- `azure-identity` - Azure authentication
- `pydantic` - Data validation and serialization
