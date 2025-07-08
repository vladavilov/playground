# Cosmos DB Initialization Service

This critical microservice initializes the Azure Cosmos DB storage layer for the **News Sentiment Service**, a component of the Fixed Income AI Risk Analytics System. It ensures the database infrastructure is ready before other data ingestion and processing services begin operations.

## System Context

### Role in News Sentiment Architecture
This service is part of the **Data Ingestion & Processing** component within the News Sentiment Service architecture. It supports the complete data flow:

```
News Sources → Data Ingestion → News Processor → Cosmos DB ← Sentiment API
                     ↑                              ↑
               [This Service Initializes]    [Stores Enriched Events]
```

### Business Requirements Supported
- **Storage Foundation**: Enables storage of `Enriched News Event` objects as defined in business requirements

## Technical Specifications

### Database Schema Initialization
Creates and configures:
- **Database**: `risk-analytics-db`
- **Container**: `enriched-news-events` 
- **Partition Strategy**: Date-based partitioning (`_partitionKey`)
- **Throughput**: 400 RU/s (minimum for emulator environment)

### Container Data Model Support
Initializes storage for the `Enriched News Event` schema:
```json
{
  "id": "string",
  "source": "string", 
  "published_at": "datetime",
  "ingested_at": "datetime",
  "event_type": "string",
  "entities": {
    "issuer_name": "string",
    "sector": "string", 
    "state": "string",
    "cusips": ["string"]
  },
  "sentiment": {
    "score": "float",
    "magnitude": "float"
  },
  "source_credibility_tier": "string",
  "summary_excerpt": "string",
  "raw_article_url": "string",
  "_partitionKey": "date"
}
```

## Architecture & Implementation

### Microservice Design Pattern
- **Language**: Python 3.13
- **Framework**: Standalone initialization service (no web framework)
- **Dependencies**: Uses shared `news-sentiment-common` library for consistency
- **Execution Model**: One-time initialization with comprehensive retry logic
- **Container Strategy**: Multi-stage Docker build for optimized image size

## Configuration

### Core Settings
| Parameter | Default | Description |
|-----------|---------|-------------|
| `COSMOS_DB_NAME` | `risk-analytics-db` | Target database name |
| `CONTAINER_NAME` | `enriched-news-events` | Target container name |
| `CONTAINER_THROUGHPUT` | `400` | RU/s provisioning (emulator minimum) |
| `MAX_RETRIES` | `30` | Connection retry attempts |
| `RETRY_DELAY` | `10` | Seconds between retry attempts |

### Environment-Specific Configuration
| Environment | Setting | Value |
|-------------|---------|-------|
| **Local Development** | `USE_LOCAL_FALLBACK` | `true` |
| **Local Development** | `COSMOS_EMULATOR_ENDPOINT` | `https://cosmosdb-emulator:8081` |
| **Local Development** | `COSMOS_EMULATOR_KEY` | `C2y6yDjf5/R+ob0N8A7Cgv...` |
| **Production** | `AZURE_COSMOSDB_ENDPOINT` | `https://<account>.documents.azure.com` |

## Service Dependencies & Integration

### Upstream Dependencies
- **`cosmosdb-emulator`**: Must be healthy and accessible before initialization
- **Network connectivity**: Docker network access to Cosmos DB service

### Downstream Dependents  
Services that require this initialization to complete:
- **Data Ingestion Service**: Stores deduplicated raw articles
- **News Processor Service**: Persists enriched news events after GPT-4 analysis  
- **Sentiment API Service**: Queries enriched events for score calculations  

### Docker Compose Integration
```yaml
# Dependency chain in docker-compose.yml
cosmosdb-emulator → db-init-service → [data-ingestion, news-processor, sentiment-api]
```

## Operational Characteristics

### Startup Behavior
1. **Health Check**: Verifies Cosmos DB emulator accessibility
2. **Database Creation**: Creates database if it doesn't exist
3. **Container Creation**: Creates container with proper configuration
4. **Verification**: Confirms successful initialization via test query
5. **Completion**: Exits with status code 0 (success) or 1 (failure)

## Local Development Usage

This service is automatically orchestrated by Docker Compose:

```bash
# Start the entire development environment
docker-compose up

# View initialization logs specifically  
docker-compose logs db-init-service

# Verify initialization success
docker-compose ps db-init-service  # Should show "exited (0)"
```

**Note**: All downstream services automatically wait for this service to complete successfully before starting their own initialization sequences. 