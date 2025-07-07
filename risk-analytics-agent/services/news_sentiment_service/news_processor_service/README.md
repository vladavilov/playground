# News Processor Service

## Overview

The News Processor Service is a FastAPI-based microservice that performs AI-powered enrichment of raw news articles. It listens to Azure Service Bus messages, processes articles using Azure OpenAI, and stores enriched results in Cosmos DB.

## Features

### Core Functionality
- **Background Message Processing**: Continuously processes news articles from Azure Service Bus
- **AI-Powered Enrichment**: Uses Azure OpenAI for entity extraction and sentiment analysis
- **Robust Error Handling**: Retry logic with dead letter queues for failed messages
- **Health Monitoring**: Multiple health check endpoints for monitoring service status
- **Local Fallback Mode**: Supports local development without Azure dependencies

### Article Processing Pipeline
1. **Entity Extraction**: Identifies issuers, sectors, states, and CUSIP codes
2. **Relevance Scoring**: Determines if articles are relevant for financial analysis
3. **Sentiment Analysis**: Scores articles for sentiment and magnitude
4. **Event Classification**: Categorizes articles by event type (upgrades, downgrades, etc.)

## Architecture

### Data Flow
```
[Benzinga Adapter] -> [DIS Orchestrator] -> [Service Bus Queue] -> [News Processor] -> [Cosmos DB]
                                                   ^                       |
                                              articles-to-process    AI Enrichment
                                                                    (Azure OpenAI)
```

### Docker Configuration
- **Container Port**: 8000 (internal)
- **Host Port**: 8002 (external - http://localhost:8002)
- **Dependencies**: 
  - Service Bus emulator (servicebus-healthcheck)
  - Cosmos DB initialization (cosmos-init)

### Components

#### ArticleEnricher
- **Purpose**: Handles AI-powered article enrichment
- **Features**: Two-step AI prompting, structured output parsing, error handling
- **Dependencies**: Azure OpenAI client

#### MessageProcessor
- **Purpose**: Manages background processing of Service Bus messages
- **Features**: Async message processing, retry logic, statistics tracking
- **Dependencies**: Service Bus client, Cosmos DB client, ArticleEnricher

#### FastAPI Application
- **Purpose**: HTTP API for health monitoring and manual processing
- **Features**: Lifespan management, background tasks, OpenAPI documentation
- **Endpoints**:
  - `GET /health` - Basic health check
  - `GET /health/detailed` - Detailed dependency status
  - `GET /stats` - Processing statistics

## Prerequisites

- Python 3.13+
- Azure OpenAI account with GPT-4 deployment
- Azure Service Bus namespace with queue
- Azure Cosmos DB account with database and container
- Azure App Configuration (optional)

## Installation

### Local Development

1. **Clone the repository**
   ```bash
   cd playground/risk-analytics-agent/services/news_sentiment_service/news_processor_service
   ```

2. **Install dependencies**
   ```bash
   pip install -e .
   ```

3. **Set up environment variables**
   ```bash
   # Create .env file
   USE_LOCAL_FALLBACK=true
   API_PORT=8000
   
   # For Azure mode, add:
   AZURE_OPENAI_ENDPOINT=https://your-openai.openai.azure.com/
   AZURE_OPENAI_API_KEY=your-api-key
   AZURE_OPENAI_API_VERSION=2024-02-01
   AZURE_OPENAI_DEPLOYMENT_NAME=gpt-4
   AZURE_SERVICEBUS_NAMESPACE=your-servicebus-namespace
   SERVICE_BUS_QUEUE_NAME=news-articles
   AZURE_COSMOSDB_ENDPOINT=https://your-cosmos.documents.azure.com:443/
   COSMOS_DB_DATABASE_NAME=news-sentiment
   COSMOS_DB_CONTAINER_NAME=enriched-articles
   ```

4. **Run the service**
   ```bash
   python -m src.main
   ```

### Docker Deployment

#### Using Docker Compose (Recommended)
```bash
# Start all services including dependencies
docker-compose up --build

# Service will be available at http://localhost:8002
```

#### Standalone Docker Container
1. **Build the Docker image**
   ```bash
   docker build -t news-processor-service .
   ```

2. **Run with Docker network**
   ```bash
   # Create network first
   docker network create risk-analytics-net
   
   # Run container
   docker run -p 8002:8000 \
     --network risk-analytics-net \
     -e USE_LOCAL_FALLBACK=false \
     -e AZURE_OPENAI_ENDPOINT=https://your-openai.openai.azure.com/ \
     -e AZURE_OPENAI_API_KEY=your-api-key \
     news-processor-service
   ```

#### Health Check
```bash
# Check service health
curl http://localhost:8002/health

# View detailed status
curl http://localhost:8002/health/detailed

# Monitor processing stats
curl http://localhost:8002/stats
```

## Configuration

### Environment Variables

#### Core Configuration
| Variable | Description | Required | Default |
|----------|-------------|----------|---------|
| `USE_LOCAL_FALLBACK` | Enable local development mode | No | `false` |
| `API_PORT` | HTTP server port | No | `8000` |

#### Service Bus Configuration
| Variable | Description | Required | Default |
|----------|-------------|----------|---------|
| `SERVICE_BUS_QUEUE_NAME` | Queue name for news articles | Yes* | `articles-to-process` |
| `SERVICE_BUS_EMULATOR_CONNECTION_STRING` | Service Bus emulator connection | Local | Auto-configured |
| `AZURE_SERVICEBUS_NAMESPACE` | Azure Service Bus namespace | Prod* | - |

#### Cosmos DB Configuration  
| Variable | Description | Required | Default |
|----------|-------------|----------|---------|
| `COSMOS_DB_DATABASE_NAME` | Cosmos DB database name | Yes* | `risk-analytics-db` |
| `COSMOS_DB_CONTAINER_NAME` | Cosmos DB container name | Yes* | `enriched-news-events` |
| `COSMOS_EMULATOR_ENDPOINT` | Cosmos DB emulator endpoint | Local | `http://host.docker.internal:8081` |
| `COSMOS_EMULATOR_KEY` | Cosmos DB emulator key | Local | Auto-configured |
| `AZURE_COSMOSDB_ENDPOINT` | Azure Cosmos DB endpoint | Prod* | - |

#### Azure OpenAI Configuration
| Variable | Description | Required | Default |
|----------|-------------|----------|---------|
| `AZURE_OPENAI_ENDPOINT` | Azure OpenAI endpoint URL | Prod* | - |
| `AZURE_OPENAI_API_KEY` | Azure OpenAI API key | Prod* | - |
| `AZURE_OPENAI_API_VERSION` | Azure OpenAI API version | Prod* | `2024-02-01` |
| `AZURE_OPENAI_DEPLOYMENT` | GPT-4 deployment name | Prod* | `gpt-4` |

*Required when `USE_LOCAL_FALLBACK=false`

### Message Processing Flow

#### 1. Message Consumption
```
Service Bus Queue -> MessageProcessor -> RawNewsArticle
```
- Polls queue every 30 seconds
- Processes up to 10 messages in parallel
- Handles JSON deserialization errors

#### 2. AI Enrichment
```
RawNewsArticle -> ArticleEnricher -> EnrichedNewsEvent
```
- **Local Mode**: Returns default/placeholder enrichment
- **Production Mode**: Uses Azure OpenAI for real AI analysis
- Two-step AI process: Entity extraction → Sentiment analysis

#### 3. Data Persistence
```
EnrichedNewsEvent -> CosmosDBClient -> Cosmos DB
```
- Saves enriched articles to `enriched-news-events` container
- Includes deduplication via article hash
- Atomic message completion after successful save

#### 4. Error Handling
- **Retry Logic**: Failed messages abandoned for retry (max 3 attempts)
- **Dead Letter**: Persistent failures sent to dead letter queue
- **Circuit Breaker**: Processing pauses on consecutive errors

### Azure App Configuration

The service can load configuration from Azure App Configuration when available:

```python
# Configuration keys in App Configuration
azure_openai_endpoint = "https://your-openai.openai.azure.com/"
azure_openai_api_version = "2024-02-01"
azure_openai_deployment_name = "gpt-4"
service_bus_namespace = "your-servicebus-namespace"
service_bus_queue_name = "news-articles"
cosmos_db_database_name = "news-sentiment"
cosmos_db_container_name = "enriched-articles"
```

## Usage

### Health Monitoring

```bash
# Basic health check (runs on port 8000 in both Docker and local)
curl http://localhost:8000/health

# Detailed health check
curl http://localhost:8000/health/detailed

# Processing statistics
curl http://localhost:8000/stats
```



### API Documentation

Access the interactive API documentation at:
- Swagger UI: `http://localhost:8000/docs`
- ReDoc: `http://localhost:8000/redoc`

## Development

### Running Tests

```bash
# Run all tests
python -m pytest tests/ -v

# Run specific test file
python -m pytest tests/test_article_enricher.py -v

# Run with coverage
python -m pytest tests/ --cov=src --cov-report=html
```

### Code Quality

```bash
# Format code
black src/ tests/

# Lint code
flake8 src/ tests/

# Type checking
mypy src/
```

### Local Development Mode

Set `USE_LOCAL_FALLBACK=true` to run the service without Azure dependencies:

- Azure OpenAI client is not initialized
- Message processing is disabled
- Health checks report local fallback mode
- Stats endpoint returns fallback message

## Monitoring

### Health Endpoints

- **`/health`**: Basic service health with key indicators
- **`/health/detailed`**: Comprehensive dependency status
- **`/stats`**: Processing statistics and metrics

### Health Check Strategy
```bash
# Docker health check (every 30s)
curl -f http://localhost:8000/health

# Response format
{
  "status": "healthy",
  "service": "news-processor-service", 
  "azure_openai_configured": false,
  "message_processor_active": false,
  "settings": {
    "use_local_fallback": true,
    "api_port": 8000
  }
}
```

### Logging

The service uses structured logging with these levels:
- `INFO`: Normal operation, message processing
- `WARNING`: Non-critical issues, retries
- `ERROR`: Processing failures, configuration issues
- `DEBUG`: Detailed debugging information

### Metrics

Available through the `/stats` endpoint:
- Messages processed count
- Messages failed count
- Processing errors count
- Service uptime
- Last processed timestamp

## Security

### Container Security
- **Non-root User**: Runs as `appuser` (not root)
- **Minimal Base**: Uses Python 3.13-slim image
- **No Secrets**: Uses emulator keys for local development

### Network Security
- **Internal Networks**: Service-to-service communication isolated
- **Health Endpoints**: Only basic status exposed externally
- **Input Validation**: Pydantic models validate all inputs

## Troubleshooting

### Common Issues

1. **Service Won't Start**
   ```bash
   # Check Service Bus emulator health
   curl http://localhost:5300/health
   
   # Verify Cosmos DB initialization completed
   docker logs cosmos-init
   
   # Check service logs
   docker logs news-processor-service
   ```

2. **Messages Not Processing**
   ```bash
   # Verify local fallback mode
   curl http://localhost:8000/health
   
   # Check queue has messages (review orchestrator logs)
   docker logs dis-orchestrator-service
   
   # Monitor processing errors
   curl http://localhost:8000/stats
   ```

3. **Database Connection Issues**
   ```bash
   # Ensure cosmos-init completed successfully
   docker logs cosmos-init
   
   # Verify Cosmos DB emulator running
   curl http://localhost:8081/_explorer/index.html
   
   # Check container connectivity (with host networking)
   docker exec news-processor-service nslookup localhost
   ```

4. **Azure OpenAI Authentication Errors** (Production)
   - Verify `AZURE_OPENAI_API_KEY` is correct
   - Check endpoint URL format
   - Ensure deployment name matches Azure configuration

5. **Service Bus Connection Issues** (Production)
   - Verify namespace and queue name
   - Check Azure credentials and permissions
   - Ensure queue exists and has messages

### Log Monitoring

```bash
# Service logs
docker logs -f news-processor-service

# All related services
docker-compose logs -f news-processor-service dis-orchestrator-service cosmos-init

# Filter by log level
docker logs news-processor-service 2>&1 | grep ERROR
```

### Performance Tuning

- Adjust `max_message_count` in message processing loop
- Configure retry policies for failed messages
- Monitor processing statistics for bottlenecks
- Scale container instances based on queue depth

## Production Considerations

### Scaling
- **Horizontal**: Multiple container instances can process queue in parallel
- **Resource Limits**: Configure memory/CPU limits based on AI processing load
- **Queue Management**: Monitor queue depth and processing throughput

### Configuration
- Replace placeholder Azure endpoints with real values
- Set `USE_LOCAL_FALLBACK=false` for production
- Configure proper Azure authentication (Managed Identity)
- Set up proper monitoring and alerting

### Network Configuration
```
news-processor-service:8000 (internal)
├── servicebus-emulator:5672 (Service Bus)
├── host.docker.internal:8081 (Cosmos DB Emulator)
└── External:8002 (HTTP API access)
```

## Contributing

1. Follow TDD methodology when adding new features
2. Ensure all tests pass before submitting changes
3. Add appropriate logging for debugging
4. Update documentation for new configuration options
5. Test both local fallback and Azure modes

## License

This project is part of the Risk Analytics system and follows the project's licensing terms. 