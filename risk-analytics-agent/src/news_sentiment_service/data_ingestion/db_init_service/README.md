# Cosmos DB Initialization Service

This microservice is responsible for initializing the Cosmos DB emulator with the required database and container for the risk analytics application.

## Purpose

- Ensures the Cosmos DB emulator is ready before other services start
- Creates the required database (`risk-analytics-db`) and container (`enriched-news-events`)
- Uses the shared common library for consistency with other services
- Provides proper error handling and retry logic

## Architecture

- **Microservice Pattern**: Follows the same structure as other services in the news_sentiment_service
- **Shared Dependencies**: Uses the common library for Cosmos DB client functionality
- **Docker Integration**: Built as a Docker container and integrated into docker-compose
- **One-time Execution**: Runs once during startup to initialize the database

## Configuration

The service uses the same configuration pattern as other services:

- `COSMOS_DB_NAME`: Database name (default: "risk-analytics-db")
- `CONTAINER_NAME`: Container name (default: "enriched-news-events")
- `USE_LOCAL_FALLBACK`: Whether to use emulator (default: true)
- `COSMOS_EMULATOR_ENDPOINT`: Emulator endpoint
- `COSMOS_EMULATOR_KEY`: Emulator authentication key

## Dependencies

- Depends on `cosmosdb-emulator` being healthy
- Other services depend on this service completing successfully
- Uses the shared `news-sentiment-common` library

## Usage

This service is automatically started by docker-compose and runs once to initialize the database. Other services wait for it to complete before starting. 