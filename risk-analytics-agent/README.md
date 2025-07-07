# Risk Analytics Agent

This repository contains the risk analytics microservices architecture for financial news sentiment analysis.

## Quick Start

### Prerequisites
- Docker and Docker Compose
- Python 3.11+ (for local development)

### Running the Application

1. **Clone and navigate to the project directory:**
   ```bash
   cd playground/risk-analytics-agent
   ```

2. **Start the services:**
   ```bash
   docker-compose up --build
   ```

### Performance Optimization

For better build performance, enable Docker Buildx Bake before running docker-compose:

```bash
# Option 1: Set environment variable for current session
export COMPOSE_BAKE=true
docker-compose up --build

# Option 2: Set permanently in your shell profile
echo 'export COMPOSE_BAKE=true' >> ~/.bashrc  # or ~/.zshrc
source ~/.bashrc

# Option 3: Create a .env file in the project root
echo 'COMPOSE_BAKE=true' > .env
docker-compose up --build
```

This improvement enables Docker Compose to delegate builds to Docker Buildx Bake, providing:
- Better build caching
- Parallel build execution
- Improved overall build performance

## Architecture

The system consists of several microservices:

- **Cosmos DB Emulator**: Local database for development
- **Service Bus Emulator**: Message queue for processing
- **Benzinga Adapter**: News data source adapter
- **Data Ingestion Orchestrator**: Coordinates news processing
- **Cosmos Init Service**: Initializes database schema

## Environment Variables

Key environment variables for configuration:

- `COMPOSE_BAKE=true`: Enable Docker Buildx Bake for better performance
- `USE_LOCAL_FALLBACK=true`: Use local emulators instead of Azure services
- `COSMOS_DB_NAME`: Database name (default: risk-analytics-db)
- `SERVICE_BUS_QUEUE_NAME`: Queue name for processing

## Development

### Local Development Setup

1. **Install dependencies:**
   ```bash
   pip install -r requirements.txt
   ```

2. **Run tests:**
   ```bash
   pytest
   ```

3. **Code formatting:**
   ```bash
   black .
   flake8 .
   ```

### Service URLs (when running locally)

- Benzinga Adapter: http://localhost:8001
- Data Ingestion Orchestrator: http://localhost:8002
- Cosmos DB Emulator: http://localhost:8081
- Service Bus Emulator: http://localhost:5672

## Troubleshooting

### Common Issues

1. **Build performance is slow**
   - Ensure `COMPOSE_BAKE=true` is set
   - Check Docker daemon resources allocation

2. **Services fail to start**
   - Check that all required ports are available
   - Verify emulator health checks are passing

3. **Database connection issues**
   - Ensure Cosmos Init service completed successfully
   - Check emulator logs for connection errors

### Logs

View service logs:
```bash
# All services
docker-compose logs

# Specific service
docker-compose logs benzinga-adapter
docker-compose logs dis-orchestrator-service
```

## Contributing

1. Follow the existing code style
2. Add tests for new features
3. Update documentation as needed
4. Ensure all services build and run successfully
