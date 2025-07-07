# Local Development with Fallback Mechanism

This orchestrator service supports automatic fallback to local emulators when Azure services are not available.

## Fallback Behavior

The service will automatically:

1. **Try Azure services first** (if configured and available)
2. **Log warnings** when Azure is unavailable
3. **Fall back to local emulators** seamlessly
4. **Continue processing** without interruption

## Environment Variables

### Azure Production Settings
```bash
# Azure App Configuration
AZURE_APPCONFIG_ENDPOINT=https://appconfig-risk-analytics-dev.azconfig.io

# Azure Cosmos DB
AZURE_COSMOSDB_ENDPOINT=https://your-cosmos-db.documents.azure.com:443/

# Azure Service Bus
SERVICE_BUS_NAMESPACE=your-sb-namespace
```

### Required Settings
```bash
COSMOS_DB_NAME=risk-analytics-db
SERVICE_BUS_QUEUE_NAME=articles-to-process
ADAPTER_URLS=http://benzinga-adapter:8000/news
```

### Local Fallback Settings
```bash
# Force local mode (optional)
USE_LOCAL_FALLBACK=false

# Cosmos DB Emulator
COSMOS_EMULATOR_ENDPOINT=https://cosmosdb-emulator:8081
COSMOS_EMULATOR_KEY=C2y6yDjf5/R+ob0N8A7Cgv30VRDJIWEHLM+4QDU5DE2nQ9nDuVTqobD4b8mGGyPMbIZnqyMsEcaGQy67XIw/Jw==

# Service Bus Emulator
SERVICE_BUS_EMULATOR_CONNECTION_STRING=Endpoint=sb://servicebus-emulator:5672;SharedAccessKeyName=RootManageSharedAccessKey;SharedAccessKey=SAS_KEY_VALUE;UseDevelopmentEmulator=true
```

## Docker Compose Usage

### Start with Emulators
```bash
# Start all services including emulators
docker-compose up --build

# Start only emulators
docker-compose up cosmosdb-emulator servicebus-emulator sqledge
```

### Force Local Mode
Set `USE_LOCAL_FALLBACK=true` in the docker-compose.yml environment section to bypass Azure completely.

## Local Development (Outside Docker)

1. **Start emulators:**
   ```bash
   docker-compose up cosmosdb-emulator servicebus-emulator sqledge
   ```

2. **Create .env file:**
   ```bash
   # Copy the example configuration
   cp .env.example .env
   
   # Edit with your local settings
   USE_LOCAL_FALLBACK=true
   COSMOS_EMULATOR_ENDPOINT=https://localhost:8081
   SERVICE_BUS_EMULATOR_CONNECTION_STRING=Endpoint=sb://localhost:5672;SharedAccessKeyName=RootManageSharedAccessKey;SharedAccessKey=SAS_KEY_VALUE;UseDevelopmentEmulator=true
   ```

3. **Run the orchestrator:**
   ```bash
   python src/main.py
   ```

## Emulator Services

### Cosmos DB Emulator
- **URL:** https://localhost:8081
- **Explorer:** https://localhost:8081/_explorer/index.html
- **Key:** C2y6yDjf5/R+ob0N8A7Cgv30VRDJIWEHLM+4QDU5DE2nQ9nDuVTqobD4b8mGGyPMbIZnqyMsEcaGQy67XIw/Jw==

### Service Bus Emulator
- **AMQP Endpoint:** localhost:5672
- **Management API:** http://localhost:9090
- **Health Check:** http://localhost:9090/health

### SQL Edge (Service Bus Dependency)
- **Server:** localhost,1433
- **User:** sa
- **Password:** YourStrong!Passw0rd
