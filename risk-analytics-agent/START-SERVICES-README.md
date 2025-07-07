# Docker Compose Service Startup Scripts

This directory contains scripts to start different combinations of services from the Docker Compose configuration.

## Available Scripts

### PowerShell Script (Recommended)
- **File**: `start-services.ps1`
- **Usage**: `.\start-services.ps1 -Group <group> [-Build] [-Detach]`

### Batch File (Windows)
- **File**: `start-services.bat`
- **Usage**: `start-services.bat <group> [build] [detach]`

## Service Groups

### `cosmos`
**Services**: `cosmosdb-emulator`, `cosmos-init`, `dis-orchestrator-service`
- Cosmos DB emulator for local development (runs in HTTPS mode)
- Database initialization service
- Data ingestion orchestrator service (shares network stack with Cosmos DB)
- **Access**: 
  - Cosmos DB: https://localhost:8081/_explorer/index.html
  - DIS Orchestrator: http://localhost:8002
- **Note**: Emulator configured with `PROTOCOL: "https"` to match client connections
- **SSL**: Uses self-signed certificates (SSL verification disabled in client code)
- **Dependencies**: DIS Orchestrator requires Service Bus and Benzinga Adapter (will auto-start if needed)

### `servicebus`
**Services**: `sqledge`, `servicebus-emulator`, `servicebus-healthcheck`
- SQL Edge database (required for Service Bus)
- Service Bus emulator
- Health check service
- **Access**: 
  - Management: http://localhost:9090
  - Health: http://localhost:5300/health

### `emulators`
**Services**: All Azure emulators (Cosmos DB + Service Bus + DIS Orchestrator)
- Combines `cosmos` and `servicebus` groups
- Perfect for testing Azure services locally with data ingestion

### `apps`
**Services**: `benzinga-adapter`, `dis-orchestrator-service`
- Application services that process news data
- **Access**:
  - Benzinga Adapter: http://localhost:8001
  - DIS Orchestrator: http://localhost:8002

### `all`
**Services**: All services in the Docker Compose file
- Complete system startup
- All emulators + all application services

## Usage Examples

```powershell
# Start only Cosmos DB services
.\start-services.ps1 -Group cosmos

# Start all Azure emulators
.\start-services.ps1 -Group emulators

# Start all services with forced rebuild
.\start-services.ps1 -Group all -Build

# Start apps in detached mode
.\start-services.ps1 -Group apps -Detach
```

```batch
# Windows batch file examples
start-services.bat cosmos
start-services.bat emulators
start-services.bat all build
start-services.bat apps detach
```

## Parameters

- **Group** (required): Which group of services to start
- **Build** (optional): Force rebuild of Docker images before starting
- **Detach** (optional): Run services in detached mode (background)

## Service Dependencies

The scripts respect service dependencies automatically:
- `cosmos-init` depends on `cosmosdb-emulator`
- `servicebus-emulator` depends on `sqledge`
- `servicebus-healthcheck` depends on `servicebus-emulator`
- `dis-orchestrator-service` depends on `cosmos-init`, `servicebus-healthcheck`, and `benzinga-adapter`

## Useful Commands

```bash
# Stop all services
docker-compose down

# View logs for specific service
docker-compose logs -f <service-name>

# View logs for all services
docker-compose logs -f

# Check running services
docker-compose ps

# Remove volumes (clean start)
docker-compose down -v
```

## Port Mappings

| Service | Port(s) | Description |
|---------|---------|-------------|
| Cosmos DB | 8081, 1234 | Emulator and data gateway |
| SQL Edge | 1433 | SQL Server connection |
| Service Bus | 5672, 5671, 9090, 5300 | AMQP, AMQPS, Management, Health |
| Benzinga Adapter | 8001 | News data adapter |
| DIS Orchestrator | 8002 | Data ingestion orchestrator |

## Troubleshooting

- **Permission denied**: Run PowerShell as Administrator or use `-ExecutionPolicy Bypass`
- **Port conflicts**: Check if ports are already in use with `netstat -an`
- **Service startup failures**: Check logs with `docker-compose logs <service-name>`
- **Network issues**: Verify Docker networks with `docker network ls` 