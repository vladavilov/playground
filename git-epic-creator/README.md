# Git Epic Creator - Microservices

This repository contains microservices for the Git Epic Creator project, including database initialization services for PostgreSQL and Neo4j Graph RAG schema.

## Architecture Overview

The system consists of the following components:

### Core Services
- **db-init-service**: Initializes PostgreSQL database schema for requirement storage
- **neo4j-maintenance-service**: Initializes and maintains Neo4j Graph RAG schema
- **shared**: Common library with shared configurations and utilities

### Supporting Infrastructure
- **PostgreSQL**: Primary database for storing projects and requirements
- **Neo4j**: Graph database for Graph RAG implementation
- **PgAdmin**: Web-based PostgreSQL administration tool (optional)

## Quick Start

### Prerequisites
- Docker and Docker Compose installed
- At least 4GB RAM available for containers
- Ports 5432, 7474, 7687, 8001, 8002, 8080 available

### Starting the Services

```bash
# Clone the repository and navigate to the project directory
cd playground/git-epic-creator

# Start all services (databases will be initialized automatically)
docker-compose up --build

# Or start in detached mode
docker-compose up --build -d

# Start with admin tools (PgAdmin)
docker-compose --profile admin-tools up --build

# Use custom environment file
docker-compose --env-file docker-compose.env up --build

# Production deployment with Docker secrets
docker-compose -f docker-compose.yml -f docker-compose.prod.yml up -d
```

### Stopping the Services

```bash
# Stop all services
docker-compose down

# Stop and remove all data volumes (WARNING: This will delete all data!)
docker-compose down -v
```

## Service Details

### Database Initialization Service (db-init-service)
- **Port**: 8001
- **Purpose**: Initializes PostgreSQL database schema
- **API Endpoints**:
  - `GET /health` - Health check
  - `POST /init-db` - Initialize database schema

**Usage Example**:
```bash
# Check service health
curl http://localhost:8001/health

# Initialize database schema
curl -X POST http://localhost:8001/init-db
```

### Neo4j Maintenance Service (neo4j-maintenance-service)
- **Port**: 8002
- **Purpose**: Initializes and maintains Neo4j Graph RAG schema
- **API Endpoints**:
  - `GET /health` - Health check
  - `POST /init-neo4j` - Initialize Neo4j schema
  - `POST /maintenance/run` - Run maintenance tasks

**Usage Example**:
```bash
# Check service health
curl http://localhost:8002/health

# Initialize Neo4j schema
curl -X POST http://localhost:8002/init-neo4j

# Run maintenance tasks
curl -X POST http://localhost:8002/maintenance/run
```

### Database Access

#### PostgreSQL
- **Host**: localhost
- **Port**: 5432
- **Database**: requirementsdb
- **Username**: postgres
- **Password**: postgres123

#### Neo4j
- **Browser**: http://localhost:7474
- **Bolt URI**: bolt://localhost:7687
- **Username**: neo4j
- **Password**: neo4j123

#### PgAdmin (Optional)
- **URL**: http://localhost:8080
- **Email**: admin@admin.com
- **Password**: admin123

## Development

### Building Individual Services

```bash
# Build db-init-service
docker-compose build db-init-service

# Build neo4j-maintenance-service
docker-compose build neo4j-maintenance-service
```

### Viewing Logs

```bash
# View logs for all services
docker-compose logs -f

# View logs for specific service
docker-compose logs -f db-init-service
docker-compose logs -f neo4j-maintenance-service
```

### Service Dependencies

The services have the following startup order:
1. **postgres** and **neo4j** (databases)
2. **db-init-service** (after postgres is healthy)
3. **neo4j-maintenance-service** (after neo4j is healthy)

## Environment Configuration

### Default Environment Variables

The following environment variables are configured by default:

#### Database Init Service
- `API_PORT=8000`
- `DATABASE_URL=postgresql://postgres:postgres123@postgres:5432/requirementsdb`

#### Neo4j Maintenance Service (Following Official Neo4j Documentation)
- `API_PORT=8000`
- `NEO4J_URI=bolt://neo4j:7687`
- `NEO4J_USERNAME=neo4j`
- `NEO4J_PASSWORD=neo4j123`
- `NEO4J_DATABASE=neo4j`
- `NEO4J_HEAP_INITIAL=512m`
- `NEO4J_HEAP_MAX=1G`
- `NEO4J_PAGECACHE=512m`
- `CONNECTION_TIMEOUT=30.0`
- `MAX_RETRY_ATTEMPTS=3`
- `RETRY_DELAY=2.0`

### Custom Configuration

#### Development Environment
Use the provided environment file:

```bash
# Use the provided environment configuration
docker-compose --env-file docker-compose.env up --build
```

#### Production Environment
For production deployments, use Docker secrets (recommended):

```bash
# Create secrets directory and files
mkdir -p secrets
echo "your_secure_neo4j_password" > secrets/neo4j_password.txt
echo "your_secure_postgres_password" > secrets/postgres_password.txt

# Set proper permissions
chmod 600 secrets/*.txt
chmod 700 secrets/

# Deploy with production configuration
docker-compose -f docker-compose.yml -f docker-compose.prod.yml up -d
```

#### Alternative: Environment Variables Override
Create a custom `.env` file in the root directory:

```env
# PostgreSQL Configuration
POSTGRES_DB=requirementsdb
POSTGRES_USER=postgres
POSTGRES_PASSWORD=your_postgres_password

# Neo4j Configuration
NEO4J_USERNAME=neo4j
NEO4J_PASSWORD=your_neo4j_password
NEO4J_DATABASE=neo4j
NEO4J_HEAP_INITIAL=1G
NEO4J_HEAP_MAX=2G
NEO4J_PAGECACHE=1G
```

## Troubleshooting

### Common Issues

1. **Port Conflicts**: Ensure ports 5432, 7474, 7687, 8001, 8002, 8080 are available
2. **Memory Issues**: Neo4j requires at least 1GB RAM. Increase Docker memory allocation if needed
3. **Database Connection**: Wait for health checks to pass before accessing services

### Health Check Commands

```bash
# Check if all services are healthy
docker-compose ps

# Check specific service health
curl http://localhost:8001/health
curl http://localhost:8002/health
```

### Debugging

```bash
# Enter service container for debugging
docker-compose exec db-init-service bash
docker-compose exec neo4j-maintenance-service bash

# View service configuration
docker-compose config
```

## API Documentation

Once the services are running, you can access the automatic API documentation:

- **Database Init Service**: http://localhost:8001/docs
- **Neo4j Maintenance Service**: http://localhost:8002/docs

## Data Persistence

Data is persisted in Docker volumes:
- `postgres_data`: PostgreSQL data
- `neo4j_data`: Neo4j database files
- `neo4j_logs`: Neo4j logs
- `neo4j_plugins`: Neo4j plugins

## Security Considerations

⚠️ **Important**: The default configuration uses hardcoded passwords for development purposes. For production deployment:

1. **Use Docker Secrets**: The project includes `docker-compose.prod.yml` with Docker secrets configuration
2. **Enable SSL/TLS**: Configure database connections with SSL/TLS encryption
3. **Configure proper network security**: Use encrypted overlay networks in production
4. **Strong passwords**: Use minimum 12-character passwords (Neo4j requirement)
5. **External secret management**: Consider HashiCorp Vault, AWS Secrets Manager, or Azure Key Vault
6. **Regular security updates**: Keep database images updated with latest security patches

### Production Files Structure

```
playground/git-epic-creator/
├── docker-compose.yml          # Development configuration
├── docker-compose.prod.yml     # Production overrides with secrets
├── docker-compose.env          # Environment variables
├── secrets/                    # Docker secrets (create manually)
│   ├── README.md              # Secrets setup guide
│   ├── neo4j_password.txt     # Neo4j password file
│   └── postgres_password.txt  # PostgreSQL password file
└── start.sh / start.bat       # Startup scripts
```

## Support

For issues and questions:
1. Check the service logs: `docker-compose logs -f <service-name>`
2. Verify service health: `curl http://localhost:<port>/health`
3. Review the Docker Compose configuration for environment variables 