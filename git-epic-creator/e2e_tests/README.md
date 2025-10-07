# End-to-End Test Suite

Comprehensive integration test suite for the microservices-based document processing and AI workflow system.

## Overview

This test suite validates the complete system behavior across all microservices, databases, message queues, and external integrations. Tests are designed to run in both Docker-based and local development environments.

### System Under Test

The e2e tests validate the following system components:

- **Project Management Service**: Project lifecycle and document upload
- **Document Processing Service**: Asynchronous document processing with Celery
- **Neo4j Ingestion Service**: Knowledge graph construction from documents
- **Neo4j Retrieval Service**: Semantic search and context retrieval (DRIFT algorithm)
- **AI Requirements Service**: AI-driven requirements generation
- **AI Tasks Service**: Backlog generation and GitLab integration
- **UI Service**: Frontend proxy with SSO and session management
- **Mock Auth Service**: Azure AD SSO simulation
- **Mock GitLab Service**: GitLab API simulation
- **PostgreSQL**: Relational data storage
- **Neo4j**: Knowledge graph storage
- **Redis**: Message broker and pub/sub
- **Azurite**: Azure Blob Storage emulation

## Architecture

### Design Principles

The test suite is built following **SOLID** and **DRY** principles:

- **Single Responsibility Principle (SRP)**: Each module has one clear responsibility
- **Open/Closed Principle (OCP)**: Easy to extend without modification
- **Liskov Substitution Principle (LSP)**: Clear inheritance hierarchies
- **Interface Segregation Principle (ISP)**: Focused, minimal interfaces
- **Dependency Inversion Principle (DIP)**: Depend on abstractions, not concretions
- **Don't Repeat Yourself (DRY)**: Eliminate code duplication
- **High Cohesion, Loose Coupling**: Related operations grouped, minimal dependencies

### Directory Structure

```
e2e_tests/
├── Dockerfile                  # Multi-stage build for test container
├── pyproject.toml             # Dependencies and pytest configuration
├── resources/                 # Test data files
│   ├── dummy.pdf             # Test document
│   ├── dummy_big.pdf         # Large document test
│   └── drift_search_cyphers.txt  # Neo4j seeding script
├── src/
│   ├── config.py             # Centralized configuration management
│   ├── conftest.py           # Pytest fixtures and session setup
│   ├── shared_utils.py       # Common utilities (HTTP, DB, etc.)
│   ├── services/             # Modular service components
│   │   ├── database/         # PostgreSQL operations
│   │   │   └── validators.py
│   │   ├── document/         # Document operations
│   │   │   └── operations.py
│   │   ├── neo4j/            # Neo4j operations
│   │   │   ├── operations.py    # Admin operations (reset, load)
│   │   │   ├── queries.py       # Cypher query constants
│   │   │   └── validators.py    # Schema/data validation
│   │   ├── validators/       # Response validation
│   │   │   ├── content_validators.py  # Content quality
│   │   │   └── response_validators.py # Structure validation
│   │   ├── workflow/         # Workflow helpers
│   │   │   ├── status_monitor.py  # Status polling
│   │   │   └── ui_helpers.py      # SSO simulation
│   │   ├── redis_test_monitor.py     # Redis pub/sub monitoring
│   │   ├── workflow_assertions.py    # Facade pattern for all services
│   │   └── workflow_models.py        # Data models
│   ├── test_document_workflow.py     # Document processing tests
│   ├── test_ui_workflow.py          # UI service workflow tests
│   ├── test_neo4j_drift_search.py   # Retrieval service tests
│   └── test_services_health.py      # Health and connectivity tests
└── test-results/             # JUnit XML results
    └── results.xml
```

## Test Modules

### 1. Service Health Tests (`test_services_health.py`)

Validates that all required services are running and accessible.

**Test Classes:**
- `TestServicesHealth`: Connectivity and health endpoint validation
  - `test_postgres_connectivity`: PostgreSQL connection and basic queries
  - `test_all_services_accessible`: All service health endpoints respond

- `TestDatabaseIntegrity`: Schema and data integrity
  - `test_postgres_schema_exists`: Required tables exist (`projects`, `project_members`)
  - `test_neo4j_connectivity`: Neo4j connection and basic operations
  - `test_redis_info`: Redis server info and database operations

**Purpose**: Ensures infrastructure is ready before running workflow tests.

### 2. Document Workflow Tests (`test_document_workflow.py`)

Tests the complete document processing pipeline from upload to knowledge graph ingestion.

**Test Classes:**
- `TestDocumentWorkflow`: Main document processing flow
  - `test_complete_document_processing_workflow`: End-to-end single document processing
    - Create project
    - Upload PDF document
    - Monitor UI progress messages (processing → active → rag_processing → rag_ready)
    - Verify database status and completion percentage
    - Verify Neo4j graph structure (project node, document nodes, relationships)
    - Verify Neo4j constraints and vector indexes
  
  - `test_multiple_document_upload_workflow`: Multiple documents to single project
    - Batch upload multiple documents
    - Verify all documents processed correctly
    - Validate graph contains all documents
  
  - `test_document_upload_error_handling`: Error scenarios
    - Upload without files (expect error)
    - Upload to non-existent project (expect 404)

- `TestDocumentWorkflowEdgeCases`: Edge cases and stress testing
  - `test_concurrent_document_uploads`: Rapid successive uploads

**Key Validations:**
- Celery task publishing to Redis queue
- Redis pub/sub progress messages (`ui_progress` channel)
- PostgreSQL status transitions
- Neo4j graph structure and relationships
- Vector index configuration for semantic search

### 3. UI Workflow Tests (`test_ui_workflow.py`)

Tests user-facing workflows through the UI service proxy, including AI-driven features.

**Test Classes:**
- `TestUIRequirementsWorkflow`: AI requirements and tasks generation
  - `test_ui_end_to_end_requirements_workflow`: AI requirements generation
    - Simulate SSO login (OAuth2 flow with mock Azure AD)
    - Create project via UI proxy
    - Upload document
    - Wait for RAG_READY status
    - Trigger AI requirements generation (`/workflow/requirements`)
    - Monitor AI progress messages: analyzing_prompt → retrieving_context → drafting_requirements
    - Validate requirements bundle structure and content quality
    - Verify mock content is present (for testing mock OpenAI service)
  
  - `test_ui_tasks_generation_workflow`: AI backlog generation with GitLab integration
    - Simulate SSO login
    - Simulate GitLab OAuth connection
    - Create project and upload document
    - Wait for RAG_READY status
    - Trigger tasks generation (`/tasks/generate`)
    - Monitor AI tasks progress: analyzing_requirements → retrieving_context → fetching_backlog → drafting_backlog → mapping_duplicates → evaluating
    - Validate backlog bundle structure with epics and tasks
    - Verify mock content in generated backlog items

**Key Features Tested:**
- SSO authentication flow simulation
- GitLab OAuth connection simulation
- Session management across requests
- AI workflow orchestration
- Redis pub/sub monitoring for AI progress (`ai_requirements_progress`, `ai_tasks_progress` channels)
- Response validation for business and functional requirements
- Backlog structure validation (epics, tasks, acceptance criteria)
- Content quality validation (mock content detection)

### 4. Neo4j Retrieval Tests (`test_neo4j_drift_search.py`)

Tests the semantic search and retrieval pipeline using the DRIFT algorithm.

**Test Function:**
- `test_retrieval_service`: Complete retrieval pipeline
  - Seed Neo4j with test data (using `drift_search_cyphers.txt`)
  - Execute retrieval query
  - Validate response structure: `final_answer`, `key_facts`, `residual_uncertainty`
  - Validate content quality (non-empty, meaningful content)
  - Validate embedded DRIFT JSON tree structure (hierarchical context)

**Key Validations:**
- DRIFT algorithm execution
- Neo4j vector search performance
- Response structure compliance
- Content quality heuristics
- JSON tree structure integrity

## Modular Service Components

The test suite uses a modular architecture in the `src/services/` directory:

### Neo4j Module (`services/neo4j/`)

- **`operations.py`**: Administrative operations
  - `reset_database()`: Clean database state
  - `load_cypher_script()`: Execute multi-statement Cypher scripts
  - `poll_active_queries()`: Monitor query execution

- **`queries.py`**: Cypher query constants
  - Centralized repository of all Cypher queries
  - Eliminates query string duplication
  - Query parameter builders

- **`validators.py`**: Schema and data validation
  - `verify_project_and_documents()`: Graph structure validation
  - `verify_constraints_minimal()`: Constraint verification
  - `verify_vector_index()`: Vector index validation
  - `verify_required_index_names()`: Named index checks

### Database Module (`services/database/`)

- **`validators.py`**: PostgreSQL validation
  - `verify_project_exists()`: Project creation validation
  - `verify_status()`: Status transition validation with completion percentage

### Document Module (`services/document/`)

- **`operations.py`**: Document operations
  - `upload_document()`: Single document upload with Celery task verification
  - `prepare_multiple_files()`: Batch upload preparation
  - `verify_upload_response()`: Upload response validation
  - Error handling verification functions

### Workflow Module (`services/workflow/`)

- **`ui_helpers.py`**: UI authentication helpers
  - `simulate_sso_login()`: Complete SSO flow simulation
  - `simulate_gitlab_oauth_connection()`: GitLab OAuth simulation
  - Session cookie management

- **`status_monitor.py`**: Status monitoring
  - `wait_for_api_status()`: Poll API until expected status
  - `expected_ui_sequence()`: Standard UI status sequence generator

### Validators Module (`services/validators/`)

- **`response_validators.py`**: Structure validation
  - `validate_workflow_progress_message()`: AI progress message structure
  - `validate_retrieving_context_message()`: Context retrieval message validation
  - `validate_backlog_bundle_comprehensive()`: Backlog structure validation

- **`content_validators.py`**: Content quality validation
  - `validate_mock_content_in_backlog()`: Mock response detection
  - `validate_mock_content_in_requirements()`: Mock requirements validation
  - `validate_retrieval_response_quality()`: Semantic quality checks
  - `validate_drift_json_tree()`: DRIFT tree structure validation

### Redis Test Monitor (`services/redis_test_monitor.py`)

Sophisticated Redis pub/sub monitoring for workflow progress tracking:

- **UI Progress Monitoring**: Track document processing status transitions
- **AI Requirements Monitoring**: Track AI requirements generation stages
- **AI Tasks Monitoring**: Track backlog generation stages
- **Sequence Validation**: Ensure correct status sequence with timeouts
- **Message Capture**: Store all messages for post-test analysis
- **Synchronization Barrier**: Uses `threading.Event` to guarantee listener thread is ready before processing starts, preventing race conditions where early messages are lost

### Workflow Assertions Facade (`services/workflow_assertions.py`)

Facade pattern providing simplified interface to all modular services:

- Backward-compatible interface for existing tests
- Delegates to specialized modules
- Context managers for monitoring (e.g., `ui_monitoring()`)
- Single import for test simplicity

## Configuration System

### Configuration Management (`config.py`)

Centralized configuration with environment variable support:

**TestConfig Class:**
- `get_service_urls()`: All microservice endpoints
- `get_postgres_config()`: PostgreSQL connection parameters
- `get_neo4j_config()`: Neo4j connection parameters
- `get_redis_config()`: Redis connection parameters (supports `REDIS_URL` or individual params)
- `get_auth_config()`: Azure AD/SSO configuration
- `get_celery_config()`: Celery broker configuration
- `get_blob_storage_config()`: Azure Blob Storage emulator configuration
- `get_dummy_pdf_path()`: Test document path resolution
- `read_dummy_pdf()`: Test document content loader

**TestConstants Class:**
- Timeout configurations
- HTTP status codes
- Project status constants
- Endpoint paths

### Environment Variables

Tests support environment variable configuration matching `docker-compose.env`:

```bash
# Service URLs
PROJECT_MANAGEMENT_SERVICE_URL=http://localhost:8003
DOCUMENT_PROCESSING_URL=http://localhost:8004
NEO4J_INGESTION_URL=http://localhost:8006
UI_SERVICE_URL=http://localhost:8007
AI_REQUIREMENTS_SERVICE_URL=http://localhost:8009
AI_TASKS_SERVICE_URL=http://localhost:8013
NEO4J_RETRIEVAL_URL=http://localhost:8008

# Database Configuration
POSTGRES_HOST=localhost
POSTGRES_PORT=5432
POSTGRES_USER=postgres
POSTGRES_PASSWORD=postgres123
POSTGRES_DB=requirementsdb

NEO4J_URI=bolt://localhost:7687
NEO4J_USERNAME=neo4j
NEO4J_PASSWORD=neo4j123
NEO4J_DATABASE=neo4j

REDIS_URL=redis://localhost:6379/0
# OR individual parameters:
REDIS_HOST=localhost
REDIS_PORT=6379
REDIS_DB=0

# Authentication
AZURE_TENANT_ID=e7963c3a-3b3a-43b6-9426-89e433d07e69
AZURE_CLIENT_ID=a9e304a9-5b6c-4ef7-9b37-23a579a6d7be
LOCAL_JWT_SECRET=dev-local-jwt-secret
```

## Pytest Fixtures

### Session-Scoped Fixtures (`conftest.py`)

Fixtures initialized once per test session:

- **`test_config`**: TestConfig instance
- **`service_urls`**: Service URL configuration
- **`postgres_config`**: PostgreSQL connection parameters
- **`neo4j_config`**: Neo4j connection parameters
- **`redis_config`**: Redis connection parameters
- **`auth_config`**: Authentication configuration
- **`blob_storage_config`**: Blob storage configuration
- **`services_ready`**: Blocks until all services are healthy (or skips tests)
- **`postgres_initialized`**: Ensures database schema is initialized via init_db_service
- **`target_db_name`**: Standardized Neo4j database name
- **`cyphers_path`**: Path to drift_search_cyphers.txt script

### Function-Scoped Fixtures

Fixtures initialized per test:

- **`auth_headers`**: JWT Bearer token for backend service authentication (LOCAL_JWT_SECRET)
- **`postgres_connection`**: PostgreSQL connection with automatic cleanup
- **`neo4j_driver`**: Neo4j driver with automatic cleanup
- **`redis_client`**: Redis client with automatic cleanup
- **`test_pdf_content`**: Test PDF document bytes
- **`unique_test_filename`**: Unique filename per test (UUID-based)
- **`test_project_data`**: Test project data with unique name
- **`project_manager`**: Context manager for project creation and cleanup
- **`redis_monitor`** / **`celery_task_monitor`**: Redis pub/sub monitoring
- **`wa`**: WorkflowAssertions facade instance

### Auto-Use Fixtures

- **`ensure_clean_session_setup`**: Resets Neo4j database before each test and recreates schema

### ProjectManager Context Manager

Ensures proper cleanup of test data even if tests fail:

```python
@pytest.fixture
def project_manager(...) -> Generator[ProjectManager, None, None]:
    manager = ProjectManager(...)
    try:
        yield manager
    finally:
        manager.cleanup()  # Always cleans up PostgreSQL and Neo4j
```

**Features:**
- Automatic project cleanup from PostgreSQL
- Automatic graph cleanup from Neo4j (by project_id)
- Prevents test data pollution across test runs

## Authentication

Tests use **LOCAL JWT tokens** (not real Azure AD tokens) for backend service authentication:

```python
def _create_local_jwt_token(oid: str = None, roles: list = None, username: str = None) -> str:
    """
    Create LOCAL JWT token for backend service authentication.
    
    Backend services validate tokens using LOCAL_JWT_SECRET (shared secret),
    NOT Azure AD tokens. This mimics what UI service does when minting S2S tokens.
    """
    secret = os.getenv("LOCAL_JWT_SECRET", "dev-local-jwt-secret")
    claims = {
        "oid": oid or str(uuid.uuid4()),
        "preferred_username": username or "test.user@example.com",
        "roles": roles or ["Admin", "User"],
        "iss": "ui-service",
        "iat": now,
        "exp": now + 3600,
    }
    return jwt.encode(claims, secret, algorithm="HS256")
```

UI workflow tests simulate the full SSO flow with mock Azure AD for end-user authentication.

## Running Tests

### Prerequisites

1. **All services must be running (locally or CI/CD)**:
   ```bash
   docker-compose up -d
   docker-compose up openai-mock-service -d
   ```

2. **Services must be healthy** (tests will wait and skip if not):
   - PostgreSQL schema initialized
   - Neo4j constraints and indexes created
   - All microservices responding to health checks

### Run All Tests

```bash
# In docker-compose (recommended)
docker-compose --profile e2e-tests up e2e-tests

# Local development
cd e2e_tests
python -m venv venv
pip install -e .[dev]
pytest -s
```

### Run Specific Test Modules

```bash
# Service health tests only
pytest src/test_services_health.py

# Document workflow tests only
pytest src/test_document_workflow.py

# UI workflow tests only
pytest src/test_ui_workflow.py

# Neo4j retrieval tests only
pytest src/test_neo4j_drift_search.py
```

### Run and Generate JUnit XML

```bash
pytest src/ --junitxml=test-results/results.xml
```

## Docker Integration

### Multi-Stage Dockerfile

```dockerfile
FROM python:3.13-slim-bookworm AS builder
# Install dependencies in isolated venv

FROM python:3.13-slim-bookworm AS final
# Copy venv and source code
# Run tests and record results
# Keep container alive for healthcheck
```

**Features:**
- Multi-stage build for minimal final image
- Isolated Python virtual environment
- Test results persisted to `/e2e-tests/test-results/results.xml`
- Container keeps running after tests for healthcheck verification
- Success/failure tracked with `/tmp/e2e_success` or `/tmp/e2e_failure`

### Healthcheck

```dockerfile
HEALTHCHECK --interval=30s --timeout=10s --start-period=300s --retries=3 \
    CMD test -f /tmp/e2e_success || exit 1
```

Container reports healthy only when tests pass, enabling automated CI/CD validation.

### Docker Compose Integration

```yaml
e2e-tests:
  build: ./e2e_tests
  environment:
    # Override service URLs to use internal docker network hostnames
    - PROJECT_MANAGEMENT_SERVICE_URL=http://project-management-service:8000
    - DOCUMENT_PROCESSING_URL=http://document-processing-service:8000
    # ... all other services
  env_file:
    - docker-compose.env
  depends_on:
    # All required services with health checks
    postgres:
      condition: service_healthy
    neo4j:
      condition: service_healthy
    # ... 13+ service dependencies
  volumes:
    - ./e2e_tests/test-results:/e2e-tests/test-results
  restart: "no"
  profiles:
    - e2e-tests
```

**Key Features:**
- Service discovery via Docker network hostnames
- Health check dependencies ensure services ready before tests
- Test results volume-mounted for external access
- Optional profile for on-demand execution

## Development Workflow

### Adding New Tests

1. **Identify the domain**: Document, workflow, database, neo4j, or validation
2. **Reuse existing fixtures**: Use `conftest.py` fixtures and `WorkflowAssertions` facade
3. **Add test function/class** to appropriate test module
4. **Follow existing patterns**: Use context managers, monitor Redis pub/sub, validate responses
5. **Update this README** with new test documentation

### Adding New Service Operations

1. **Choose appropriate module**: `neo4j/`, `database/`, `document/`, `workflow/`, `validators/`
2. **Add function to module**: Follow SRP - one function, one responsibility
3. **Update facade if needed**: Add delegation method to `WorkflowAssertions`
4. **Write tests**: Ensure new operations are covered

### Debugging Failed Tests

1. **Check service health**: `pytest src/test_services_health.py -v`
2. **Inspect test results**: `cat test-results/results.xml`
3. **Check container logs**:
   ```bash
   docker-compose logs e2e-tests
   docker-compose logs <service-name>
   ```
4. **Run specific test with verbose output**:
   ```bash
   pytest src/test_document_workflow.py::TestDocumentWorkflow::test_complete_document_processing_workflow -v -s
   ```
5. **Check Redis messages**: RedisTestMonitor captures all pub/sub messages
6. **Check database state**: Use pgAdmin (port 8080) or Neo4j Browser (port 7474)

## Shared Utilities (`shared_utils.py`)

Common utility classes to eliminate code duplication:

### HTTPUtils

- `make_request_with_retry()`: HTTP requests with automatic retry logic
- `check_service_health()`: Single service health check
- `wait_for_service_health()`: Wait for service to become healthy

### ServiceHealthChecker

- `check_all_services_health()`: Batch health check for all services
- `wait_for_all_services()`: Block until all services healthy or raise error

### DatabaseUtils

- `execute_query_with_retry()`: Database queries with retry logic
- `verify_table_exists()`: Table existence validation

### ProjectTestUtils

- `build_project_url()`: Standardized project URL builder
- `build_upload_url()`: Document upload URL builder

## Test Coverage

The test suite validates:

### Infrastructure
- ✅ PostgreSQL connectivity and schema
- ✅ Neo4j connectivity and operations
- ✅ Redis connectivity and pub/sub
- ✅ Azure Blob Storage emulation
- ✅ Service health endpoints

### Document Processing Pipeline
- ✅ Project creation
- ✅ Single document upload
- ✅ Multiple document upload
- ✅ Celery task publishing
- ✅ Redis pub/sub progress messages
- ✅ Document parsing and chunking
- ✅ Neo4j graph ingestion
- ✅ Vector embedding generation
- ✅ Status transitions (processing → active → rag_processing → rag_ready)
- ✅ Error handling (invalid uploads, missing projects)

### Knowledge Graph
- ✅ Project node creation
- ✅ Document node creation
- ✅ Chunk node creation
- ✅ Relationships (PART_OF, BELONGS_TO)
- ✅ Constraints (uniqueness, existence)
- ✅ Vector indexes
- ✅ Named indexes

### AI Workflows
- ✅ Requirements generation
- ✅ Backlog generation
- ✅ GitLab integration
- ✅ Context retrieval (DRIFT algorithm)
- ✅ AI progress monitoring
- ✅ Response structure validation
- ✅ Content quality validation
- ✅ Mock content detection

### UI Service
- ✅ SSO authentication flow
- ✅ GitLab OAuth connection
- ✅ Session management
- ✅ Proxy endpoint routing
- ✅ Error handling

### Integration Points
- ✅ Service-to-service communication
- ✅ Database consistency
- ✅ Message queue reliability
- ✅ External API mocking

## Troubleshooting

### Tests Fail with "Service not healthy"

**Solution**: Ensure all services are running and healthy:
```bash
docker-compose ps
docker-compose logs <service-name>
```

Wait for all services to report healthy status in their logs.

### Tests Fail with "Connection refused"

**Solution**: Check that service URLs match your environment:
- Local development: Uses `localhost` URLs
- Docker environment: Uses internal network hostnames

Verify `docker-compose.env` configuration matches your setup.

### Tests Hang on Status Monitoring

**Solution**: Check Redis pub/sub messages:
```python
# Add debug logging
redis_monitor.start_ui_progress_monitoring(project_id)
print(redis_monitor.ui_messages_received)  # Inspect captured messages
```

Verify that services are actually publishing messages to Redis channels.

**Note**: The monitoring methods use synchronization barriers to prevent race conditions. If tests still hang:
- Verify Redis is accessible and pub/sub is working: `redis-cli ping`
- Check that publisher services are running and healthy
- Ensure no firewall blocking Redis port 6379
- See `PUBSUB_RACE_CONDITION_FIX.md` for detailed technical analysis

### Neo4j Tests Fail

**Solution**: Ensure Neo4j schema is initialized:
```bash
# Check Neo4j logs
docker-compose logs neo4j-maintenance-service
docker-compose logs neo4j-schema-init

# Manually trigger schema initialization
curl -X POST http://localhost:8002/init-neo4j \
  -H "Authorization: Bearer <token>"
```

### Authentication Errors

**Solution**: Verify `LOCAL_JWT_SECRET` is set consistently:
```bash
# Check docker-compose.env
grep LOCAL_JWT_SECRET docker-compose.env

# Verify services can decode tokens
docker-compose exec project-management-service env | grep LOCAL_JWT_SECRET
```

### Docker Build Fails

**Solution**: Clear Docker build cache and rebuild:
```bash
docker-compose build --no-cache e2e-tests
```

## Performance Considerations

- **Test Isolation**: Each test starts with clean Neo4j database (auto-use fixture)
- **Parallel Execution**: Tests are not parallel-safe due to shared database state
- **Timeouts**: Configurable timeouts in `TestConstants` (default: 30s, processing: 60s)
- **Retry Logic**: HTTP requests and database queries use retry logic for reliability
- **Resource Cleanup**: ProjectManager ensures cleanup even on test failure
- **Pub/Sub Synchronization**: Monitoring startup adds ~3-5ms overhead per test to ensure reliable message capture (prevents race conditions)

## Contributing

When contributing tests:

1. **Follow SOLID principles**: Keep modules focused and responsibilities clear
2. **Reuse existing code**: Check for existing utilities before writing new ones
3. **Add comprehensive docstrings**: Explain what, why, and how
4. **Update this README**: Document new tests and features
5. **Validate locally**: Run full test suite before committing
6. **Check linting**: Ensure code follows project style (PEP 8)
7. **Avoid timing assumptions**: Use synchronization barriers for thread coordination, never assume pub/sub subscriptions are instant

## License

This test suite is part of the larger microservices project and follows the same license.

---

**Last Updated**: 2025-10-07
**Test Framework**: pytest 7.0+
**Python Version**: 3.11+

