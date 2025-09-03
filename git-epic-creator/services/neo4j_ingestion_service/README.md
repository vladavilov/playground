## Neo4j Ingestion Service — Neo4j GraphRAG Python Pipeline Orchestrator

This service orchestrates a project-scoped ingestion using the Neo4j GraphRAG Python pipeline and writes results directly to Neo4j. It consumes trigger messages from Redis Streams, downloads JSON inputs from Azure Blob, runs the in-process pipeline, ensures a vector index, and updates the Project Management Service with progress and final status.

### Scope and responsibilities
- Accept a trigger message with `job_id`, `project_id`, `attempts` via Redis Streams.
- Download project-scoped `.json` documents from Azure Blob into `RAG_WORKSPACE_ROOT/{project_id}/input/` (prefix: `output/`).
- Run the Neo4j GraphRAG Python pipeline (`SimpleKGPipeline`) to construct a knowledge graph directly in Neo4j.
- Ensure a Neo4j vector index exists (1536 dims, cosine) for embeddings.
- Update Project Management Service status to `rag_processing` at start, `rag_ready` on success, or `rag_failed` on error.

Reference: Neo4j GraphRAG (Python pipeline)

### Architecture flow
```mermaid
sequenceDiagram
    autonumber
    participant P as Project Mgmt Service
    participant D as Document Processing Service
    participant Q as Redis Streams (trigger)
    participant W as Ingestion Subscriber
    participant C as Celery Worker
    participant B as Azure Blob Storage
    participant G as GraphRAG Python Pipeline
    participant N as Neo4j

    P->>D: Publish task request to task_streams:document_processing {task_type, project_id}
    Note over D: Document Processing writes output/*.json and publishes ingestion.trigger
    Q-->>W: XREADGROUP stream=ingestion.trigger {job_id, project_id, attempts}
    W->>C: apply_async run_graphrag_job(job_id, project_id, attempts)
    C->>B: List prefix "output/"; download *.json → RAG_WORKSPACE_ROOT/{project_id}/input/
    C->>G: run_documents(driver, documents) with schema + LLM/embeddings
    G->>N: Writes nodes/relationships + embeddings; ensures vector index
    C->>P: PUT /projects/{project_id}/status: rag_processing → rag_ready | rag_failed
    W-->>Q: XACK message
```

### Message contract (Redis Streams)
- Stream key: `ingestion.trigger` (consumer group: `neo4j_ingestor`, consumer name configurable)
- Message shape:
```json
{
  "job_id": "<uuid>",
  "project_id": "<uuid>",
  "attempts": 0
}
```
- Dead-letter stream: `ingestion.trigger.deadletter` after max attempts.

### Inter-service integration
- Upstream producer: `document_processing_service`
  - Writes per-document JSONs to project container under `output/` prefix with fields `title`, `text`, and filtered `metadata`.
  - Publishes `ingestion.trigger` entries with fields `job_id`, `project_id`, `attempts` via shared `models.ingestion_messages`.
- This service consumes `ingestion.trigger`, downloads the `output/*.json` payloads, and ingests them into Neo4j via the GraphRAG Python pipeline.

### Filesystem layout (inside container)
- `RAG_WORKSPACE_ROOT` (default `./graphrag` via local script)
  - `{project_id}/input/*.json` — downloaded inputs
  - `{project_id}/.lock` — lock file to prevent concurrent runs
  - Workspace is cleaned after successful pipeline execution

### GraphRAG input JSON expectations (per document)
Upstream produces JSON documents; the service injects `project_id` and enriches metadata if present.
```json
{
  "title": "<title>",
  "text": "<full text or structured JSON>",
  "metadata": {
    "file_name": "<original filename>",
    "file_type": "<pdf|docx|...>",
    "content_type": "<mime>",
    "creation_date": "2024-01-01T00:00:00Z",
    "modification_date": "2024-01-02T00:00:00Z"
  }
}
```

### Runtime dependencies
- `neo4j-graphrag` (GraphRAG Python pipeline)
- `neo4j` (official Python driver)
- `azure-storage-blob` (download JSONs)
- `structlog`, `fastapi`, `celery`, `redis`

### Environment configuration
- GraphRAG / LLM (from `config` in this service; shared LLM config at `shared/configuration/llm_config.py`)
  - `OAI_KEY` (API key)
  - `OAI_BASE_URL` (for Azure or custom gateway). When paired with `OAI_API_VERSION`, Azure mode is used.
  - `OAI_API_VERSION` (e.g., `2024-06-01`)
  - `OAI_MODEL` (chat model)
  - `OAI_EMBED_MODEL` (embedding model)
- Azure Blob
  - `AZURE_STORAGE_CONNECTION_STRING`
  - `AZURE_STORAGE_CONTAINER_NAME`
  - `AZURE_STORAGE_BLOB_ENDPOINT`
- Paths
  - `RAG_WORKSPACE_ROOT` (e.g., `./graphrag`)
- Messaging
  - `REDIS_URL`
  - `CELERY_BROKER_URL`, `CELERY_RESULT_BACKEND`
- Neo4j
  - `NEO4J_URI`, `NEO4J_USERNAME`, `NEO4J_PASSWORD`, `NEO4J_DATABASE`
- Project Management
  - `PROJECT_MANAGEMENT_SERVICE_URL`

### GraphRAG pipeline and vector index
- Uses `SimpleKGPipeline` to build a KG directly in Neo4j; no CLI, `settings.yaml`, or parquet artifacts are used.
- A default finance-oriented schema is provided with permissive extensions.
- Vector index is ensured via `neo4j_graphrag.indexes.create_vector_index`:
  - name: `graphrag_index`, label: `Chunk`, property: `embedding`, dimensions: `1536`, similarity: `cosine`.

### Subscriber and retry/DLQ
- Subscriber (`subscribers/task_subscriber.py`) consumes stream `ingestion.trigger` and enqueues the Celery task on queue `neo4j_ingestion` with args `[job_id, project_id, attempts]`.
- Retry policy uses shared backoff logic; retries published back to `ingestion.trigger`, DLQ to `ingestion.trigger.deadletter`.

### Orchestration steps (Celery task `tasks.neo4j_ingestion.run_graphrag_job`)
1) Create `RAG_WORKSPACE_ROOT/{project_id}/input` and lightweight `.lock` to prevent concurrent runs.
2) List Blob files with prefix `output/` and download `.json` files to the input directory.
3) Prepare documents (inject `project_id`, carry forward metadata), ensure Neo4j vector index.
4) Run the Neo4j GraphRAG pipeline with accumulated documents.
5) On success, mark project `rag_ready` with counts; on failure, `rag_failed` with error; release lock and clean workspace on success.

### Health
- `GET /health/celery` exposes discovered vs expected tasks and registration status.

### Operational notes
- Backpressure: keep 1–2 in-flight tasks per consumer; `acks_late=true`, `worker_prefetch_multiplier=1`.
- Resiliency: retries via backoff; DLQ to `ingestion.trigger.deadletter`.
- Observability: structured logs for blob → pipeline → neo4j; capture counts and durations.
- Cleanup: workspace is removed on success; lock is always released.

### Docker and Compose
- Image should install: `neo4j-graphrag`, `azure-storage-blob`, plus service deps.
- Run a dedicated Celery worker for the `neo4j_ingestion` queue; Windows workers use `--pool=solo`.
- Healthcheck: `GET /health/celery`.

### Local development quickstart
1) Start Docker Compose services (from `playground/git-epic-creator`):
   - Suggested services: `neo4j redis azurite project-management-service openai-mock-service mock-auth-service`
   - Example:
     ```bash
     docker compose up -d neo4j redis azurite project-management-service openai-mock-service mock-auth-service
     ```
2) In a Python 3.11 venv, install shared + this service (from `playground/git-epic-creator/services`):
   ```bash
   python -m venv venv
   source venv/bin/activate  # Windows: .\venv\Scripts\Activate.ps1
   pip install -U pip
   pip install -e ./shared
   pip install -e ./neo4j_ingestion_service[dev]
   ```
3) Run the service locally (Windows PowerShell):
   ```powershell
   # From service dir: playground/git-epic-creator/services/neo4j_ingestion_service
   powershell -ExecutionPolicy Bypass -File .\scripts\run_local.ps1
   ```
   - The script exports envs pointing to localhost ports and starts FastAPI + a local Celery worker + the subscriber.
4) Validate health:
   ```bash
   curl http://localhost:8000/health/celery
   ```
5) Trigger an ingestion job (from service dir):
   ```powershell
   # Replace with a real project UUID; blob container (per-project when UUID) must contain JSONs under prefix "output/"
   powershell -ExecutionPolicy Bypass -File .\scripts\trigger_ingestion.ps1 -ProjectId 11111111-1111-1111-1111-111111111111
   ```
6) Inspect logs; verify GraphRAG runs, Neo4j constraints/index are created, counts returned, and project status updated.

### Acceptance criteria
- On successful completion of the Celery task for a project, all input JSON documents are processed through the Neo4j GraphRAG Python pipeline into Neo4j, the vector index exists, and Project status is set to `rag_ready` with counts.

### References
- Neo4j GraphRAG (Python pipeline)

### Implementation notes
- FastAPI app and health: `src/main.py` (via `FastAPIFactory`), includes `/health/celery`.
- Celery worker app: `src/worker/celery_app.py` configures routing and acks; tasks are under `tasks.neo4j_ingestion.*`.
- Subscriber: `src/subscribers/task_subscriber.py` wraps shared `TaskStreamSubscriber` and wires args/order/queue.
- Ingestion service: `src/services/ingestion_service.py` orchestrates blob listing/downloading and calls `ingestion/graphrag_pipeline.py`.
- Retry/DLQ helpers: `src/tasks/retry.py` publish retries/DLQ entries back to Redis Streams.


