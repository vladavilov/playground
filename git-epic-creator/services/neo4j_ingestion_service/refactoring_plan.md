## Neo4j Ingestion Service Refactoring Plan — GraphRAG Orchestration

### Goal
Align the service with the new GraphRAG-triggered ingestion flow described in `README.md`, replacing the old “documents-in-message + local chunking” approach with “stream trigger + GraphRAG CLI orchestration + parquet→Neo4j”.

### High-level changes
- **Replace placeholder pipeline**: Remove local text chunking/entity stubs and implement GraphRAG CLI orchestration in the Celery task.
- **New Redis Streams contract**: Switch to `ingestion.trigger` messages of shape `{job_id, project_id, attempts}` (no `documents` in stream).
- **Blob → GraphRAG → Parquet → Neo4j**: Download JSON inputs from Blob to per-project workspace, run GraphRAG, load parquet outputs, MERGE into Neo4j, update project status.
- **Subscriber wiring**: `TaskStreamSubscriber` should enqueue Celery jobs with `[job_id, project_id, attempts]` only.
- **Config/compose**: Add GraphRAG env vars and a workspace volume; ensure Celery worker/subscriber run at runtime.
- **Remove dead code paths**: Eliminate the now-unused “documents-in-stream” flow.

### File-by-file changes

#### 1) `services/neo4j_ingestion_service/src/tasks/ingestion_tasks.py`
Current signature expects `documents` and implements placeholder chunking/entities; it must be replaced per README.

- Change task signature to remove `documents`:
  - `def run_graphrag_job(self, job_id: str, project_id: str, attempts: int = 0) -> Dict[str, Any]`
- Remove imports and logic tied to local chunking/entity extraction (e.g., `neo4j_ingestion_utils`, `semantic_chunk_text`, `hash_text_unit`).
- Implement orchestration steps:
  - Ensure `${RAG_WORKSPACE_ROOT}/input/{project_id}` and `${RAG_WORKSPACE_ROOT}/output/{project_id}` exist.
  - Use `BlobStorageClient` (from shared utils) to list and download JSON inputs derived from `AZURE_BLOB_PREFIX_TEMPLATE` into `input/{project_id}`.
  - Initialize and patch GraphRAG `settings.yaml` (per-project input/output). Delegate workspace init + CLI invocation to a small `graphrag_runner.py` helper.
  - Run `graphrag index --root ${RAG_WORKSPACE_ROOT}` via `subprocess`; validate exit code.
  - Load parquet artifacts from `output/{project_id}` via `pyarrow` and map to rows.
  - Upsert rows into Neo4j using `Neo4jClient` with idempotent MERGE; create HNSW vector indexes (1536/cosine) if missing.
  - Update Project Mgmt Service status to `rag_ready` on success; `rag_failed` on error.
- Preserve `acks_late=True`, strong structured logging, and timing metrics. Return value should include counts and duration for observability.

Add new local helper module:
- `services/neo4j_ingestion_service/src/graphrag_runner.py`
  - Responsibilities: workspace init, `settings.yaml` templating (input/output paths, Azure OpenAI settings), and `graphrag` CLI invocation with exit-code check.

#### 2) `services/neo4j_ingestion_service/src/task_subscriber.py`
Wrapper currently uses old stream key and default args mapping that includes `documents`.

- Change `stream_key` to `ingestion.trigger`.
- Provide `build_apply_kwargs` to pass only `[job_id, project_id, attempts]` and coerce `attempts` to `int`:
  - `args: [fields.get("job_id"), fields.get("project_id"), int(fields.get("attempts", 0) or 0)]`
- Keep `consumer_group="neo4j_ingestor"`, `consumer_name="worker"`, and `queue="neo4j_ingestion"`.

#### 3) `services/shared/src/models/ingestion_messages.py`
`IngestionJobMessage` currently requires `documents`. Under the new contract, the trigger message contains no documents.

- Add a new model `IngestionTriggerMessage` with fields: `job_id: str`, `project_id: UUID` (or `str` if preferred), `attempts: int = 0`.
- If no other code relies on `IngestionJobMessage`, remove it (and related tests). Otherwise, keep both, but prefer the trigger model for the stream contract.

#### 4) `services/shared/src/utils/task_stream_subscriber.py`
Already supports `build_apply_kwargs` and JSON decoding; no required functional changes.

Optional improvements:
- Document usage example for ingestion trigger contract in the class docstring.
- Make default arg mapping resilient if `documents` is absent (to avoid accidental arg mismatch in services that forget to pass `build_apply_kwargs`).

#### 5) `services/shared/src/utils/unified_redis_messages.py`
Not used by the `ingestion.trigger` stream (simple field entries). No changes required. Optionally clarify in comments that this module targets pub/sub or other stream patterns.

#### 6) `services/shared/src/utils/redis_abstractions.py`
Core pub/sub/streams abstractions; no changes required for the new ingestion flow.

### Environment and Compose updates

#### `playground/git-epic-creator/docker-compose.env`
Add GraphRAG and workspace configuration:
- `GRAPHRAG_API_KEY=<key>`
- For Azure OpenAI via settings templating (if used):
  - `GRAPHRAG_API_BASE=https://<instance>.openai.azure.com`
  - `GRAPHRAG_API_VERSION=2024-02-15-preview`
  - `GRAPHRAG_CHAT_DEPLOYMENT=<chat_deployment_name>`
  - `GRAPHRAG_EMBED_DEPLOYMENT=<embedding_deployment_name>`
- `RAG_WORKSPACE_ROOT=/app/graphrag`
- `AZURE_BLOB_PREFIX_TEMPLATE=projects/{project_id}/graphrag-input`

Keep existing Azure Blob, Redis, Neo4j, and Project Mgmt envs.

#### `playground/git-epic-creator/docker-compose.yml`
Ensure the ingestion pipeline runs end-to-end with a Celery worker and subscriber and persists GraphRAG workspace.

- In `neo4j-ingestion-service`:
  - Add dependencies: `redis` (healthy) and `azurite` (started), in addition to `neo4j-maintenance-service`.
  - Mount a volume for the GraphRAG workspace: `- graphrag_workspace:/app/graphrag`.
  - Keep `env_file: docker-compose.env` for new envs.
- Add a dedicated Celery worker service (recommended):
  - `neo4j-ingestion-worker`: same build context; command `celery -A neo4j_ingestion_service.src.celery_worker_app worker -Q neo4j_ingestion --loglevel=INFO`.
  - `depends_on`: `redis`, `neo4j-maintenance-service`, `azurite`.
  - Share network and (optionally) the `graphrag_workspace` volume.
- Subscriber process:
  - Option A: Start `TaskStreamSubscriber` in the API process on startup (as per README Option A).
  - Option B: Create a minimal `neo4j-ingestion-subscriber` service running an entrypoint that calls `start_listening()`.
  - Choose one to avoid duplicate consumption.
- Define the volume at top-level: `graphrag_workspace:`.

### Additional files and Docker image updates
- Add `services/neo4j_ingestion_service/src/graphrag_runner.py` (workspace init, settings templating, CLI invocation).
- Add helper(s) for parquet→Neo4j mapping (either in this service or shared utils) to batch `UNWIND` MERGE nodes/relationships and create vector indexes.
- Update `services/neo4j_ingestion_service/Dockerfile` to install `graphrag`, `pyarrow` (or `pandas[parquet]`), and ensure `curl` is present for health checks.

### Tests
- Remove/replace old text chunking tests.
- Add tests for:
  - `graphrag_runner` settings templating and CLI invocation args.
  - parquet→Neo4j mapping (row shape and idempotent MERGE semantics).
  - Subscriber `build_apply_kwargs` mapping `{job_id, project_id, attempts}` → Celery args, including `attempts` coercion to `int`.
- Update any integration tests that assumed documents were delivered via the stream.

### Assumptions
- The trigger stream is the single source of truth; inputs are fetched from Blob.
- Old “documents-in-stream” path will be removed to avoid drift and dead code.
- Worker/subscriber will run as separate containers unless embedding subscriber into API process is preferred.

### Risks/Impacts
- Code relying on `IngestionJobMessage.documents` will break; remove or update accordingly.
- Without a worker/subscriber process, tasks will not execute; compose must include them.
- Docker image must include `graphrag` and `pyarrow` for runtime.

### Deliverables
- Edited files: `ingestion_tasks.py`, `task_subscriber.py`, `docker-compose.env`, `docker-compose.yml`.
- New file(s): `graphrag_runner.py` (+ optional parquet→Neo4j helpers).
- Model updates in `ingestion_messages.py` (add trigger model; remove or deprecate unused job model and tests if not referenced).

## Document Processing Service Refactor (to support GraphRAG-triggered flow)

The document processing service should stop publishing document payloads to Redis and instead:
- Persist per-document GraphRAG input JSON files to Azure Blob Storage under a project-specific prefix.
- Publish a minimal trigger to `ingestion.trigger` with `{ job_id, project_id, attempts }` once extraction completes.

### Files to update

#### `services/document_processing_service/src/services/ingestion_job_publisher.py`

Current behavior: publishes `IngestionJobMessage` with `documents` to `ingestion.jobs`.

Required changes:
- Repurpose to a trigger publisher (or replace with a new `IngestionTriggerPublisher`).
  - Change `stream_key` default to `"ingestion.trigger"`.
  - Remove dependency on `IngestionJobMessage` and `models` entirely.
  - Publish fields only: `job_id` (string), `project_id` (string UUID), `attempts` (stringified int, default `"0"`).
  - Remove `documents` field from `xadd` payload.
  - Update logging fields accordingly.

Example new publish signature and fields:
```python
async def publish(self, job_id: str, project_id: str, attempts: int = 0) -> str:
    fields = {
        "job_id": job_id,
        "project_id": project_id,
        "attempts": str(int(attempts) if attempts is not None else 0),
    }
    return await self.redis_client.xadd(self.stream_key, fields)
```

Removal note:
- If the class name is retained (`IngestionJobPublisher`), rename docstrings and log messages to “trigger” to avoid confusion. Alternatively, rename the class to `IngestionTriggerPublisher` and update imports.

#### `services/document_processing_service/src/tasks/document_core.py`

Current behavior: extracts text, accumulates `documents_for_ingestion` in memory, and returns them. Cleans up original blobs.

Required changes:
- Write GraphRAG input JSON to Azure Blob for each processed document rather than returning payloads in-memory.
  - JSON shape per README:
    - `{ "id": "<doc_id>", "title": "<title>", "text": "<full text>", "creation_date": "<ISO8601>", "sources_url": "https://...", "metadata": { ... } }`
  - At minimum: `id`, `title`, `text`, optional `creation_date`, optional `metadata`.
  - Use a deterministic `id` if upstream `id` is missing (e.g., sha256 of `project_id|basename|index`).
- Persist JSONs under a project-scoped prefix so the ingestion service can download them:
  - Honor `AZURE_BLOB_PREFIX_TEMPLATE` (e.g., `projects/{project_id}/graphrag-input`) to derive the write path.
  - If `BlobStorageClient` lacks a JSON upload method, write to a temp file and use existing `upload_file` or add a helper `upload_json(project_id, blob_name, data)`.
- Keep existing cleanup of original source blobs after extraction (as implemented), but do not delete the newly created JSON inputs.
- Adjust return value:
  - Remove `documents_for_ingestion` from the returned dict.
  - Include counts and optionally the destination prefix for observability: `{ ..., "ingestion_input_prefix": <derived prefix> }`.

Implementation notes:
- Maintain progress updates via `send_progress_update` as-is.
- Continue to use injected collaborators; add an `upload` capability to the blob client if needed.

#### `services/document_processing_service/src/tasks/document_tasks.py`

Current behavior: after processing, converts payloads to `DocumentForIngestion`, builds `IngestionJobMessage`, and publishes to `ingestion.jobs` via `IngestionJobPublisher`.

Required changes:
- Remove imports and usage of `DocumentForIngestion`, `IngestionJobMessage`, and the old `IngestionJobPublisher` contract.
- After successful processing, publish a minimal trigger to Redis Streams `ingestion.trigger`:
  - Use a publisher as refactored above, or directly call `xadd` via `get_redis_client()`.
  - Fields: `job_id` = Celery `self.request.id` (or a generated UUID), `project_id` = task arg, `attempts` = `"0"`.
- Keep existing progress update logic and structured logging.
- Update docstrings to reflect the new flow: “extract documents → write GraphRAG JSON inputs to Blob → emit trigger for GraphRAG ingestion”.

Example trigger publish (inline):
```python
publisher = IngestionTriggerPublisher(get_redis_client())
asyncio.run(publisher.publish(job_id=self.request.id, project_id=project_id, attempts=0))
```

### Tests to update
- Remove tests that assert publishing of `documents` in stream entries.
- Add tests that validate:
  - JSON files are written to Blob with correct schema and prefix.
  - Trigger publishing contains only `{job_id, project_id, attempts}` to `ingestion.trigger`.
  - The task returns counts without `documents_for_ingestion`.

### Cross-service contract alignment
- Ensure `neo4j_ingestion_service` subscriber expects `ingestion.trigger` and maps only `[job_id, project_id, attempts]` to `run_graphrag_job`.
- Ensure `neo4j_ingestion_service` Celery task signature is `run_graphrag_job(job_id: str, project_id: str, attempts: int = 0)`.

