## Neo4j Ingestion Service — Implementation Task List

Each task is small, TDD-friendly, and references the exact parts of `../README.md`. Check off when complete.

- [x] Task 1 — Service skeleton and worker wiring
  - Description: Create `src/celery_worker_app.py` and `src/main.py`; init Celery app; import ingestion tasks; expose `/health`; set queue/prefetch/acks.
  - Doc refs: README L88–89, L297–303
  - Example: Queue `neo4j_ingestion`, `acks_late=True`, `worker_prefetch_multiplier=1`.
  - Acceptance:
    - `GET /health` returns 200
    - Celery app loads with queue `neo4j_ingestion`, `acks_late=True`, prefetch=1

- [x] Task 2 — Shared ingestion message schemas
  - Description: Add `DocumentForIngestion` and `IngestionJobMessage` models in `services/shared/src/models/`, export via `__init__.py`.
  - Doc refs: README L351–360
  - Example: `DocumentForIngestion(title: str, text: str, id?: str, creation_date?: datetime, metadata?: dict)`.
  - Acceptance:
    - Models importable from `services.shared.models`
    - Pydantic validation tests pass for valid/invalid payloads

- [x] Task 3 — Generalized Redis Streams subscriber
  - Description: Implement `services/shared/src/utils/task_stream_subscriber.py` using Streams group consumption; enqueue Celery tasks; `XACK` on success; support claiming.
  - Doc refs: README L290–296, L361–366, L67–73
  - Example: `TaskStreamSubscriber(stream_key="ingestion.jobs", consumer_group="neo4j_ingestor", consumer_name="ingestor-1", task=run_graphrag_job)`.
  - Acceptance:
    - Messages on `ingestion.jobs` → `apply_async(..., queue="neo4j_ingestion")`
    - Successful enqueues are `XACK`ed; idle claims work

- [x] Task 4 — Ingestion job publisher helper
  - Description: Add `IngestionJobPublisher` in shared utils to publish `IngestionJobMessage` to `ingestion.jobs`.
  - Doc refs: README L371–375
  - Example: `publisher.publish(job) -> message_id`.
  - Acceptance:
    - Publishes schema-conformant entries to `ingestion.jobs`
    - Round-trip test: published message consumable by Task 3 subscriber

- [x] Task 5 — Wire Document Processing → Ingestion dispatch
  - Description: Map outputs in `document_processing_service` to `DocumentForIngestion` and publish one `IngestionJobMessage` per project.
  - Doc refs: README L34–45, L371–375
  - Example: filename→title; extracted text→text; metadata propagated.
  - Acceptance:
    - Correct `project_id`, `job_id`, and document count in published message
    - Invalid documents rejected by schema with clear errors

- [x] Task 6 — Celery task `run_graphrag_job`
  - Description: Implement `tasks.neo4j_ingestion.run_graphrag_job(job_id, project_id, documents, attempts=0)` with progress hooks; stub GraphRAG initially.
  - Doc refs: README L79–87, L90–97, L298–301
  - Example: Return artifacts dict: `documents`, `text_units`, `entities`, `relationships`, `communities`, `community_reports`.
  - Acceptance:
    - Accepts message schema; returns summary result
    - On failure raises to trigger retry; success/failure updates project status (Task 11)

- [x] Task 8 — Neo4j client enhancements and vector index creation
  - Description: Migrate stream contract and subscriber to the new trigger flow; introduce trigger message schema.
  - Subtasks:
    - Shared models: add `IngestionTriggerMessage(job_id: str, project_id: str, attempts: int = 0)` in `services/shared/src/models/ingestion_messages.py`. Keep `IngestionJobMessage` only if still referenced elsewhere.
    - Subscriber wrapper: update `neo4j_ingestion_service/src/task_subscriber.py` to use `stream_key="ingestion.trigger"` and pass only `[job_id, project_id, attempts]` via `build_apply_kwargs` with `attempts` coerced to `int`.
  - Doc refs: README L36–47 (message contract), L190–211 (subscriber wrapper example); Refactoring plan L36–43, L44–56
  - Acceptance:
    - Consuming a trigger entry with fields `{job_id, project_id, attempts}` enqueues `run_graphrag_job(job_id, project_id, attempts)` on `queue="neo4j_ingestion"`
    - Successful enqueue results in `XACK`; `attempts` is `int`

- [x] Task 9 — Ingest functions for nodes and relationships
  - Description: Add `graphrag_runner` to orchestrate per-project GraphRAG runs safely in parallel; implement Blob→workspace input sync; import GraphRAG parquet outputs into Neo4j with idempotent batched upserts.
  - Subtasks:
    - Graphrag workspace isolation and CLI runner
      - Create `neo4j_ingestion_service/src/graphrag_runner.py` with a function `run_index(project_id: str) -> Path` that:
        - Computes per-project workspace: `WORKDIR = ${RAG_WORKSPACE_ROOT}/${project_id}` (must be unique per project to allow safe concurrent runs).
        - Initializes workspace if missing: `graphrag init --root "${WORKDIR}"` (idempotent if already initialized; use `--force` only during migrations).
        - Ensures `settings.yaml` under `WORKDIR` points `storage.base_dir` and `reporting.base_dir` to `output` within `WORKDIR` (defaults are already relative; verify and patch only if needed). Avoid mutating any shared `settings.yaml` outside `WORKDIR`.
        - Sets optional internal concurrency limits via env when spawning the CLI process to respect provider limits under parallel Celery runs: `GRAPHRAG_LLM_THREAD_COUNT`, `GRAPHRAG_EMBEDDING_THREAD_COUNT` (e.g., 4–16 depending on capacity).
        - Invokes `graphrag index --root "${WORKDIR}"` and validates non-zero exit codes by raising a typed error (surface stderr/stdout in logs). Do not run multiple CLI processes against the same `WORKDIR` concurrently.
        - Returns `WORKDIR` for downstream import steps.
      - Concurrency safety:
        - By construction, different `project_id` values map to different `WORKDIR`s, allowing multiple Celery tasks to run the CLI concurrently without file contention.
        - If a shared root were ever unavoidable, guard the CLI section with a distributed lock keyed by the root path (Redis lock) so only one index runs per root at a time. Prefer per-project isolation over locking.
    - Blob→workspace input sync
      - Use `services/shared/src/utils/blob_storage.py` to list and download project inputs from Blob into `"${WORKDIR}/input"` using `AZURE_BLOB_PREFIX_TEMPLATE` formatted with `project_id`.
      - Accept JSON/CSV/TXT as supported by GraphRAG input configuration; keep the local files structured as GraphRAG expects (`WORKDIR/input/...`).
    - Parquet→Neo4j import
      - Read GraphRAG output tables from `"${WORKDIR}/output"` using `pyarrow` (or `pandas` backed by `pyarrow`) for: `documents`, `text_units`, `entities`, `relationships`, `communities`, `community_reports`.
      - Ensure Neo4j constraints/indexes exist with idempotent `CREATE CONSTRAINT ... IF NOT EXISTS` before import (labels: `__Document__`, `__Entity__`, `__Community__`; relationship: `RELATED(id)` unique).
      - Upsert via batched Cypher using `UNWIND $rows MERGE ...` with configurable `NEO4J_BATCH_SIZE` (default 1000). Use stable unique identifiers so repeated imports are idempotent.
      - Log per-batch counters; surface totals in the task result for observability.
  - Doc refs: README L48–53 (filesystem), L96–121 (settings), L124–133 (CLI), L135–143 (orchestration), L144–155 (Neo4j mapping); Refactoring plan L23–31, L32–35, L96–99; GraphRAG docs: CLI `init/index --root`, internal concurrency env `GRAPHRAG_LLM_THREAD_COUNT`, `GRAPHRAG_EMBEDDING_THREAD_COUNT`.
  - Operational notes (parallelism):
    - Celery may run multiple `run_graphrag_job` tasks in parallel; safe parallelism requires per-project `WORKDIR` isolation so each Graphrag process writes only within its own root.
    - Keep vector store URIs and any auxiliary storage paths inside `WORKDIR` to avoid cross-run conflicts.
    - Tune Graphrag internal threads per process via env, and Celery worker concurrency at the worker command. Keep `worker_prefetch_multiplier=1` for fairness on long-running jobs.
  - Acceptance:
    - `settings.yaml` under each `WORKDIR` reflects correct relative `input`/`output` (or defaults are validated) and no shared root is mutated.
    - CLI invocation uses `--root ${WORKDIR}`; non-zero exits raise and are logged with stderr/stdout context.
    - Multiple jobs for different `project_id`s can run concurrently without file conflicts; repeated imports are idempotent.
    - Parquet rows import idempotently; `NEO4J_BATCH_SIZE` supports 1000+ rows without timeouts; constraints remain stable across reruns.

- [ ] Task 10 — Dead-letter strategy and exponential backoff
  - Description: Track `attempts`; retry with exponential backoff; move to DLQ after `MAX_ATTEMPTS`; support replay.
  - Doc refs: README L36–47 (message contract, DLQ), L156–160 (operational notes)
  - Example: On failure, increment attempts and re-enqueue with `countdown`.
  - Acceptance:
    - After max attempts, write to `ingestion.trigger.deadletter` and `XACK` original
    - Replay tool re-publishes DLQ entries successfully

- [ ] Task 11 — Project Management Service client and integration
  - Description: Add shared `project_mgmt_client.update_status(project_id, status, processed_count, total_count, error_message=None)`; invoke in task.
  - Doc refs: README L98–103, L386–389
  - Example: On success: `rag_ready`; on failure: `rag_failed` with message.
  - Acceptance:
    - Calls made with base URL from `PROJECT_MGMT_BASE_URL`
    - Status updates verified in tests

- [ ] Task 12 — Configuration keys and defaults in shared config
  - Description: Centralize env keys and defaults for GraphRAG CLI, Blob, Redis Streams, Neo4j batching, and backoff.
  - Doc refs: README L76–95 (environment), L96–121 (settings), L88–95 (paths/messaging)
  - Example keys and defaults:
    - GraphRAG/LLM: `GRAPHRAG_API_KEY`, optional Azure settings (api_base, api_version, deployments)
    - Paths: `RAG_WORKSPACE_ROOT=/app/graphrag`
    - Blob: `AZURE_STORAGE_CONNECTION_STRING`, `AZURE_BLOB_CONTAINER`, `AZURE_BLOB_PREFIX_TEMPLATE=projects/{project_id}/graphrag-input`
    - Messaging: `INGESTION_STREAM_KEY=ingestion.trigger`, `INGESTION_GROUP=neo4j_ingestor`, `REDIS_URL`
    - Neo4j: `NEO4J_URI`, `NEO4J_USERNAME`, `NEO4J_PASSWORD`, `NEO4J_DATABASE=neo4j`, `NEO4J_BATCH_SIZE=1000`, `NEO4J_ENTITY_EMBEDDING_DIM=1536`
    - Retry: `MAX_ATTEMPTS`, backoff base/ceil
  - Acceptance:
    - Missing envs have sane defaults; startup validation explicit
    - A generated `settings.yaml` under `${RAG_WORKSPACE_ROOT}` works with `graphrag index`

- [ ] Task 13 — Minimal GraphRAG pipeline stubs (MVP)
  - Description: End-to-end ingestion happy-path test using the trigger flow and CLI runner.
  - Doc refs: README L14–34 (flow), L124–133 (CLI), L135–143 (orchestration)
  - Example:
    - Publish `{job_id, project_id, attempts: 0}` to `ingestion.trigger`
    - Verify JSONs downloaded, CLI run, parquet imported, and status set to `rag_ready`
  - Acceptance:
    - After import: labels present for `__Document__`, `__Entity__`, `__Community__`; RELATED edges > 0; entity vector index online

- [ ] Task 14 — End-to-end ingestion happy-path test
  - Description: Neo4j client enhancements and vector index creation.
  - Doc refs: README L144–155 (vector index example)
  - Acceptance:
    - Helpers are idempotent (safe to run multiple times)
    - Constraints exist for GraphRAG labels and `RELATED(id)`
    - HNSW vector index exists and is online for `__Entity__(embedding)` with 1536/cosine

- [ ] Task 15 — Fault and retry test suite
  - Description: Tests for transient Neo4j errors, Celery retries/backoff, DLQ behavior, and replay.
  - Doc refs: README L156–160
  - Example: Inject write error; verify attempts increment and DLQ on max.
  - Acceptance:
    - Replay DLQ item leads to successful job completion

- [ ] Task 16 — Identity and idempotency verification tests
  - Description: Tests for deterministic IDs and idempotent MERGE for GraphRAG parquet imports.
  - Doc refs: README L116–124; GraphRAG Neo4j Cypher import
  - Example: Run GraphRAG import twice; counts unchanged; `RELATED(id)` unique
  - Acceptance:
    - Node/edge counts stable across repeated imports
    - Constraints prevent duplicates; `ON MATCH` updates applied where defined

- [ ] Task 17 — Operational limits and backpressure tests
  - Description: Verify concurrency limits, prefetch, in-flight limits, and visibility-timeout claiming.
  - Doc refs: README L156–160
  - Example: Configure max in-flight per consumer; assert throttling.
  - Acceptance:
    - Subscriber enforces in-flight limit; Celery fairness via `worker_prefetch_multiplier=1`

- [ ] Task 18 — Project Management status enums alignment
  - Description: Ensure `rag_failed` is accepted by Project Management API; align enums across services.
  - Doc refs: README L102–103
  - Example: Update enum or validation in `project_router` and shared models as needed.
  - Acceptance:
    - API accepts `rag_failed`; tests cover both `rag_ready` and `rag_failed`

- [ ] Task 19 — Minimal operations: artifacts directory and audit
  - Description: Persist GraphRAG outputs for audit under `${RAG_WORKSPACE_ROOT}/output` and job-scoped `${ARTIFACTS_DIR}/{project_id}/{job_id}` (e.g., copied subset + logs). Optionally export GraphML snapshots.
  - Doc refs: README L143–145
  - Example: Toggle persistence in non-debug envs; `snapshots.graphml=true` when enabled
  - Acceptance:
    - Artifacts retained per retention policy; optional GraphML present when enabled

- [ ] Task 20 — Documentation updates (service + shared)
  - Description: Update docs and compose for the trigger flow and workspace volume.
  - Doc refs: README L162–166; Refactoring plan L63–99
  - Example:
    - `docker-compose.env`: add `GRAPHRAG_API_KEY`, optional Azure OpenAI vars, `RAG_WORKSPACE_ROOT`, `AZURE_BLOB_PREFIX_TEMPLATE`
    - `docker-compose.yml`: mount `graphrag_workspace` volume to `/app/graphrag`; add dedicated Celery worker and (optionally) a subscriber process
  - Acceptance:
    - Docs reflect actual behavior; local quickstart and CI scripts run green end-to-end


