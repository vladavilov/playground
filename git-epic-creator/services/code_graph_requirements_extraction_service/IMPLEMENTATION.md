# Code Graph Requirements Extraction Service — Implementation Guide

This document defines **how to implement** the graph-driven, message-triggered extractor described in `README.md`.

Key changes vs older drafts:
- **No REST trigger** (runs as a Celery task)
- **Language-agnostic UoA selection** based on graph parameters, with **Leiden-assisted** partitioning
- **No slicing/truncation inside a unit**; budget compliance is achieved by graph refinement
- Leaf extraction omits IO/errors and assumptions except where explicitly allowed by `README.md`

---

## 1) Core libraries to reuse

### Celery (job execution)
- Use the shared Celery factory `services/shared/src/utils/celery_factory.py`.
- Follow the shared constants approach in `services/shared/src/constants/pipelines.py` (add new constants for this service).
- Requirement mapping: **CGE-FR-002**

### Neo4j graph reads (evidence + topology)
- **Neo4j Python driver** (`neo4j`).
- Use `RoutingControl.READ` for read-only queries.
- Treat Neo4j as the graph system-of-record; do not accept portable exports.
- Requirement mapping: **CGE-FR-007**, **CGE-FR-010**, **CGE-FR-011**

### Repo index reads (Postgres)
- **SQLAlchemy + psycopg** using shared ORM models in `services/shared/src/models/project_db.py`.
- Read `ProjectRepoIndex.repo_index_json` keyed by `(project_id, repo_fingerprint)`.
- Requirement mapping: **CGE-FR-009**

### Project status updates (Postgres)
- Update `projects.status` and optionally `projects.processed_pct` using `Project` from `services/shared/src/models/project_db.py`.
- Status values MUST match `ProjectStatus` in `services/shared/src/models/project_rest.py`.
- Requirement mapping: **CGE-FR-006**

### Redis pub/sub (progress + completion)
- Publish to channel `ui:ai_requirements_progress` (see `services/shared/src/constants/streams.py`).
- Prefer reusing `WorkflowProgressMessage` in `services/shared/src/models/progress_messages.py` + `RedisProgressPublisher` in `services/shared/src/utils/redis_progress_publisher.py`.
- Requirement mapping: **CGE-FR-005**, **CGE-FR-005A**

### Embeddings and clustering
- Deterministic embedding + deterministic clustering (union-find / deterministic greedy).
- Similarity threshold MUST be configurable.
- Requirement mapping: **CGE-FR-016**

### Blob outputs
- **Azure Blob SDK** (`azure-storage-blob`), upload with overwrite.
- Requirement mapping: **CGE-FR-020**

---

## 2) Proposed high-level file structure (Celery-first)

```
code_graph_requirements_extraction_service/
  src/
    main.py                         # optional FastAPI health only (CGE-FR-003)
    celery_worker_app.py            # Celery app bootstrap for this service
    tasks/
      requirements_tasks.py         # extract_requirements_task (CGE-FR-002)
    core/
      orchestrator.py               # end-to-end extraction run (CGE-FR-002..021)
      repo_index.py                 # Postgres read + endpoint classification (CGE-FR-009, CGE-FR-014A)
      traversal_planner.py          # Tarjan SCC + levels (CGE-FR-011/011A)
      uoa/
        partition.py                # Leiden-assisted partition + refinement (CGE-FR-012/012A)
        serialize.py                # deterministic UoA serialization (CGE-FR-013..013B)
      leaf_extractor.py             # LLM calls + schema validation (CGE-FR-014..014B)
      reducer.py                    # hierarchical reduction + confidence aggregation (CGE-FR-017/017A/018)
      dedup/
        normalize.py                # Pass A (CGE-FR-015)
        embeddings.py               # embed + cache adapter (CGE-FR-016)
        cluster.py                  # deterministic clustering (CGE-FR-016)
        llm_merge.py                # cluster merge prompt (CGE-FR-016)
      validation/
        coverage.py                 # Gate 1 (CGE-FR-019)
        entailment.py               # Gate 2 (CGE-FR-019)
        contradictions.py           # Gate 3 (CGE-FR-019)
      publish/
        progress.py                 # Redis pub/sub progress + completion (CGE-FR-005/005A)
      render/
        markdown_renderer.py        # requirements.md generation (CGE-FR-020)
        metadata.py                 # run-metadata.json (CGE-FR-020)
    persistence/
      neo4j_reads.py                # query layer for nodes/edges/evidence
      postgres.py                   # Postgres sessions for Project + ProjectRepoIndex
      blob_storage.py               # artifact uploads
```

Notes:
- If you keep FastAPI at all, it is for **health only** (no `/extract` endpoint).
- The only “start extraction” mechanism is the Celery task entrypoint.

---

## 3) Orchestration (Celery job)

Requirement mapping: **CGE-FR-002**, **CGE-FR-004**, **CGE-FR-005/005A**, **CGE-FR-006**

### 3.1 Task signature

Recommended Celery task inputs:
- `project_id: str` (UUID)
- `repo_fingerprint: str`
- Optional config overrides (or use env/config module)

### 3.2 Job lifecycle (must be observable and idempotent)

Algorithm sketch:
1. Set project status to `rag_processing` in Postgres.
2. Publish Redis progress “started” on `ui:ai_requirements_progress`.
3. Load `repo_index.json` from Postgres (`project_repo_indexes`).
4. Read the relevant graph slice from Neo4j (project-scoped, fingerprint-scoped).
5. Build SCC DAG + levels from entrypoints derived from repo_index.
6. Build budget-safe UoAs (Leiden + refinement) and process them deterministically.
7. Dedup + reduce + validate.
8. Upload artifacts to blob.
9. Update project status to `rag_ready` or `rag_failed`.
10. Publish Redis completion message with artifact paths + summary metrics.

---

## 4) Deterministic traversal planner (SCC + levels)

Requirement mapping: **CGE-FR-010**, **CGE-FR-011**, **CGE-FR-011A**

### 4.1 Build the traversal graph `G`
- Vertices: `__CodeNode__.node_id` values in-scope (project_id + repo_fingerprint).
- Edges: `CALLS`, `PERFORMS`, `IMPORTS`, plus ingestion synthetic chunk pipe edges (identified by `r.metadata.pipe == true`).

Neo4j query sketch to fetch adjacency (IDs only):

```cypher
MATCH (n:__CodeNode__ {project_id:$project_id, repo_fingerprint:$repo_fingerprint})
WHERE n.node_id IN $node_ids
OPTIONAL MATCH (n)-[r:CALLS|PERFORMS|IMPORTS]->(m:__CodeNode__ {project_id:$project_id, repo_fingerprint:$repo_fingerprint})
RETURN n.node_id AS src, collect(m.node_id) AS dsts
```

### 4.2 Tarjan SCC (deterministic)
- Implement Tarjan in-repo.
- Sort adjacency lists before running Tarjan.

(The Tarjan skeleton from older drafts remains valid; keep it as-is.)

### 4.3 Entry points for level(0) (repo-index driven)

Do NOT rely on a graph property like `is_entrypoint` unless ingestion explicitly emits it.

Recommended approach:
- Derive entrypoints and main public endpoints from `repo_index.json` (ingestion contract).
- Resolve them to `node_id` using deterministic keys (e.g., `(file_path, kind, symbol, start_line, end_line)` if present in the index) and/or ingestion’s canonical symbol map.

Level schedule:
- `level(0)`: SCCs that contain at least one resolved entrypoint node.
- `level(k+1)`: SCCs whose incoming edges originate only from SCCs with level ≤ k.

Deterministic ordering inside each level:
- sort SCCs by `(-scc_size, lexical_min_key)` where `lexical_min_key` is the min of `(file_path, symbol, node_id)` inside the SCC.

---

## 5) UoA partitioning (language-agnostic; Leiden-assisted; budget-safe)

Requirement mapping: **CGE-FR-012**, **CGE-FR-012A**

This section replaces “COBOL-first” selection. UoAs are built using graph parameters only.

### 5.1 Deterministic weighted graph for Leiden

Construct an undirected weighted graph \(H\) from the directed traversal graph \(G\):
- For each directed edge \(u \to v\):
  - \(w(u \to v) = confidence(u \to v) \times edge\_type\_weight(type)\)
- Symmetrize:
  - \(w(u,v) = \max(w(u \to v), w(v \to u))\)

Seed:
- Use a stable seed derived from `(project_id, repo_fingerprint)` for repeatability.

### 5.2 Partition + refinement strategy

1. Compute SCC levels from entrypoints.
2. For each configurable “level window”, run Leiden to get initial communities.
3. Expand each community into a candidate UoA by adding mandatory neighbors:
   - `INCLUDES` copybook/file neighbors when relevant
   - `CONFIG_WIRES` wiring neighbors when relevant
   - ingestion chunk pipes (metadata `{pipe:true}`) to preserve chunk continuity
4. Enforce `max_uoa_tokens` (or max_chars):
   - If too large, refine in graph space (increase Leiden resolution / re-run within the community).
   - If still too large, split along SCC depth boundaries while preserving highest-confidence edges.

Deterministic ordering of final UoAs:
- primary: increasing topological level
- secondary: decreasing total incident weight (relationship count/strength)
- tie-break: `(file_path, symbol, node_id)`

---

## 6) UoA serialization (single chunk; no slicing; no truncation)

Requirement mapping: **CGE-FR-013**, **CGE-FR-013A**, **CGE-FR-013B**

Key rule: **Do not slice `__CodeNode__.text`**. Use full text for every included node; enforce budget via partitioning (Section 5).

Recommended serialization:
1. Sort nodes by `(file_path, start_line, end_line, node_id)`.
2. Emit a minimal header for each node containing only:
   - `file_path`, `kind`, `start_line..end_line`, optional `language`
   - Do NOT include `snippet_hash`, `node_id`, or `symbol` in the header (these remain usable internally and for evidence pointers).
3. Emit the verbatim `__CodeNode__.text` for each node.
4. Emit an **EDGE LIST** section describing relationships between included nodes so the LLM has explicit relational context:
   - For every relationship `(src)-[r]->(dst)` where both endpoints are in the UoA, emit a line with:
     - `src_index`, `rel_type`, `dst_index`, `confidence`, optional `metadata`
   - Deterministic edge ordering:
     - primary: `src_index` ascending
     - secondary: `rel_type` ascending
     - tertiary: `dst_index` ascending
     - tie-breakers: `confidence` descending, then stable JSON serialization of metadata
5. Keep the final unit as a single LLM input chunk.

Neo4j edge fetch sketch (within-UoA edges only):

```cypher
MATCH (a:__CodeNode__ {project_id:$project_id, repo_fingerprint:$repo_fingerprint})
WHERE a.node_id IN $uoa_node_ids
MATCH (a)-[r]->(b:__CodeNode__ {project_id:$project_id, repo_fingerprint:$repo_fingerprint})
WHERE b.node_id IN $uoa_node_ids
RETURN a.node_id AS src, type(r) AS rel_type, b.node_id AS dst,
       r.confidence AS confidence, r.metadata AS metadata
```

---

## 7) Leaf extraction contract (UoA → leaf requirements)

Requirement mapping: **CGE-FR-014**, **CGE-FR-014A**, **CGE-FR-014B**

Hard rules:
- Do NOT extract `inputs/outputs/errors` unless the UoA corresponds to an **entrypoint** or **main public endpoint** (repo-index driven).
- Do NOT emit assumptions at leaf-level.

Minimal leaf output shape (example):

```json
{
  "requirements": [
    {
      "statement": "The system shall ...",
      "rationale": "Evidence-grounded rationale.",
      "evidence": [
        {"file_path": "src/foo.cbl", "start_line": 10, "end_line": 42, "snippet_hash": "abc123", "node_id": "n1"}
      ],
      "confidence": 0.82
    }
  ]
}
```

For entrypoints/public endpoints only, extend with an optional structured section:
- `io_contract` with `inputs`, `outputs`, `errors`.

Schema enforcement strategy:
- Validate with Pydantic v2 immediately after LLM return.
- Reject/repair outputs that omit evidence or use out-of-range confidences.

---

## 8) Dedup engine (threshold-configurable)

Requirement mapping: **CGE-FR-015**, **CGE-FR-016**

### 8.1 Pass A — deterministic normalization
- Normalize modal verbs → “shall”
- Normalize whitespace/punctuation and unicode (NFKC recommended)
- Deterministically strip “noise” that breaks exact matching but not meaning (e.g., trailing punctuation, repeated spaces)
- Canonicalize entity names using `repo_index.json` symbol map when available
- Compute `statement_hash = sha256(canonical_statement_bytes)`
- Exact duplicate merge by `statement_hash`:
  - choose a deterministic canonical representative (lexicographically smallest original statement; tie-break by stable `(file_path,start_line,end_line)` of first evidence)
  - union evidence pointers and provenance (`merged_from`)
  - compute merged confidence deterministically (pick **max** or **evidence-weighted mean**; keep the rule stable and record it in `validation.json`)

### 8.2 Pass B — semantic clustering + LLM merge

Similarity threshold MUST be configurable (default 0.88).

Update the function signature to make configurability explicit:

```python
def cluster_by_threshold(
    text_hashes: list[str],
    embeddings: list[list[float]],
    *,
    threshold: float,
) -> list[list[int]]:
    ...
```

(The union-find skeleton from older drafts remains valid; keep it, but do not hardcode 0.88.)

Additional deterministic details (recommended):
- **Blocking**: avoid O(n²) by grouping candidates deterministically (e.g., hash prefix buckets of the normalized statement).
- **Cluster ordering**: order clusters by smallest member index (or by lexical representative) to ensure stable downstream merges.
- **Cluster merge**:
  - pick a deterministic “anchor” statement for the prompt (lexicographically smallest canonical statement)
  - LLM merge MUST output a single merged statement + rationale; evidence = union; provenance must list all merged hashes/IDs.

---

## 9) Reduction (hierarchical synthesis) + confidence aggregation

Requirement mapping: **CGE-FR-017**, **CGE-FR-017A**, **CGE-FR-018**

### 9.1 Hierarchical reduction
Reduce requirements across levels:
- UoA → file/module → subsystem → system

### 9.2 Confidence aggregation (deterministic; auditable)

Recommended conservative aggregation is an evidence-weighted mean:

\[
conf = \frac{\sum_i w_i \cdot conf_i}{\sum_i w_i}
\]

Where \(w_i\) is a deterministic weight such as:
- number of distinct evidence nodes (preferred)
- or cumulative edge-confidence weight supporting the reduced statement

The reducer MUST record provenance (`reduced_from` IDs + weights) so the aggregated confidence is auditable in `validation.json`.

### 9.3 Assumptions only at final synthesis
- Leaf-level: no assumptions.
- Final system-level synthesis MAY introduce assumptions, explicitly flagged, with bounded confidence.

---

## 10) Validation gates

Requirement mapping: **CGE-FR-019**

- **Gate 1**: coverage ≥ 0.90 (unless uncovered are unreachable)
- **Gate 2**: entailment check per requirement
- **Gate 3**: contradiction scan within each dedup cluster

---

## 11) Artifact uploads

Requirement mapping: **CGE-FR-020**, **CGE-FR-021**

Upload paths:
- `requirements-artifacts/{project_id}/{repo_fingerprint}/requirements.md`
- `requirements-artifacts/{project_id}/{repo_fingerprint}/validation.json`
- `requirements-artifacts/{project_id}/{repo_fingerprint}/run-metadata.json`

Minimal `run-metadata.json` fields:
- `project_id`, `repo_fingerprint`, `started_at`, `finished_at`
- `uoa_count_total`, `uoa_count_covered`, `coverage`
- `levels_processed`, `scc_count`
- `dedup_stats` (before/after counts, cluster sizes, threshold used)
- `validation_summary` (entailed/not_supported/contradicted)

