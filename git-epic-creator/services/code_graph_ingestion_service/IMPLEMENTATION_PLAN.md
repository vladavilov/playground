# Code Graph Ingestion Service — Implementation Plan (Incremental, Agent-Ingestible)

This file is the execution plan derived from:
- `services/code_graph_ingestion_service/README.md` (requirements: `CGI-FR-###`)
- `services/code_graph_ingestion_service/IMPLEMENTATION.md` (implementation guidelines and internal contracts)

The service does not currently have an implementation (`src/` is not present). This plan bootstraps the service using existing shared utilities in `services/shared/src/` and implements the full requirement set in deterministic, testable increments.

## Global implementation rules (apply to every task)
- **Determinism first** (README “General implementation tips and rules”):
  - Sort all lists (files, symbols, nodes, edges) before hashing, persisting, or serializing.
  - Do not rely on dict iteration order for persisted results.
- **Idempotent graph writes**: use `MERGE` patterns keyed by stable IDs and scoped by `(project_id, repo_fingerprint)` (**CGI-FR-006**, IMPLEMENTATION.md §7).
- **Project scoping**: every created node/relationship includes `project_id` and every node links to `(__Project__ {id})` via `:IN_PROJECT` (README “Graph model (Neo4j)”).
- **Single-plugin execution**: exactly one plugin is executed per request, selected by `source_language` (**CGI-FR-004A**, **CGI-FR-008**, **CGI-FR-009**, IMPLEMENTATION.md §2.1).
- **Evidence fidelity**: persisted `__CodeNode__.text` preserves **verbatim physical lines** (**CGI-FR-011**, IMPLEMENTATION.md §5.0/§5.1).
- **No job_id modeling**: identify ingestion by `(project_id, repo_fingerprint)` (**CGI-FR-003**).

## Increment sequencing (recommended)
- **Increment A (runnable skeleton + API contract)**: Tasks 00–02
- **Increment B (core determinism + persistence plumbing)**: Tasks 03–10
- **Increment C (COBOL pipeline end-to-end)**: Tasks 11–15 + 17
- **Increment D (Java + XML wiring)**: Task 16
- **Increment E (JavaScript)**: Task 18
- **Increment F (hardening + determinism proofs)**: Tasks 19–22

---

## Tasks (each is a self-contained increment; keep tasks small and test-driven)

### [x] Task 00 — Create service scaffold (packaging, deps, tests folder)
- **Requirements**: Enables implementation of **CGI-FR-001/002/004** (service runtime), and all subsequent requirements.
- **Guidelines**:
  - Follow the packaging pattern used by sibling services (e.g., `services/authentication_service/pyproject.toml`).
  - Target runtime: **Python 3.12** (README “High-level implementation details”).
  - Add `shared @ file:../shared` dependency (reuse existing clients/config/app wiring).
  - Decide on the Git materialization strategy per IMPLEMENTATION.md §1 (choose **one**):
    - **Preferred**: `dulwich` (portable, no OS git required) **or**
    - Git CLI (then Dockerfile must include git; do not mix strategies).
- **Deliverables**:
  - `services/code_graph_ingestion_service/pyproject.toml`
  - `services/code_graph_ingestion_service/Dockerfile`
  - `services/code_graph_ingestion_service/run_local.bat` (optional but consistent with repo)
  - `services/code_graph_ingestion_service/src/__init__.py`
  - `services/code_graph_ingestion_service/tests/` with pytest configuration (`pythonpath=["src"]`)
- **Tests**:
  - Smoke: import `src.main:app` (once Task 01 exists).
- **Acceptance**:
  - `pytest` runs.
  - `uvicorn src.main:app --reload --port 8015` starts (once Task 01 exists).

### [x] Task 01 — FastAPI app wiring + `/health` (shared health endpoint)
- **Requirements**: **CGI-FR-004** (Health).
- **Guidelines**:
  - Reuse shared app and clients:
    - `services/shared/src/utils/app_factory.py`
    - `services/shared/src/utils/neo4j_client.py`
    - `services/shared/src/utils/postgres_client.py`
  - Do NOT replace the shared `/health` route; use `FastAPIFactory` defaults.
- **Deliverables**:
  - `src/main.py` using `FastAPIFactory.create_app(enable_postgres=True, enable_neo4j=True, ...)`
  - `src/config.py` for service-specific settings (at minimum: `COBOL_TS_MAX_ERROR_RATIO`)
- **Tests**:
  - `tests/test_health.py`:
    - verifies `/health` returns 200 and `{status:"ok"}`
- **Acceptance**:
  - `/health` is provided by the shared factory; detailed checks are on `/health/postgres` and `/health/neo4j`.

### [x] Task 02 — Public API contract: `/ingest/zip` and `/ingest/git` + schemas
- **Requirements**: **CGI-FR-001** (ZIP ingest), **CGI-FR-002** (Git ingest), **CGI-FR-003** (no job root), **CGI-FR-004A** (user-selected language).
- **Guidelines**:
  - `source_language` is a required field and must be one of `cobol|java|javascript` (README “source_language values”).
  - Endpoints return `(project_id, repo_fingerprint)` (README “workflow position”).
  - Do not introduce `job_id` into response or storage keys (**CGI-FR-003**).
- **Deliverables**:
  - `src/api/schemas.py` (Pydantic v2 request/response models)
  - `src/api/routers/ingest.py` with:
    - `POST /ingest/zip` (multipart upload)
    - `POST /ingest/git` (JSON body)
  - Wire routers in `src/main.py`.
- **Tests**:
  - Contract tests for 422 on missing `project_id`/`source_language`.
  - 200/202 shape test (stub orchestrator initially).
- **Acceptance**:
  - Both endpoints validate inputs, return contract shape, and route to orchestrator (stub allowed until Task 10).

---

### [x] Task 03 — Deterministic repo materialization: ZIP extract + Git clone/checkout
- **Requirements**: **CGI-FR-001**, **CGI-FR-002**, supports **CGI-FR-005** determinism.
- **Guidelines** (IMPLEMENTATION.md §1 “Repo materialization”):
  - ZIP: use built-in `zipfile` (no external deps). Prevent zip-slip. Normalize path separators and ensure deterministic extraction.
  - Git: use the single chosen strategy (Task 00 decision). Support optional `ref` checkout.
  - Workspace paths must be deterministic and project-scoped.
- **Deliverables**:
  - `src/core/repo_materializer.py` (ZIP + Git)
  - `src/core/workspace.py` (workspace layout helper)
- **Tests**:
  - Zip-slip rejection test.
  - Git checkout test (use local fixture repo or dulwich mocking).
- **Acceptance**:
  - Materializer produces identical bytes on disk for identical inputs.

### [x] Task 04 — Deterministic `repo_fingerprint` algorithm
- **Requirements**: **CGI-FR-005**.
- **Guidelines**:
  - Git: fingerprint derived from commit SHA, plus deterministic dependency manifest hashes **when present** (README §Determinism).
  - ZIP: fingerprint derived from content hash, plus deterministic manifest hashes **when present**.
  - Define exactly which manifests are included and keep list stable across runs (determinism gate).
- **Deliverables**:
  - `src/core/fingerprint.py`
  - `src/core/dependency_manifests.py` (central manifest list + hashing)
- **Tests**:
  - Same repo => same fingerprint; reordering ZIP entries does not change fingerprint.
- **Acceptance**:
  - Fingerprint is stable for identical input and changes when manifest/content changes.

### [x] Task 05 — Ignore rules + deterministic file inventory builder
- **Requirements**: supports **CGI-FR-005/006** (stable inventory for reproducibility and idempotency).
- **Guidelines** (IMPLEMENTATION.md §1 “Ignore rules”):
  - Use `pathspec` for gitignore-compatible filtering.
  - Always ignore `.git/`, build outputs, vendored deps (define a deterministic default ignore set).
  - Inventory must be deterministic: stable sorting by repo-relative POSIX path.
- **Deliverables**:
  - `src/core/ignore_rules.py`
  - `src/core/inventory.py` producing `files[]` entries with `path, sha256, language, line_count`.
- **Tests**:
  - Inventory ordering determinism test.
  - Hash correctness test for a small fixture tree.
- **Acceptance**:
  - `files[]` inventory and computed hashes are stable across runs.

### [x] Task 06 — Internal record contracts + stable IDs/hashes
- **Requirements**: **CGI-FR-007** (deterministic node IDs), **CGI-FR-011** (evidence fidelity via snippet hash and text).
- **Guidelines** (IMPLEMENTATION.md §3.1/§3.2/§4):
  - Implement `CodeNodeRecord` and `EdgeRecord`.
  - Implement stable `node_id` (`stable_node_id`) per IMPLEMENTATION.md §4.1.
  - Implement `snippet_hash` per IMPLEMENTATION.md §4.2: allow only newline normalization + trailing whitespace trimming.
- **Deliverables**:
  - `src/core/records.py`
  - `src/core/stable_ids.py`
- **Tests**:
  - Deterministic `node_id` and `snippet_hash` tests.
- **Acceptance**:
  - IDs/hashes are stable; no randomness introduced.

### [x] Task 07 — Plugin framework + registry (exactly one plugin per request)
- **Requirements**: **CGI-FR-008**, **CGI-FR-009**, **CGI-FR-004A**.
- **Guidelines** (IMPLEMENTATION.md §2.1):
  - Implement `LanguagePlugin` contract and `select_plugin()` with clear error for unsupported `source_language`.
  - Ensure orchestrator executes **exactly one** plugin per request.
- **Deliverables**:
  - `src/plugins/base.py` (`IngestionContext`, `LanguagePlugin` Protocol)
  - `src/plugins/registry.py` (registry + selection)
- **Tests**:
  - Plugin selection tests (supported/unsupported).
  - Assertion that only selected plugin’s `ingest()` called.
- **Acceptance**:
  - Single-plugin execution is enforced and test-covered.

### [x] Task 08 — Neo4j writer (idempotent MERGE + scoping)
- **Requirements**: **CGI-FR-006**, plus Neo4j schema/scoping rules in README “Graph model”.
- **Guidelines** (IMPLEMENTATION.md §7):
  - Use `MERGE` on unique keys:
    - `__Project__` by `{id}`
    - `__Repo__` by `{project_id, repo_fingerprint}`
    - `__File__` by `{project_id, repo_fingerprint, file_path}`
    - `__CodeNode__` by `{project_id, repo_fingerprint, node_id}`
  - Relationship required properties on every edge: `project_id`, `repo_fingerprint`, `confidence`, optional `metadata` (README “Relationship required properties”).
  - Add `:IN_PROJECT` edges for every node to `__Project__`.
  - Handle extra labels (`__UnresolvedCall__`, `__DbTable__`) deterministically; prefer APOC pattern in IMPLEMENTATION.md §7, but provide a fallback if APOC isn’t available.
- **Deliverables**:
  - `src/persistence/neo4j_writer.py`
- **Tests**:
  - Query-shape tests (ensure `MERGE` keys + required props always included).
- **Acceptance**:
  - Re-ingesting same `(project_id, repo_fingerprint)` is idempotent (no duplicates).

### [x] Task 09 — Postgres repo index store (canonical JSON + sha256 + upsert)
- **Requirements**: **CGI-FR-019**, **CGI-FR-020**.
- **Guidelines** (IMPLEMENTATION.md §8):
  - Use `ProjectRepoIndex` ORM model in `services/shared/src/models/project_db.py`.
  - Canonical JSON serialization: sorted keys, no whitespace; store `content_sha256 = sha256(canonical_bytes)` (IMPLEMENTATION.md §8.2).
  - Upsert semantics on `(project_id, repo_fingerprint)` (IMPLEMENTATION.md §8.3).
- **Deliverables**:
  - `src/core/repo_index.py` (builder)
  - `src/persistence/repo_index_store.py` (upsert)
- **Tests**:
  - Canonicalization determinism test.
  - Upsert behavior test (updates `updated_at`, preserves uniqueness).
- **Acceptance**:
  - Deterministic `repo_index_json` bytes and `content_sha256`; upsert works by `(project_id, repo_fingerprint)`.

### [x] Task 10 — Orchestrator: materialize → fingerprint → inventory → plugin → persist
- **Requirements**: **CGI-FR-001/002/003/004A/005/006/008/009/019**.
- **Guidelines** (README “Architecture of service internals” + IMPLEMENTATION.md §2.1):
  - Orchestrate in deterministic order:
    1) materialize repo
    2) compute fingerprint
    3) build inventory (ignore rules)
    4) select plugin by `source_language` (exactly one)
    5) run plugin to produce `CodeNodeRecord[]`, `EdgeRecord[]`, plugin facts
    6) apply oversized chunking pass (Task 17)
    7) write to Neo4j (Task 08)
    8) build+upsert repo_index.json to Postgres (Task 09)
  - Logging must include `(project_id, repo_fingerprint)` (README “Observability”).
- **Deliverables**:
  - `src/core/orchestrator.py`
  - Wire orchestrator into API endpoints (`/ingest/zip`, `/ingest/git`).
- **Tests**:
  - Orchestrator ordering test with mocks.
  - API integration test (using stub plugin) that returns `repo_fingerprint`.
- **Acceptance**:
  - End-to-end path works with a stub plugin: graph writes + repo_index upsert invoked.

---

## COBOL pipeline (must be precise)

### [x] Task 11 — COBOL preprocessing: FIXED/FREE + evidence stream + parse stream mapping
- **Requirements**: **CGI-FR-010**, **CGI-FR-011**, and “Preprocessing responsibility (mandatory)” in **CGI-FR-014**.
- **Guidelines** (IMPLEMENTATION.md §5.0 and §5.1):
  - Produce two streams per file:
    - **evidence stream**: original physical lines verbatim (used for persisted `__CodeNode__.text`)
    - **parse stream**: normalized bytes for Tree-sitter parsing
  - Default mode FIXED; switch on `>>SOURCE FORMAT FREE|FIXED`.
  - Merge continuations for parse stream while maintaining logical→physical span mapping.
- **Deliverables**:
  - `src/plugins/cobol/normalizer.py`
  - `src/plugins/cobol/parse_quality.py` (shared helpers for error ratio calculation)
- **Tests**:
  - Use fixtures from `services/tree_sitter_cobol_binding/tests/fixtures/ibm_enterprise/`.
  - Tests proving evidence text equals original lines verbatim.
- **Acceptance**:
  - COBOL evidence fidelity is preserved; parse stream retains line mapping.

### [x] Task 12 — COBOL copybook expansion: `COPY ... REPLACING` + provenance + `INCLUDES` edges
- **Requirements**: **CGI-FR-013**.
- **Guidelines** (IMPLEMENTATION.md §5.2):
  - Resolve copybooks via ordered search paths (configurable).
  - Apply `REPLACING` at token level before insertion.
  - Emit `INCLUDES` edges to copybook files and maintain evidence provenance via an expansion map.
- **Deliverables**:
  - `src/plugins/cobol/copybooks.py`
  - Update COBOL plugin context/config with copybook search paths.
- **Tests**:
  - Deterministic resolution order test.
  - `REPLACING` stability test.
- **Acceptance**:
  - Identical inputs produce identical expanded stream and identical `INCLUDES` edges.

### [x] Task 13 — COBOL unitization: AST-first with deterministic fallback scanner
- **Requirements**: **CGI-FR-012**.
- **Guidelines** (IMPLEMENTATION.md §5.3):
  - **Precondition (pipeline responsibility; per `tree_sitter_cobol_binding/README.md`)**:
    - Tree-sitter COBOL is **not** a full COBOL preprocessor. The ingestion pipeline **must normalize and expand** source before parsing.
    - Required normalization before invoking Tree-sitter (must be deterministic and preserve an evidence map):
      - **Newlines**: normalize `\r\n`/`\r` → `\n`.
      - **Encoding/codepage**: decode (e.g., EBCDIC→Unicode/UTF-8) before any other transforms; record evidence metadata.
      - **Source format**: handle FIXED vs FREE, including directives that can switch format mid-stream (IBM/MF/GnuCOBOL realities).
      - **Card-image columns (FIXED)**:
        - cols **1–6** sequence area, col **7** indicator, cols **8–72** text, cols **73+** identification area.
        - apply deterministic handling driven by col 7 (comment/debug/continuation semantics).
      - **Inline comments**: support enterprise `*>` comment-to-EOL.
      - **COPY / REPLACE expansion**: must run prior to parse (Task 12), including deterministic `REPLACING` and a provenance/expansion map.
      - **`EXEC ... END-EXEC` blocks**: detect and preserve as evidence artifacts; present to COBOL parser as **opaque** content (unitize as `exec_block`, do not attempt to parse embedded SQL/CICS/IMS here).
    - Output of preprocessing should be a **normalized byte stream** + a **span/line mapping** back to original evidence (see Task 11/12).
  - **Tree-sitter integration (use the precompiled wheel; “golden source”)**:
    - Use `tree-sitter-cobol` (module `tree_sitter_cobol`) from `services/tree_sitter_cobol_binding`.
    - Dialect selection uses the wheel API:
      - `capsule = tree_sitter_cobol.languages()[dialect_key]`
      - `lang = tree_sitter.Language(capsule)`
      - NOTE: Today dialect keys (`cobol|ibm|micro_focus|gnucobol`) are baseline aliases; dialect variance is handled first by preprocessing + fixtures.
    - Implement a deterministic **capability check** (import + `Language(...)` + parse minimal snippet) once per process start; if it fails, force fallback for all COBOL.
  - **Prefer Tree-sitter AST when per-file quality gate passes**:
    - Gate logic should match the binding’s tests (`tests/test_parse_quality.py`):
      - walk tree and count `total_nodes`, `ERROR` nodes, and `is_missing` nodes
      - compute `error_ratio = (error_nodes + missing_nodes) / max(total_nodes, 1)`
    - Accept AST for the file when:
      - grammar loads successfully, parse returns a root node, and
      - `error_ratio <= COBOL_TS_MAX_ERROR_RATIO` (env/config; **default `0.02`**), and
      - (optional) `root.has_error == False` can be used as a *stricter* mode for “core” fixtures/tests
    - Persist/emit metrics (`has_error`, `error_ratio`, counts) as ingest telemetry/facts to keep dialect support measurable.
  - **Else fall back to deterministic state-machine unitization**:
    - Fallback operates on the **same normalized stream** (not raw file bytes) so spans/IDs are stable.
    - Must be conservative: find only high-signal structural units (PROGRAM-ID, DIVISION/SECTION/PARAGRAPH boundaries, sentences via period, `EXEC...END-EXEC` via preprocessor markers, COPY-provenance boundaries when available).
  - **AST-first unitization output**:
    - Output `__CodeNode__` kinds:
      - `program, division, section, paragraph, sentence, statement, exec_block, copybook, data_item`
    - Use Tree-sitter node byte spans and translate them back to original evidence via the preprocessing span map (Task 11/12).
    - Ensure stable spans and stable node IDs (Task 06): same input → same normalized stream → same units → same IDs.
- **Deliverables**:
  - `src/plugins/cobol/unitizer.py`
  - `src/plugins/cobol/ts_gate.py` (per-file gate)
- **Tests**:
  - Unitization of minimal program fixture (AST path) with stable units/spans across runs.
  - Parse-quality gate tests:
    - `error_ratio` calculation matches the binding test definition (`ERROR` + `is_missing`).
    - threshold behavior: `COBOL_TS_MAX_ERROR_RATIO` default `0.02`, override via env/config.
  - Forced fallback tests:
    - simulate “grammar unavailable” (import failure / Language capsule failure)
    - simulate parse errors (feed intentionally mangled normalized stream or set max ratio to `0.0`)
  - Unitization preserves evidence mapping:
    - AST-produced byte spans map back to correct original file/line ranges using the preprocessing map.
- **Acceptance**:
  - Same file yields same stable units and spans across runs.

### [x] Task 14 — COBOL edge extraction (conservative; confidence-coded; unresolved nodes explicit)
- **Requirements**: **CGI-FR-014**.
- **Guidelines** (IMPLEMENTATION.md §5.4):
  - `CALL` literal: `confidence=1.0` resolve by `PROGRAM-ID`
  - Dynamic `CALL`: edge to `__CodeNode__` kind=`unresolved` with extra label `__UnresolvedCall__`, `confidence=0.2`
  - `PERFORM`: resolve paragraph/section targets deterministically, `confidence=1.0`
  - `EXEC CICS`: `LINK`/`XCTL` literal programs create `CALLS confidence=0.8` with metadata `{protocol:"CICS"}`
  - `EXEC SQL`: token-scan tables create `__DbTable__` nodes with `confidence=0.6`
  - `READS/WRITES`: conservative best-effort; prefer emitting low-confidence rather than guessing.
- **Deliverables**:
  - `src/plugins/cobol/edges.py`
  - `src/plugins/cobol/symbols.py` (program-id and label maps for resolution)
- **Tests**:
  - Literal vs dynamic call tests.
  - Exec CICS/SQL fixture tests.
- **Acceptance**:
  - No speculative high-confidence edges; unresolved and DB table nodes are modeled explicitly.

### [x] Task 15 — COBOL plugin integration (file selection → preprocess → copybooks → unitize → edges → facts)
- **Requirements**: **CGI-FR-008/009** plugin architecture + **CGI-FR-010..014**.
- **Guidelines** (IMPLEMENTATION.md §2.1 plugin contract + §5):
  - Implement COBOL `LanguagePlugin` as the **single orchestrator** for the COBOL pipeline:
    - deterministic file selection (`.cbl`, `.cob`, `.cpy` where appropriate)
    - end-to-end deterministic stages (order is important; each stage uses outputs/mappings from prior stages):
      - **(a) preprocess** (Task 11): encoding decode → newline normalization → FIXED/FREE/columns/directives normalization → `EXEC ... END-EXEC` preservation markers → evidence/span map
      - **(b) copybook expansion** (Task 12): `COPY ... REPLACING` expansion into the normalized parse stream + expansion provenance + `INCLUDES` edges
      - **(c) unitize** (Task 13): AST-first via `tree_sitter_cobol` with per-file quality gate; fallback scanner on the same normalized stream
      - **(d) edges/symbols** (Task 14): conservative edge extraction + explicit unresolved nodes
      - **(e) facts/telemetry**: persist ingest metadata needed for auditability and “dialect support is measurable”
    - dialect handling:
      - accept a `dialect_key` (default `cobol`); pass it to Tree-sitter capsule selection (`tree_sitter_cobol.languages()[dialect_key]`)
      - NOTE: wheel dialects are currently aliases; **dialect behavior is primarily preprocessing + fixtures** (per `tree_sitter_cobol_binding/README.md`)
    - output must be stable:
      - stable units/spans from the evidence map
      - stable node IDs (Task 06)
      - stable edge resolution ordering (stable sort keys; no hash-order iteration)
    - repo-index facts (IMPLEMENTATION.md §8) must include at least:
      - **copybook stats**: resolved count, missing count, and deterministic search-path order used
      - **unresolved call rate**
      - **Tree-sitter availability + quality**: `package_version`, `__grammar_version__`, per-file `error_ratio` stats, fallback rate, dialect_key used
  - Packaging / dependency wiring (make “AST-first” actually runnable in-service):
    - Add dependency in `code_graph_ingestion_service/pyproject.toml`:
      - `tree-sitter-cobol==<pinned_version>` (import name `tree_sitter_cobol`)
    - Local dev workflow uses a **locally built wheel** (no runtime compilation in the service):
      - build: `services/tree_sitter_cobol_binding/scripts/build_wheels.*` → outputs `services/tree_sitter_cobol_binding/wheelhouse/*.whl`
      - install into venv: `pip install --find-links <wheelhouse> tree-sitter-cobol==<pinned_version>`
    - Docker workflow mirrors the shared-lib approach:
      - copy `tree_sitter_cobol_binding/` sources into the image
      - build the wheel inside the image (`python -m build --wheel --outdir /app/wheelhouse`)
      - install from that wheelhouse (`pip install --find-links /app/wheelhouse tree-sitter-cobol==<pinned_version>`) before installing the service
- **Deliverables**:
  - `src/plugins/cobol/plugin.py`
  - Register plugin in `src/plugins/registry.py`
  - Update `pyproject.toml` dependency for `tree-sitter-cobol`
  - Update `Dockerfile` to build+install `tree-sitter-cobol` from `tree_sitter_cobol_binding/` sources (shared-lib style)
- **Tests**:
  - Plugin produces deterministic nodes/edges for known fixtures.
  - Package wiring / smoke:
    - `/health` returns COBOL parser `status != "unavailable"` when wheel is present
    - container build succeeds when compiling/building the COBOL wheel from source inside the image (Linux build env)
- **Acceptance**:
  - End-to-end `/ingest/*` with `source_language=cobol` writes graph + upserts repo_index.

---

## Java and JavaScript pipelines

### [x] Task 16 — Java plugin: Java nodes/edges + XML wiring in same run
- **Requirements**: **CGI-FR-015**, **CGI-FR-016**, **CGI-FR-009B**.
- **Guidelines** (IMPLEMENTATION.md §1 Parsing + README “Java config wiring”):
  - Use `tree-sitter-language-pack` for Java and XML parsing (IMPLEMENTATION.md §1).
  - When `source_language=java`, include XML parsing as part of the Java plugin run (no separate XML plugin selection from UI).
  - Emit `CONFIG_WIRES` edges from XML wiring (e.g., Spring bean IDs) to Java code nodes (conservative).
- **Deliverables**:
  - `src/plugins/java/plugin.py`
  - `src/plugins/java/unitizer.py`, `src/plugins/java/edges.py`
  - `src/plugins/xml/parser.py` (used internally by Java plugin), `src/plugins/xml/edges.py`
- **Tests**:
  - Java `IMPORTS` and basic unitization tests.
  - XML wiring produces `CONFIG_WIRES` edges deterministically.
- **Acceptance**:
  - Java ingestion emits Java nodes + XML-derived wiring edges.

### [x] Task 17 — Oversized node chunking (>1000 physical lines) + synthetic pipe edges
- **Requirements**: **CGI-FR-018**.
- **Guidelines** (IMPLEMENTATION.md §6):
  - If a `__CodeNode__` spans > 1000 physical lines, chunk deterministically and connect chunks with synthetic pipe edges:
    - relationship type: `CALLS`
    - `confidence=1.0`
    - `metadata={synthetic:true, pipe:true, order:i}`
  - Prefer natural boundaries first (AST boundaries: methods/paragraphs/functions). Fall back to deterministic window partitioning.
  - Ensure chunk node IDs are stable (derive from parent identity + chunk order).
- **Deliverables**:
  - `src/core/chunking.py`
  - Integrate chunking pass into orchestrator (Task 10).
- **Tests**:
  - Deterministic chunk boundary test.
  - Metadata correctness test for pipe edges.
- **Acceptance**:
  - Oversized nodes are chunked deterministically, and chunk continuity is encoded via pipe edges.

### [x] Task 18 — JavaScript plugin: deterministic module resolution + `IMPORTS` edges
- **Requirements**: **CGI-FR-017**.
- **Guidelines**:
  - Resolve ES imports and CommonJS `require()` deterministically.
  - Emit `IMPORTS` edges (conservative; do not guess if resolution fails).
  - Ensure file selection and resolution ordering are stable.
- **Deliverables**:
  - `src/plugins/javascript/plugin.py`
  - `src/plugins/javascript/resolver.py`, `unitizer.py`, `edges.py`
- **Tests**:
  - ES import resolution tests.
  - CommonJS require resolution tests.
- **Acceptance**:
  - JS ingestion produces stable `IMPORTS` edges for the same repo.

---

## Hardening, determinism proofs, and system integration

### [x] Task 19 — Enforce Neo4j graph contract + scoping invariants
- **Requirements**: README “Graph model (Neo4j)” + **CGI-FR-006**.
- **Guidelines**:
  - Validate at write time (or pre-write) that every node/edge has `project_id` and `repo_fingerprint`.
  - Ensure every node has an `:IN_PROJECT` edge to `(__Project__ {id})`.
  - Ensure relationship properties always include required fields.
- **Deliverables**:
  - Validation helpers in `src/persistence/neo4j_writer.py` or `src/core/orchestrator.py`.
- **Tests**:
  - Negative tests: missing scoping fields causes failure.
- **Acceptance**:
  - It is impossible to write out-of-scope graph entities.

### [x] Task 20 — Full end-to-end determinism tests (ZIP + Git)
- **Requirements**: **CGI-FR-005**, **CGI-FR-006**, **CGI-FR-007**.
- **Guidelines**:
  - Run orchestrator twice for the same input and compare:
    - `repo_fingerprint`
    - canonical `repo_index_json` bytes + `content_sha256`
    - produced `(node_id, snippet_hash)` sets (sorted)
  - Keep test fixtures small and deterministic.
- **Deliverables**:
  - `tests/test_e2e_determinism_zip.py`
  - `tests/test_e2e_determinism_git.py`
- **Acceptance**:
  - Two runs produce identical deterministic outputs.

### [x] Task 21 — Observability: structured logs + OpenTelemetry spans around phases
- **Requirements**: README “Observability”.
- **Guidelines**:
  - Structured logs must include `(project_id, repo_fingerprint)` consistently.
  - Add spans for: materialize, fingerprint, inventory, plugin run, chunking, neo4j write, postgres upsert.
  - Reuse shared logging configuration patterns where possible.
- **Deliverables**:
  - `src/observability/logging.py` and/or minimal wiring in `src/main.py`
  - `src/observability/tracing.py` (if needed)
- **Acceptance**:
  - Logs and traces are sufficient to debug determinism and ingestion failures.

### [x] Task 22 — README alignment pass (keep docs and code contract consistent)
- **Requirements**: Documentation consistency (project engineering practice).
- **Guidelines**:
  - Update `services/code_graph_ingestion_service/README.md` only where the implemented API payloads differ in detail (e.g., form field names for ZIP ingestion).
  - Do not change requirement IDs; only clarify request/response examples.
- **Deliverables**:
  - Updated `README.md` (only if necessary).
- **Acceptance**:
  - Implemented API matches documented contract and `source_language` selection semantics.

