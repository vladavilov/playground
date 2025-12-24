# Code Graph Ingestion Service (Rust / Axum, plugin-based; UI-selected source language)

This service ingests a **project-scoped repository snapshot** (ZIP upload or Git URL), performs **deterministic static analysis** via **language plugins**, and (optionally) persists a code RepoGraph via **`neo4j-repository-service`** plus a deterministic `repo_index.json` manifest in Postgres.

This `README.md` is the **single golden source** for requirements, architecture, and implementation details.

---

## Requirements (CGI-FR-###)

### Ingestion interfaces
- **CGI-FR-001 (ZIP ingest)**: Expose `POST /ingest/zip` to ingest an uploaded ZIP into a project-scoped workspace.
- **CGI-FR-002 (Git ingest)**: Expose `POST /ingest/git` to clone a Git repository (optionally checkout a ref) into a workspace.
- **CGI-FR-003 (No job graph root)**: Identify an ingestion by `(project_id, repo_fingerprint)` (no `job_id` modeling).
- **CGI-FR-004 (Health)**: Expose `GET /health` returning `{ "status": "ok" }`.
  - Also expose `GET /health/postgres` and `GET /health/neo4j`.
- **CGI-FR-004A (User-selected source language)**: Execute **exactly one** language pipeline selected by `source_language`.

#### `source_language` values (API contract)
`source_language` MUST be one of:
- `cobol`
- `java`
- `javascript`

### Determinism and idempotency
- **CGI-FR-005 (Deterministic fingerprint)**:
  - Git: derived from a commit SHA (+ dependency manifest hashes when present).
  - ZIP: derived from a content hash (+ dependency manifest hashes when present).
- **CGI-FR-006 (Idempotent writes)**: Graph persistence must be idempotent for identical `(project_id, repo_fingerprint)`.
- **CGI-FR-007 (Deterministic node IDs)**: `__CodeNode__.node_id` is stable for identical inputs.

### Language plugins
- **CGI-FR-008 (Plugin-based languages)**: One plugin per supported language; shared materialization/fingerprint/inventory and shared persistence.
- **CGI-FR-009 (Single-plugin execution)**: Exactly one plugin runs per request.
- **CGI-FR-009B (Java config wiring)**: When `source_language=java`, also ingest relevant XML and emit `CONFIG_WIRES`.

### COBOL
- **CGI-FR-010 (Fixed/Free handling)**: Default FIXED; switch via `>>SOURCE FORMAT FREE|FIXED`.
- **CGI-FR-011 (Physical lines preserved)**: `__CodeNode__.text` preserves **verbatim physical lines**.
- **CGI-FR-012 (COBOL unitization)**: Create stable COBOL units (see “Current implementation status” for what’s implemented today).
- **CGI-FR-013 (Copybook expansion)**: Expand `COPY ... REPLACING` deterministically and emit `INCLUDES`.
- **CGI-FR-014 (COBOL edges)**: Emit conservative `CALLS`/`PERFORMS`/`READS`/`WRITES` and model unresolved targets explicitly.

### Java / JavaScript
- **CGI-FR-015 (Java nodes/edges)**: Emit class/method nodes and conservative `IMPORTS`/`CALLS`.
- **CGI-FR-016 (XML wiring)**: Emit `CONFIG_WIRES` edges from XML to Java code nodes.
- **CGI-FR-017 (JS module resolution)**: Emit `IMPORTS` edges and deterministically resolve to in-repo modules when possible.

### Oversized node handling
- **CGI-FR-018 (Oversized chunking)**: Any `__CodeNode__` spanning >1000 physical lines is chunked deterministically into `kind="chunk"` nodes and connected via `NEXT_CHUNK` edges with `confidence=1.0` and metadata `{synthetic:true, order:i}`.

### Structural hierarchy
- **CGI-FR-021 (CONTAINS ordering)**: `CONTAINS` edges are generated centrally via span nesting and MUST include deterministic order metadata for direct children: `metadata: { strategy: "span-nesting", order: <1-based> }` where `order` is the source order of the child among its siblings within the same parent span (sorted by `(start_line, -end_line, node_id)`).

### Repo index persistence (Postgres)
- **CGI-FR-019**: Persist deterministic `repo_index.json` to Postgres keyed by `(project_id, repo_fingerprint)`.
- **CGI-FR-020**: Downstream services retrieve `repo_index.json` from Postgres.

---

## Public HTTP API

### `GET /health`
Returns `{ "status": "ok" }`.

### `GET /health/postgres`
- If `DATABASE_URL` is not set: returns `{ "status": "skipped" }`
- Else attempts `SELECT 1` and returns `{ "status": "ok" }` or `503`.

### `GET /health/neo4j`
- If `NEO4J_REPOSITORY_SERVICE_URL` is not set: returns `{ "status": "skipped" }`
- Else calls `{NEO4J_REPOSITORY_SERVICE_URL}/health` and returns `{ "status": "ok" }` or `503`.

### `POST /ingest/git` (JSON)
Body:
- `project_id`: UUID
- `source_language`: `cobol|java|javascript`
- `git_url`: string (must be non-empty)
- `ref`: optional string (checkout target; passed to `git2` revparse)

Response:
- `200 OK`: `{ "project_id": "<uuid>", "repo_fingerprint": "<string>" }`

### `POST /ingest/zip` (multipart form-data)
Fields:
- `project_id`: UUID
- `source_language`: `cobol|java|javascript`
- `file`: ZIP bytes

Response:
- `200 OK`: `{ "project_id": "<uuid>", "repo_fingerprint": "<string>" }`

---

## System workflow (high level)

```mermaid
sequenceDiagram
  autonumber
  participant UI as UI / Client
  participant CGI as code-graph-ingestion-service
  participant NRS as neo4j-repository-service
  participant PG as Postgres

  UI->>CGI: POST /ingest/git|zip (project_id, source_language, source)
  CGI->>CGI: Materialize repo + compute repo_fingerprint
  CGI->>CGI: Build inventory (path, sha256, language, line_count)
  CGI->>CGI: Run selected plugin (exactly one)
  CGI->>CGI: Chunk oversized nodes + add NEXT_CHUNK edges
  CGI->>CGI: Build CONTAINS edges by span nesting
  CGI->>CGI: Validate graph contract
  alt NEO4J_REPOSITORY_SERVICE_URL set
    CGI->>NRS: Merge project/repo/files/nodes/edges (idempotent)
  end
  alt DATABASE_URL set
    CGI->>PG: Upsert repo_index.json (canonical JSON + sha256)
  end
  CGI-->>UI: 200 OK (project_id, repo_fingerprint)
```

---

## Internal architecture (runtime modules)

```mermaid
flowchart TB
  subgraph API[Axum API]
    A1[POST /ingest/zip]
    A2[POST /ingest/git]
    A3[GET /health*]
  end

  subgraph Core[Core pipeline]
    C1[Repo materializer<br/>ZIP extract / Git clone]
    C2[Fingerprint<br/>repo_fingerprint]
    C3[Ignore rules + inventory<br/>path/sha256/language/lines]
    C4[Plugin selection<br/>source_language]
    C5[Selected plugin ingest<br/>nodes/edges/facts]
    C6[Chunking >1000 lines<br/>chunk nodes + NEXT_CHUNK]
    C7[CONTAINS edges<br/>span nesting]
    C8[Graph contract validation]
  end

  subgraph Persist[Optional persistence]
    P1[neo4j-repository-service client]
    P2[Postgres repo_index upsert]
  end

  A1 --> C1
  A2 --> C1
  C1 --> C2 --> C3 --> C4 --> C5 --> C6 --> C7 --> C8
  C8 --> P1
  C8 --> P2
```

---

## Determinism rules (implementation)

- **Stable IDs**: `node_id = sha256("part1|part2|...")[:24]` (see `src/core/stable_ids.rs`).
- **Stable snippet hash**: `snippet_hash = sha256(file_path|start|end|normalized_text)` where normalization is:
  - newline normalization (`\r\n`/`\r` → `\n`)
  - trim trailing spaces/tabs on each line
- **Deterministic ZIP extraction**:
  - rejects zip-slip and symlinks
  - extracts entries sorted by normalized repo-relative path
- **Deterministic file inventory**:
  - repo-relative POSIX paths
  - deterministic ordering by path
  - default ignores include `.git/**`, `node_modules/**`, `target/**`, etc, plus `.gitignore` when present

---

## Graph record contracts (in-service)

### `CodeNodeRecord`
A universal unit-of-code node (language-agnostic) with evidence:
- `project_id`, `repo_fingerprint`
- `node_id` (stable)
- `language`: `cobol|java|javascript|xml|other`
- `kind`: e.g. `program|module|class|method|division|paragraph|data_def|exec_block|chunk|unresolved|xml_config`
- `symbol`: optional display name
- `file_path`, `start_line`, `end_line`
- `snippet_hash`
- `text`: verbatim physical lines slice (or empty for unresolved nodes)
- `extra_labels`: e.g. `__UnresolvedCall__`, `__UnresolvedImport__`, `__Data88__`, etc

### `EdgeRecord`
- `rel_type`: `CALLS|PERFORMS|REFERENCES|IMPORTS|INCLUDES|READS|WRITES|CONFIG_WIRES|CONTAINS|NEXT_CHUNK`
- `confidence`: `0.0..=1.0`
- `metadata`: JSON map (must be stable for de-dup)

### Graph contract validation
- Enforces `project_id`/`repo_fingerprint` consistency across all nodes/edges
- Enforces `INCLUDES.dst_node_id` is a **file_path present in inventory** (not a node id)
- Enforces `node_id` uniqueness in the emitted node set

---

## Plugin pipelines (current behavior)

### Common plugin contract
Each plugin implements:
- deterministic **file selection**
- deterministic extraction of `(nodes, edges, facts)`

### COBOL plugin (`source_language=cobol`)
File selection: `.cbl`, `.cob`, `.cpy`

See also: `COBOL_NODE_KINDS_REFERENCE.md` (Tree-sitter node kinds/fields used by ingestion, plus span mapping rules).

Pipeline:
1) **Preprocess** (`src/plugins/cobol/normalizer.rs`)
   - newline normalization for internal processing, but physical lines preserved as evidence
   - FIXED vs FREE handling, with `>>SOURCE FORMAT` switches
   - continuation merging into logical lines with a logical→physical span map
   - `EXEC ... END-EXEC` collapsed into a single logical marker line `EXEC_BLOCK.` while preserving physical evidence span
2) **Copybook expansion** (`src/plugins/cobol/copybooks.rs`)
   - detects `COPY <name>` and resolves copybook in deterministic search-path order (currently defaults to repo root)
   - supports basic `REPLACING ==A== BY ==B==` substitutions on inserted lines
   - inserted logical lines map back to the `COPY` statement physical span (stable evidence)
   - emits `INCLUDES` edges from the program node to the copybook file path(s)
3) **Unitization** (`src/plugins/cobol/unitizer.rs`)
   - emits a `program` node spanning the full physical file (symbol from `PROGRAM-ID` when found)
   - emits `exec_block` nodes for `EXEC_BLOCK.` markers
   - emits additional fine-grained unit nodes:
     - `copybook` (from COPY expansion provenance; evidence maps to the COPY statement span)
     - `data_item` (from Tree-sitter `data_description` nodes when available; regex fallback in DATA DIVISION)
     - `statement` (Tree-sitter `*_statement` nodes in PROCEDURE DIVISION)
     - `sentence` (conservative period-terminated segmentation using the logical→physical span map)
4) **Semantic linker** (`src/plugins/cobol/semantic_linker.rs`)
   - emits structural span nodes: `division`, `section`, `paragraph`, `data_def`
   - emits semantic edges:
     - `PERFORMS` (resolved to paragraph/section when unique, else to `unresolved` with `__UnresolvedPerform__`)
       - metadata includes `target` and optional `thru` text (for `PERFORM ... THRU ...`)
       - when `thru` is present and uniquely resolves, metadata additionally includes `thru_kind` + `thru_node_id`
     - `CALLS` (emitted as unresolved; repo-global postprocessing resolves literal calls by `PROGRAM-ID`)
     - `READS`/`WRITES` (conservative, from `READ|WRITE|REWRITE|DELETE`; targets are modeled as unresolved `__UnresolvedFile__` / `__UnresolvedRecord__`)
     - `EXEC`-derived:
       - `EXEC SQL`: emits `sql_table` nodes (label `__ExecSqlTable__`) and `READS|WRITES` edges based on conservative SQL keyword heuristics (`SELECT` => `READS`, `INSERT|UPDATE|DELETE|MERGE` => `WRITES`)
       - `EXEC CICS`: emits a conservative `CALLS` edge to an unresolved `CICS:<COMMAND>` node (`__UnresolvedCicsCommand__`); for common program-control commands (`LINK|XCTL|LOAD|START`) also emits a `CALLS` edge to the `PROGRAM(...)` target (literal/dynamic) so repo-global literal resolution can connect it to a `PROGRAM-ID` node when possible
     - `REFERENCES` (qualified references only in MVP; unresolved uses `__UnresolvedDataRef__`)
5) **Repo-global postprocessing (CALL literal resolution)**
   - indexes all COBOL `program` nodes by `PROGRAM-ID`
   - rewrites `CALLS` edges where `metadata.call_type == "literal"` to target the resolved program node when a unique best candidate exists

Facts:
- `file_count`, `includes_count`

### Java plugin (`source_language=java`)
File selection: `.java` + `.xml` (XML ingested only as part of Java ingestion)

Extraction:
- `module` nodes spanning each Java file
- `class` and `method` span nodes (via `tree-sitter-graph` DSL)
- `IMPORTS` edges from module → unresolved import nodes; postprocess resolves to class nodes by:
  - fully-qualified match `package.Class`
  - else unique simple-name match
- conservative `CALLS` edges for method invocations to unresolved nodes (`__UnresolvedCall__`)
- XML wiring:
  - emits an `xml_config` node spanning each XML file
  - extracts `class="..."` attributes and emits `CONFIG_WIRES` edges to unresolved Java class nodes (MVP)

Facts:
- `java_file_count`, `xml_file_count`

### JavaScript plugin (`source_language=javascript`)
File selection: `.js`, `.jsx`, `.mjs`, `.cjs`, `.ts`, `.tsx`

Extraction:
- `module` nodes spanning each file
- `IMPORTS` edges extracted via `tree-sitter-graph` DSL:
  - ES imports, `export ... from`, `require("...")`, dynamic `import("...")`
  - edges start as unresolved module nodes with metadata `{kind, specifier}`
- postprocess resolves `IMPORTS` edges to in-repo module nodes when possible using deterministic heuristics:
  - relative specifiers (`./x`, `../x`) relative to importer dir
  - repo-root absolute specifiers (`/src/x`)
  - unique suffix match for bare specifiers (MVP; resolves only if unique)

Facts:
- `js_file_count`

---

## Persistence

### Neo4j (via `neo4j-repository-service`)
Enabled when `NEO4J_REPOSITORY_SERVICE_URL` is set.

The service calls the repository-service’s idempotent merge endpoints:
- `POST /v1/code-graph/merge-project`
- `POST /v1/code-graph/merge-repo`
- `POST /v1/code-graph/merge-files`
- `POST /v1/code-graph/merge-code-graph`

Timeout:
- `NEO4J_REPOSITORY_TIMEOUT_S` (default `60`)

### Postgres repo index (`repo_index.json`)
Enabled when `DATABASE_URL` is set.

Upserts into `project_repo_indexes(project_id, repo_fingerprint, repo_index_json, content_sha256)` where:
- `repo_index_json` is stored as canonical JSON (sorted keys, no whitespace)
- `content_sha256` is sha256 of those canonical bytes (determinism gate)

---

## Configuration (env)

- `API_PORT` (default `8015`)
- `WORKSPACE_ROOT` (default `workspace`)
- `DATABASE_URL` (optional; enables Postgres upsert)
- `NEO4J_REPOSITORY_SERVICE_URL` (optional; enables graph persistence)
- `NEO4J_REPOSITORY_TIMEOUT_S` (default `60`)

---

## Running locally

From `playground/git-epic-creator/`, start dependencies (example):

```bash
docker compose up -d postgres neo4j redis db-init-service postgresdb-schema-init neo4j-repository-service neo4j-schema-init
```

Run the service:

```powershell
cargo test
$env:API_PORT=8015
# Optional:
# $env:WORKSPACE_ROOT="workspace"
# $env:DATABASE_URL="postgres://postgres:postgres@localhost:5432/postgres"
# $env:NEO4J_REPOSITORY_SERVICE_URL="http://localhost:8080"
cargo run --release
```

---

## Current implementation status / known gaps (vs requirements)

- **Repo fingerprint for ZIP (CGI-FR-005)**: currently `repo_fingerprint = "zip:" + sha256(zip_bytes)` which means ZIP entry order changes the fingerprint; the intended behavior is a content-based hash independent of ZIP container ordering (+ optional dependency manifests).
- **Dependency manifest augmentation (CGI-FR-005)**: not implemented for ZIP or Git; Git fingerprints are the checked-out commit SHA only.
- **COBOL I/O and EXEC modeling limitations (CGI-FR-014)**:
  - `READS|WRITES` targets are conservative and do **not** yet resolve record names (`WRITE|REWRITE`) to their owning `FD` file-name.
  - `EXEC SQL` table extraction prefers parsing the extracted SQL slice with a dedicated SQL grammar, but keeps a regex fallback and remains conservative (e.g. host variables like `:WS-ID` are normalized for parsing; not all vendor SQL dialect features are modeled).
  - `EXEC CICS` program-target extraction is limited to `PROGRAM(...)` for common program-control commands; other CICS resources (files/queues/transids) are not modeled yet.
- **Semantic CONTAINS edges with order metadata**: structural `CONTAINS` is generated centrally via span nesting, but no `order` metadata is emitted.

### External references used by this implementation
- `tree-sitter` overview (incremental parsing + error recovery): `https://github.com/tree-sitter/tree-sitter`
- `tree-sitter-graph` DSL for extracting structured matches from tree-sitter CSTs: `https://github.com/tree-sitter/tree-sitter-graph`
- COBOL parser precedent for keeping `EXEC ... END-EXEC` permissive / extracted-as-text (ANTLR-based): `https://github.com/uwol/proleap-cobol-parser`
- Higher-fidelity SQL inside `EXEC SQL` is implemented by parsing the extracted SQL slice with a dedicated tree-sitter SQL grammar (`tree-sitter-sequel` / DerekStride): `https://github.com/derekstride/tree-sitter-sql`
