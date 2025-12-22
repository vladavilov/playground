# Code Graph Ingestion Service — Implementation Guide

This document defines **how to implement** the Code Graph Ingestion Service. Every section references a concrete requirement ID from `README.md`.

---

## 1) Core libraries to reuse

### Parsing
- **Tree-sitter (multi-language parsers)**: `tree-sitter-language-pack`
  - API: `get_language(name)`, `get_parser(name)`
  - Use for **Java**, **JavaScript**, and **XML** parsing and Tree-sitter queries.
  - Requirement mapping: **CGI-FR-008**, **CGI-FR-015**, **CGI-FR-016**, **CGI-FR-017**

- **COBOL Tree-sitter wheel (precompiled)**: `tree-sitter-cobol` (import name: `tree_sitter_cobol`)
  - Implemented and maintained in this repo at `services/tree_sitter_cobol_binding/`.
  - Consumer API surface includes:
    - `tree_sitter_cobol.language()` (default dialect capsule)
    - `tree_sitter_cobol.languages()` and dialect helpers (`language_ibm()`, `language_micro_focus()`, `language_gnucobol()`)
    - `tree_sitter_cobol.grammar_revisions()` / `__grammar_version__`
  - Requirement mapping: **CGI-FR-008**, **CGI-FR-010**, **CGI-FR-012**

Example (language-pack API):

```python
from tree_sitter_language_pack import get_language, get_parser
from tree_sitter import Query, QueryCursor

java_parser = get_parser("java")
java_lang = get_language("java")

tree = java_parser.parse(b"class A { void m(){} }")
query = Query(java_lang, "(method_declaration name: (identifier) @m)")
captures = QueryCursor(query).captures(tree.root_node)
for node in captures.get("m", []):
    print("m", node.text.decode())
```

### Neo4j
- **Neo4j Python driver**: `neo4j` (official driver)
  - Prefer `driver.execute_query()` for simple idempotent `MERGE` statements.
  - Use `session.execute_write()` transaction functions for large batched writes.
  - Requirement mapping: **CGI-FR-006**

### Repo index persistence (Postgres)
- **SQLAlchemy + psycopg (or asyncpg)** using the shared `shared/src/models/*` models
  - Store `repo_index.json` as JSONB in `project_repo_indexes`.
  - Requirement mapping: **CGI-FR-019**, **CGI-FR-020**

### Repo materialization
Choose one deterministic path and keep it stable:
- **ZIP**: built-in `zipfile` (no external deps)
- **Git**: prefer a pure-Python Git library for container portability (e.g., `dulwich`) OR explicitly depend on `git` CLI. Do not mix strategies per run.
  - Requirement mapping: **CGI-FR-001**, **CGI-FR-002**, **CGI-FR-005**

### Ignore rules
- `pathspec` for gitignore-compatible filtering.
  - Requirement mapping: **CGI-FR-005** (reproducibility) + **CGI-FR-006** (stable inventory)

---

## 1.1 Health endpoint

Requirement mapping: **CGI-FR-004**

This service **does not override** the shared `/health` route provided by `services/shared/src/utils/app_factory.py`.

- `GET /health` returns a simple `{ "status": "ok" }`
- Detailed checks are exposed via:
  - `GET /health/postgres`
  - `GET /health/neo4j`

## 2) Proposed high-level file structure

> This is the target code layout; you can implement it iteratively.

```
code_graph_ingestion_service/
  src/
    main.py                      # FastAPI app wiring (CGI-FR-001/002/004)
    api/
      routers/ingest.py           # /ingest/zip and /ingest/git endpoints
      schemas.py                  # Pydantic request/response models
    core/
      orchestrator.py             # end-to-end orchestration
      fingerprint.py              # repo_fingerprint algorithm (CGI-FR-005)
      inventory.py                # file inventory + language detection
      repo_index.py               # repo_index.json builder (CGI-FR-019)
      ignore_rules.py             # gitignore + defaults
    plugins/
      base.py                     # LanguagePlugin interface (CGI-FR-008)
      registry.py                 # plugin registry + ordering (CGI-FR-009)
      cobol/
        normalizer.py             # fixed/free + continuation (CGI-FR-010/011)
        copybooks.py              # COPY REPLACING expansion (CGI-FR-013)
        unitizer.py               # program/div/section/paragraph/... (CGI-FR-012)
        edges.py                  # CALLS/PERFORMS/EXEC SQL/CICS (CGI-FR-014)
      java/
        unitizer.py               # package/class/method nodes (CGI-FR-015)
        edges.py                  # IMPORTS/CALLS conservative
      javascript/
        resolver.py               # module resolution (CGI-FR-017)
        unitizer.py               # module/function nodes
        edges.py
      xml/
        unitizer.py               # XmlBean nodes
        edges.py                  # CONFIG_WIRES (CGI-FR-016)
    persistence/
      neo4j_writer.py             # MERGE patterns + batching
      repo_index_store.py         # Postgres upsert/read (CGI-FR-019/020)
    observability/
      logging.py                  # structured logs
      tracing.py                  # OpenTelemetry wiring
```

---

## 2.1) Plugin framework (required for multi-language ingestion)

Requirement mapping: **CGI-FR-008**, **CGI-FR-009**, **CGI-FR-004A**

The orchestrator must treat language logic as plugins while keeping **one shared graph writer** and **one shared inventory/fingerprint** subsystem.

**Important behavioral contract:** plugins are **not** run sequentially; each request runs **exactly one** plugin selected by `source_language`.

Suggested interface (Python 3.12 typing):

```python
from __future__ import annotations

from dataclasses import dataclass
from pathlib import Path
from typing import Iterable, Protocol

@dataclass(frozen=True)
class IngestionContext:
    project_id: str
    repo_fingerprint: str
    repo_root: Path
    source_language: str  # "cobol" | "java" | "javascript"
    # add: copybook_paths, thresholds, etc.

class LanguagePlugin(Protocol):
    name: str                   # e.g., "cobol"
    file_globs: tuple[str, ...]

    def iter_files(self, ctx: IngestionContext) -> Iterable[Path]:
        \"\"\"Return repo-relative source files this plugin owns (sorted).\"\"\"

    def ingest(self, ctx: IngestionContext, files: list[Path]) -> tuple[list["CodeNodeRecord"], list["EdgeRecord"], dict]:
        \"\"\"Produce CodeNodes + edges + repo-index facts. Must be deterministic.\"\"\"

def select_plugin(ctx: IngestionContext, plugins: list[LanguagePlugin]) -> LanguagePlugin:
    by_name = {p.name: p for p in plugins}
    try:
        return by_name[ctx.source_language]
    except KeyError:
        raise ValueError(f"Unsupported source_language={ctx.source_language!r}. Supported={sorted(by_name)}")

def run_selected_plugin(ctx: IngestionContext, plugins: list[LanguagePlugin]) -> tuple[list[\"CodeNodeRecord\"], list[\"EdgeRecord\"], dict]:
    plugin = select_plugin(ctx, plugins)
    files = sorted(plugin.iter_files(ctx), key=lambda p: p.as_posix())
    nodes, edges, facts = plugin.ingest(ctx, files)
    return nodes, edges, {plugin.name: facts}
```

## 3) Data contracts inside the service

### 3.1 Unit-of-code record (input to Neo4j writer)
Requirement mapping: **CGI-FR-007**, **CGI-FR-012**, **CGI-FR-015**

```python
from dataclasses import dataclass
from typing import Literal, Optional

Language = Literal["cobol", "java", "javascript", "xml", "other"]

@dataclass(frozen=True)
class CodeNodeRecord:
    project_id: str
    repo_fingerprint: str
    node_id: str                # stable
    language: Language
    kind: str                   # see README (universal kinds)
    symbol: Optional[str]
    file_path: str
    start_line: int
    end_line: int
    snippet_hash: str           # stable
    text: str                   # verbatim physical lines
    extra_labels: tuple[str, ...] = ()
```

### 3.2 Relationship record
Requirement mapping: **CGI-FR-014**, **CGI-FR-018**

```python
@dataclass(frozen=True)
class EdgeRecord:
    project_id: str
    repo_fingerprint: str
    rel_type: str
    src_node_id: str
    dst_node_id: str
    confidence: float
    metadata: dict
```

---

## 4) Deterministic IDs and hashes

### 4.1 Stable `node_id`
Requirement mapping: **CGI-FR-007**

Rules:
- Derive from **semantic identity** + **physical span**.
- Do not include random values.

Suggested implementation:

```python
import hashlib

def stable_node_id(*parts: str, size: int = 24) -> str:
    raw = "|".join(parts).encode("utf-8")
    return hashlib.sha256(raw).hexdigest()[:size]

# Example:
# node_id = stable_node_id(project_id, repo_fingerprint, file_path, kind, symbol or "", str(start_line), str(end_line))
```

### 4.2 `snippet_hash`
Requirement mapping: **CGI-FR-011**

- Hash over `(file_path, start_line, end_line)` and a **normalized** representation of `text`.
- Normalization must not change evidence meaning; limit to newline normalization and trimming trailing whitespace.

---

## 5) COBOL plugin — the hard part

### 5.0 COBOL Tree-sitter grammar binding (compiled) — build, ship, verify

Requirement mapping: **CGI-FR-008**, **CGI-FR-010**, **CGI-FR-012**

Because `tree-sitter-language-pack` does not reliably provide COBOL across platforms, the COBOL pipeline MUST carry its own **compiled tree-sitter COBOL grammar binding** shipped as a **dedicated Python wheel** (no runtime compilation, no dynamic-library fallback).

**Golden source (single source of truth):**
- `playground/git-epic-creator/services/tree_sitter_cobol_binding/README.md`

Those documents define:
- the wheel’s required API (`tree_sitter_cobol.language()` capsule contract),
- the build matrix (Windows + manylinux minimum),
- the dialect strategy (“preprocess first”, grammar evolution rules, and optional per-dialect variants), and
- the corpus-driven CI gates for multi-dialect parsing quality.

How the COBOL plugin MUST load the grammar (consumer usage):

```python
from tree_sitter import Language, Parser
import tree_sitter_cobol

COBOL = Language(tree_sitter_cobol.language())
parser = Parser()
parser.language = COBOL
```

Dialect selection (optional, configuration-driven):
- If the ingestion run is configured with a dialect key (e.g., `COBOL_DIALECT=ibm`), load it via the wheel helpers:

```python
from tree_sitter import Language
import tree_sitter_cobol

lang_capsule = tree_sitter_cobol.languages().get("ibm", tree_sitter_cobol.language())
COBOL = Language(lang_capsule)
```

Consumer responsibility split (critical):
- The `tree_sitter_cobol` wheel provides a deterministic COBOL `Language` object (AST when parsing quality is sufficient).
- The **COBOL ingestion plugin** is responsible for deterministic preprocessing before parsing:
  - newline normalization
  - FIXED/FREE + columns/indicator rules
  - inline comment conventions (e.g., `*>`)
  - COPY/REPLACING expansion with search paths + expansion map
  - `EXEC ... END-EXEC` detection; treat embedded languages as opaque
  - (optional) EBCDIC → Unicode decode before normalization/parsing (and record evidence metadata)

Preprocess→Parse contract (mandatory):
- The COBOL pipeline MUST produce two streams per file:
  - **evidence stream**: original physical lines verbatim (used for persisted `__CodeNode__.text`)
  - **parse stream**: normalized bytes used for Tree-sitter parsing
- The parse stream MUST preserve enough line structure so that AST nodes can be mapped back to physical `(start_line, end_line)` ranges.

### 5.1 Fixed/Free format normalizer
Requirement mapping: **CGI-FR-010**, **CGI-FR-011**

Implementation algorithm (single pass):
1. Default mode = FIXED.
2. For each physical line:
   - if directive `>>SOURCE FORMAT FREE|FIXED` found, switch mode for subsequent lines.
   - classify line (comment/debug/continuation/normal) based on mode.
3. Build **logical lines** for parsing purposes by merging continuations, BUT keep a mapping:
   - logical line → `[start_physical_line, end_physical_line]`
4. Emit:
   - `physical_lines[]` (verbatim) for evidence
   - `logical_stream` for parsing/tokenization

Pseudo-code:

```python
mode = "FIXED"
logical = []
current = []
current_span = None

for i, line in enumerate(physical_lines, start=1):
    if ">>SOURCE FORMAT FREE" in line: mode = "FREE"
    if ">>SOURCE FORMAT FIXED" in line: mode = "FIXED"

    kind = classify(mode, line)
    if kind == "comment":
        flush_if_needed()
        logical.append((line, (i, i), {"comment": True}))
        continue

    content, is_cont = extract_content(mode, line)
    if not current:
        current = [content]
        current_span = (i, i)
    elif is_cont:
        current.append(content)
        current_span = (current_span[0], i)
    else:
        logical.append(("".join(current), current_span, {}))
        current = [content]
        current_span = (i, i)

flush_end()
```

### 5.2 Copybook expansion (`COPY ... REPLACING`)
Requirement mapping: **CGI-FR-013**

Core rules:
- Resolve copybooks using ordered search paths.
- Apply `REPLACING` at token-level before insertion.
- Emit `INCLUDES` edges from the program/section node to the `File` node representing the copybook.
- Maintain an **expansion map** ONLY for copybook inserts:
  - inserted span in expanded stream → source file path + physical lines

### 5.3 Unitization state machine (Programs → Divisions → Sections → Paragraphs → Sentences → Statements)
Requirement mapping: **CGI-FR-012**

Strategy:
- **Primary**: Use Tree-sitter COBOL AST when it is available and passes the deterministic capability check (defined in `services/tree_sitter_cobol_binding/README.md`).
- **Fallback**: Use deterministic token scanning/state-machine unitization when:
  - the COBOL grammar cannot be loaded, or
  - parsing yields too many `ERROR`/missing nodes, or
  - the file is in a dialect the grammar cannot handle reliably.

How to ensure Tree-sitter AST is available:
- CI must build and publish/install the COBOL binding wheel (Approach A).
- A startup/health probe must verify readiness using the same snippet and threshold check as runtime.

How to determine “AST is partial” for a given file (per-file gate):
1. Parse the file bytes to a syntax tree.
2. Compute `has_error = tree.root_node.has_error`.
3. Optionally compute `error_ratio` by traversing nodes and counting `type == "ERROR"` or `node.is_missing`.
4. Use AST mode if:
   - `has_error is False`, OR
   - `error_ratio <= COBOL_TS_MAX_ERROR_RATIO` (permissive mode).
   Otherwise fall back.

Minimum state variables:
- `program_stack`, `current_division`, `current_section`, `current_paragraph`, `current_sentence_start`

Key transitions:
- On `PROGRAM-ID`: open program unit
- On `END PROGRAM`: close program unit
- On `... DIVISION.` headers: close/open division
- On `<name> SECTION.`: close/open section
- On `<name>.` paragraph label: close/open paragraph
- On `.` period within procedure division: close current sentence
- On `EXEC ... END-EXEC`: emit `exec_block` unit node

### 5.4 Edge extraction (CALLS/PERFORMS/SQL/CICS + data item usage)
Requirement mapping: **CGI-FR-014**

Implementation rules:
- **CALL literal**: `confidence=1.0`, resolve destination by `PROGRAM-ID` symbol
- **CALL identifier (dynamic)**: emit edge to a dedicated `__CodeNode__` (kind=`unresolved`, extra label `__UnresolvedCall__`) with `confidence=0.2`
- **PERFORM**: resolve paragraph/section targets deterministically by label; `confidence=1.0`
- **EXEC CICS**: parse `LINK`/`XCTL` blocks; literal program names produce `CALLS confidence=0.8` with metadata `{protocol:"CICS"}`
- **EXEC SQL**: token scan table identifiers; emit to `__CodeNode__` nodes (kind=`db_table`, extra label `__DbTable__`) with `confidence=0.6`
- **READS/WRITES**: conservative pattern matching for verbs (`MOVE`, `COMPUTE`, `ADD`, `READ`, `WRITE`); if uncertain, prefer `READS` with low confidence.

---

## 6) Oversized node chunking

Requirement mapping: **CGI-FR-018**

Deterministic chunking algorithm:
1. Prefer AST boundaries (methods, paragraphs, functions).
2. Else use language markers.
3. Else window partition (150–250 lines with fixed overlap).
4. Greedy merge adjacent blocks while cosine similarity ≥ 0.80.
5. Emit chunk nodes `N#1..N#k` and synthetic `CALLS` pipe edges with metadata.

---

## 7) Neo4j write strategy

Requirement mapping: **CGI-FR-006**

Guidelines:
- Use `MERGE` on unique keys for `__Project__`, `__Repo__`, `__File__`, `__CodeNode__`.
- Batch writes with `UNWIND` for throughput.
- Use one write transaction per batch.

Example batch upsert for `__CodeNode__`:

```cypher
UNWIND $rows AS row
MERGE (n:__CodeNode__ { project_id: row.project_id, repo_fingerprint: row.repo_fingerprint, node_id: row.node_id })
SET n.language = row.language,
    n.kind = row.kind,
    n.symbol = row.symbol,
    n.file_path = row.file_path,
    n.start_line = row.start_line,
    n.end_line = row.end_line,
    n.snippet_hash = row.snippet_hash,
    n.text = row.text
WITH n, row
CALL apoc.create.addLabels(n, row.extra_labels) YIELD node
RETURN count(*)
```

---

## 8) Postgres `repo_index.json` (project manifest)

Requirement mapping: **CGI-FR-019**, **CGI-FR-020**

`repo_index.json` MUST include (minimum):
- `project_id`, `repo_fingerprint`, `created_at`
- deterministic `files[]` inventory (path, sha256, language, line_count)
- `symbols[]` (exported/public + entrypoints) and normalization map
- `cobol` summary (copybook resolution stats, unresolved call rate)

### 8.1 Table and model
The shared DB layer MUST include a model like:
- `ProjectRepoIndex(project_id, repo_fingerprint, repo_index_json, content_sha256, created_at, updated_at)`

Minimal DDL (conceptual):

```sql
CREATE TABLE project_repo_indexes (
  id uuid PRIMARY KEY,
  project_id uuid NOT NULL,
  repo_fingerprint text NOT NULL,
  repo_index_json jsonb NOT NULL,
  content_sha256 text NOT NULL,
  created_at timestamptz NOT NULL,
  updated_at timestamptz NOT NULL,
  UNIQUE(project_id, repo_fingerprint)
);
CREATE INDEX project_repo_indexes_project_fingerprint_idx
  ON project_repo_indexes(project_id, repo_fingerprint);
```

### 8.2 Canonical JSON + sha256 (determinism gate)
To prevent “same repo, different index bytes” drift:
- Serialize JSON in a canonical form (sorted keys, no whitespace)
- Persist `content_sha256 = sha256(canonical_bytes)`

### 8.3 Upsert algorithm
1. Build `repo_index` dict deterministically (sorted files, sorted symbols).
2. Compute canonical JSON bytes + sha256.
3. Upsert into Postgres by `(project_id, repo_fingerprint)`.
SQL upsert sketch:

```sql
INSERT INTO project_repo_indexes(project_id, repo_fingerprint, repo_index_json, content_sha256)
VALUES (:project_id, :repo_fingerprint, :repo_index_json, :content_sha256)
ON CONFLICT (project_id, repo_fingerprint)
DO UPDATE SET repo_index_json = EXCLUDED.repo_index_json,
              content_sha256 = EXCLUDED.content_sha256,
              updated_at = now();
```


