# COBOL Tree-sitter node kinds reference (pinned grammar; ingestion-facing)

This document is a **practical, ingestion-facing reference** for COBOL Tree-sitter node kinds and fields that are **actually used** by `code_graph_ingestion_service` for deterministic COBOL ingestion.

It is intentionally **minimal**:
- We document only the node kinds and fields that ingestion relies on.
- We avoid listing every grammar production (the full grammar is large).
- When in doubt, prefer **Tree-sitter AST kinds** over lexical heuristics, and always map back to **physical evidence** via the pipeline’s span map.

The pinned grammar is vendored in:
- `services/tree_sitter_cobol_binding/vendor/tree-sitter-cobol/grammar.js`

---

## How to read this document

Two separate “kind” concepts exist:

- **Tree-sitter node kinds**: `Node::kind()` values produced by the pinned COBOL grammar.
- **Graph node kinds**: `CodeNodeRecord.kind` values emitted by ingestion (e.g., `program`, `statement`, `sentence`, `data_item`).

This document focuses on **Tree-sitter node kinds / fields** and how they map into graph node kinds.

---

## Deterministic span mapping (critical)

The COBOL pipeline uses two streams:
- **physical lines**: verbatim evidence
- **logical lines**: normalized parse stream (continuations merged, COPY expanded, EXEC collapsed)

Tree-sitter parsing happens over the **logical parse stream**.

To create evidence-backed nodes, ingestion must map any Tree-sitter node span back to physical lines using:
- `PreprocessResult.logical_spans[row] -> (start_physical_line, end_physical_line)`

This is the foundation for high-confidence spans under COPY expansion and EXEC collapsing.

---

## Top-level program structure (Tree-sitter)

These nodes are used to anchor structure extraction:

- **`program_definition`**
  - contains divisions: `identification_division`, `environment_division`, `data_division`, `procedure_division`
- **`identification_division`**
  - contains `program_name` (used to derive `PROGRAM-ID` symbol)
- **`environment_division`**
- **`data_division`**
- **`procedure_division`**

### Ingestion usage
- `src/plugins/cobol/unitizer.rs`
  - finds `procedure_division` to extract `statement`/`sentence` units
  - extracts `data_description` (see below) for `data_item`
- `src/plugins/cobol/semantic_linker.rs`
  - uses division kinds to emit `division` nodes

---

## Procedure structure (Tree-sitter)

### Paragraph headers
- **`paragraph_header`**
  - used by the semantic linker to emit `paragraph` nodes

### Statements
The grammar exposes a large set of statement nodes which follow the naming convention:
- **`*_statement`** (examples: `call_statement`, `read_statement`, `write_statement`, `move_statement`, …)

The grammar also exposes:
- **`_end_statement`** / `end_statement` (statement terminators / punctuation)

### Ingestion usage
- `src/plugins/cobol/unitizer.rs`
  - emits graph nodes:
    - **`statement`**: one per Tree-sitter `*_statement` node (span-mapped to physical evidence)
    - **`sentence`**: preferably segmented by Tree-sitter end-statement nodes; fallback to period-terminated logical line scan

---

## Data Division structure (Tree-sitter)

### Data description entries
- **`data_description`**
  - corresponds to a COBOL “data description entry” (level-number + data-name + clauses)

### Ingestion usage
- `src/plugins/cobol/unitizer.rs`
  - emits graph nodes:
    - **`data_item`**: extracted from `data_description` nodes when parse is available
  - fallback (when Tree-sitter unavailable): regex scan in DATA DIVISION

Note: deeper clause semantics (PICTURE parsing, VALUE clauses, OCCURS/REDEFINES) are **not** modeled at unitization time; we only extract stable span nodes.

---

## COPY handling (important nuance)

The pinned grammar includes `copy_statement`, but it is configured as an **extra token** (not a normal AST node). As a result:
- you should not rely on Tree-sitter to surface COPY nodes in the AST
- you should treat COPY as a **pipeline concern**, not a grammar concern

### Ingestion usage
- `src/plugins/cobol/copybooks.rs`: expands COPY deterministically and produces an `expansion_map`
- `src/plugins/cobol/unitizer.rs`: emits graph nodes:
  - **`copybook`** units derived from COPY expansion provenance
  - evidence spans map to the COPY statement physical span (stable, high-confidence)

---

## Node kinds + fields relied on by tree-sitter-graph (TSG) extraction

`src/plugins/tsg/mod.rs` uses tree-sitter-graph to extract high-signal syntax patterns and then emits conservative edges.

### CALL
- Node kind: **`call_statement`**
- Field: **`x`** (call target)

Captured into:
- `CobolCallHit { callee, call_type, logical_row }`

### PERFORM (including THRU)
- Node kind: **`perform_statement_call_proc`**
- Field: `procedure` → `perform_procedure`
- Captures:
  - target label (required)
  - THRU label (optional; also recoverable from logical line fallback)

Captured into:
- `CobolPerformHit { target, thru, logical_row }`

### I/O verbs
Used for conservative READS/WRITES:
- Node kind: **`read_statement`** (field: `file_name`)
- Node kind: **`write_statement`** (field: `record_name`)
- Node kind: **`rewrite_statement`** (field: `record`)
- Node kind: **`delete_statement`** (field: `file_name`)

Captured into:
- `CobolIoHit { op, target, logical_row }`

---

## Recommended “stable subset” for downstream work

If you’re building further semantics, prefer these as stable anchors:
- `procedure_division`
- `paragraph_header`
- `*_statement` + `_end_statement`
- `data_division` + `data_description`

And rely on the pipeline outputs:
- COPY expansion + provenance
- EXEC collapsing markers (`EXEC_BLOCK.`) + exec unit nodes

---

## Regeneration / maintenance notes

Because the vendored grammar does not ship `node-types.json`, keep this reference aligned by:
- checking `grammar.js` when upgrading the pinned commit
- keeping `src/plugins/tsg/mod.rs` DSL captures and `unitizer.rs` kind checks consistent with the grammar


