# `tree_sitter_cobol_binding` — COBOL Tree-sitter grammar (Rust-first, deterministic)

This folder is the **single source of truth** for the **vendored COBOL Tree-sitter grammar sources** and the **Rust language crate** used by Rust services.

We maintain this because:

- `tree-sitter-language-pack` has historically been unreliable for COBOL coverage across platforms.
- Enterprise COBOL ingestion needs **deterministic**, **pinned**, **auditable** grammar builds.

---

## What you get

## Rust usage (for Rust services)

This folder also contains a small **Rust Tree-sitter language crate** that compiles the
committed/generated COBOL parser sources and exposes the standard `LANGUAGE.into()`
API used by other `tree-sitter-*` crates.

- Crate path: `services/tree_sitter_cobol_binding/rust/tree-sitter-cobol`

Add it to a Rust service:

```toml
[dependencies]
tree-sitter = "0.24"
tree-sitter-cobol = { path = "../tree_sitter_cobol_binding/rust/tree-sitter-cobol" }
```

And use it:

```rust
let mut parser = tree_sitter::Parser::new();
parser.set_language(&tree_sitter_cobol::LANGUAGE.into())?;
let tree = parser.parse(source, None).unwrap();
```

## Developer tasks (regen)

Regenerating `vendor/tree-sitter-cobol/src/parser.c` / `scanner.c` is developer-only (requires Node.js `npx`):

```powershell
cargo run -p xtask -- regen
```

## Functional requirements (non-negotiable)

- **No runtime compilation via Node/tree-sitter-cli in services**
  - Runtime must not require Node/npm or `tree-sitter generate`.
- **Deterministic builds**
  - Generated parser sources (`parser.c`, and `scanner.c` when present) are committed.
  - The vendored grammar source is **pinned by commit** and recorded in code (`src/_meta.py`) and in this README.
  - Build tool versions are pinned (see “Build toolchain (pinned)”).
- **Multi-dialect readiness**
  - The wheel must expose a stable way to select a dialect key (even if currently aliased).
  - Dialect variation is handled **first** via deterministic preprocessing in the consuming pipeline (fixed/free, columns, directives, COPY/REPLACE expansion, EXEC blocks). Where preprocessing is insufficient, we either extend the grammar or ship dialect variants.
- **Platform coverage (minimum)**
  - Windows `win_amd64`
  - Linux `manylinux_x86_64`

---

## Current implementation status (what is actually shipped today)

- **Grammar**
  - Baseline grammar source: [BloopAI/tree-sitter-cobol](https://github.com/BloopAI/tree-sitter-cobol.git)
  - Vendored under: `src/libs/tree-sitter-cobol/`
  - Pinned commit (all current dialect keys): `8ba6692cc3c2bded0693d198936c6e26e6501230`
- **Dialect support model**
  - Public API exposes dialect keys: `cobol`, `ibm`, `micro_focus`, `gnucobol`
  - **Today** they all return the **same** grammar capsule (aliases). This keeps the wheel simple and deterministic while we validate dialect handling primarily through preprocessing + fixtures. Cobol85 is the default dialect, expanded with additional features for IBM.
  
- **Tests / quality gates**
  - Fixture corpora live under `tests/fixtures/<dialect>/`.
  - `tests/test_parse_quality.py` enforces:
    - `cobol_core` is **strict** (`root.has_error == False`)
    - dialect folders are **thresholded** (`error_ratio <= COBOL_TS_MAX_ERROR_RATIO`, default `0.02`)

---

## Architecture

### High-level design

- **Vendored grammar sources** live in `vendor/tree-sitter-cobol/`:
  - `grammar.js` (source)
  - `src/parser.c`, `src/scanner.c` (generated; committed)
  - `src/tree_sitter/parser.h` (header)
- **A Rust language crate** `rust/tree-sitter-cobol`:
  - Compiles the committed C sources via `build.rs`.
  - Exposes `tree_sitter_cobol::LANGUAGE` for Rust services.

### Key implementation details (for maintainers)

- **Capsule name**
  - The C extension returns `PyCapsule_New((void*)lang, "tree_sitter.Language", NULL)`.
  - That capsule name string **must** remain `tree_sitter.Language` for `tree_sitter.Language(...)` to accept it.
- **Exported grammar symbol**
  - The current vendored grammar exports `tree_sitter_COBOL()` (uppercase).
  - The binding references it explicitly in `src/_binding.c`.
- **Build inputs**
  - Extension build definition lives in `setup.py` and compiles:
    - `src/_binding.c`
    - `src/libs/tree-sitter-cobol/src/parser.c`
    - `src/libs/tree-sitter-cobol/src/scanner.c`
- **Versioning policy**
  - The package version encodes the pinned grammar revision, e.g. `0.1.0+grammar.8ba6692` (see `pyproject.toml`).
  - The exact full SHA(s) are recorded in `src/_meta.py` and exposed at runtime via `grammar_revisions()`.

---

## Dialect strategy (how we get to “parses real enterprise repos”)

COBOL “dialects” are rarely solved purely by grammar. In IBM/Micro Focus/GnuCOBOL codebases, **preprocessing and source-format normalization** can change the token stream dramatically.

This wheel intentionally focuses on providing a deterministic Tree-sitter **COBOL language object**. The **consuming pipeline** must normalize/expand source before parsing.

### What must be handled outside the grammar (pipeline responsibility)

- **Newline normalization** (`\r\n`/`\r` → `\n`)
- **Source format**: FIXED vs FREE, including directives that switch format mid-stream
- **Card-image columns (FIXED)**
  - cols 1–6 sequence area, col 7 indicator, cols 8–72 text, cols 73+ identification area
  - comment/debug/continuation semantics driven by col 7
- **Inline comment conventions**: `*>` comment-to-EOL (common enterprise behavior)
- **COPY / REPLACE expansion**
  - resolve copybooks via configured search paths
  - apply `REPLACING` pseudo-text (`==...==`) deterministically
  - preserve an expansion map for traceability back to original files/lines
- **`EXEC ... END-EXEC` blocks**
  - detect and preserve embedded SQL/CICS/IMS/DLI blocks as evidence artifacts
  - treat them as opaque content for the COBOL parser (do not attempt to parse embedded languages here)
- **Encoding/codepage**
  - decode EBCDIC to Unicode/UTF-8 before normalization/parsing and record evidence metadata

### “Supported dialect” (measurable)

A dialect is considered supported when its fixture corpus:

- imports + parses without catastrophic failures
- meets the configured quality gate (`has_error` for core; `error_ratio` threshold for dialect folders)

---

## Setup & usage

### Runtime install (consumer)

Install the package (from your internal artifact store or index):

```powershell
python -m pip install tree-sitter-cobol
```

Verify it works (capability check):

```powershell
python -c "from tree_sitter import Language; import tree_sitter_cobol; Language(tree_sitter_cobol.language()); print(tree_sitter_cobol.grammar_revisions())"
```

### Developer setup

```powershell
python -m venv .venv
.\.venv\Scripts\Activate.ps1
python -m pip install -U pip
python -m pip install -e ".[test]"
pytest -q
```

---

## Build (developer / CI)

This project is a standard **PEP 517** wheel that compiles a small CPython extension linking the committed Tree-sitter `parser.c` / `scanner.c`.

### Build toolchain (pinned)

Pinned versions live in scripts and `pyproject.toml`:

- Python: **3.12**
- Tree-sitter runtime: `tree-sitter==0.25.1`
- Build frontend: `build==1.2.2.post1`
- Wheel matrix builder: `cibuildwheel==2.22.0`
- Tests: `pytest==8.3.4`

### Local build (current platform only)

```powershell
python -m pip install -U pip
python -m pip install "build==1.2.2.post1"
python -m build
```

### Multi-platform wheel build (recommended)

Use the scripts:

- Windows: `scripts/build_wheels.ps1`
- Linux/macOS: `scripts/build_wheels.sh`

Or run directly:

```powershell
python -m pip install "cibuildwheel==2.22.0"
python -m cibuildwheel --output-dir wheelhouse
```

Outputs land in `wheelhouse/`.

---

## Updating / regenerating the vendored grammar (maintainers)

### Regenerate `parser.c` / `scanner.c` (developer-only)

Only needed if `grammar.js` changes.

- Requires Node.js (developer machine / CI only)
- Uses pinned `tree-sitter-cli` **0.20.7**

Run:

- Windows: `scripts/regen_parser_sources.ps1`
- Linux/macOS: `scripts/regen_parser_sources.sh`

### Upgrade the pinned grammar commit (checklist)

1. Update the vendored grammar under `src/libs/tree-sitter-cobol/` to the desired commit.
2. Regenerate parser sources (scripts above) and commit outputs.
3. Update pinned SHAs in `src/_meta.py` (`GRAMMAR_REVISIONS`).
4. Update `pyproject.toml` version suffix (e.g. `+grammar.<shortsha>`).
5. Run `pytest -q` locally.
6. Build wheels via `cibuildwheel` and publish artifacts.

---

## Contract with Code Graph Ingestion Service

This wheel exists to satisfy the ingestion pipeline’s need for a deterministic COBOL Tree-sitter binding:

- `code_graph_ingestion_service/IMPLEMENTATION.md` §5.0 (COBOL Tree-sitter binding)
- Functional requirements: **CGI-FR-008**, **CGI-FR-010**, **CGI-FR-012**

At startup, the consuming service must be able to do a deterministic import+parse check (see “Setup & usage”).

---

## Non-goals (explicit)

- This wheel does **not** implement a COBOL preprocessor (COPY library resolution, REPLACE rules, conditional compilation).
- This wheel does **not** parse embedded sublanguages inside `EXEC ... END-EXEC` (SQL/CICS/IMS/DLI).
- This wheel does **not** guarantee “no error nodes” on vendor corpora; dialect support is defined by measurable fixture gates.
