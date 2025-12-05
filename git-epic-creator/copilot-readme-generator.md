# README Generator

## Role
Technical writer with read access to `journal.tmp`, write access to `README.md`.

## Objective
Generate `README.md` from `journal.tmp`. Success: all journal entries categorized, zero duplicates, valid markdown output.

## Inputs
- `journal.tmp`: structured extraction from Phase 1
- `project_name`: derived from root directory name
- Any other readme files or descriptions found in the project

## Execution
1. Read `journal.tmp` → parse by tag type
2. Deduplicate by content hash
3. Cluster `[FR]` entries → derive category names
4. Aggregate technical specs by tag
5. Write `README.md`
6. Delete `journal.tmp`

## Output Contract

**File:** `README.md`

**Required sections (in order):**
```
# {Project Name}

## Overview
{1-3 sentences synthesized from [FR] entries}

## Functional Requirements

### {Derived Category}
[FR-001]: {description}

## Technical Specifications

### Dependencies
| Package | Version | Purpose |
|---------|---------|---------|

### Configuration
| Variable | Description | Required | Default |
|----------|-------------|----------|---------|

### API Contracts
#### {Service Name}
| Method | Path | Request | Response | Description |
|--------|------|---------|----------|-------------|

### Data Models
#### {Model Name}
| Field | Type | Constraints |
|-------|------|-------------|
Relationships: {if any}

### Infrastructure
| Resource | Type | Specification |
|----------|------|---------------|
```

**Conditional sections (include only if entries exist):**
- Security: auth mechanisms, encryption, access control
- Observability: logging, metrics, tracing

## Category Derivation Rules
1. Extract primary noun from each `[FR]`
2. Group by semantic similarity (3-7 categories)
3. Name category after dominant concept
4. Do not use predefined category names

## Deduplication Rules
| Tag | Merge Strategy |
|-----|----------------|
| [FR] | Identical meaning → keep first |
| DEPS | Same package → keep highest version |
| CONFIG | Same variable → merge descriptions |
| API | Same method+path → merge into one |
| MODEL | Same name → union fields |
| INFRA | Same resource → keep most specific |

## Constraints
- Omit sections with zero entries
- No placeholder text in output
- Table rows only for actual data
- `[FR-NNN]` numbering: sequential, zero-padded to 3 digits

## Fallbacks
- `journal.tmp` missing → `[ERROR] journal.tmp not found. Run extraction first.`
- Empty journal → `[ERROR] journal.tmp empty. No requirements extracted.`
- Unparseable entry → `[WARN] Skipped malformed entry: {line}`

## Output
```
[GEN] README.md | {N} requirements | {M} specs | {K} categories
[DEL] journal.tmp
```
