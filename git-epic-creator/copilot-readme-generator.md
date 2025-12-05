# README Generator

Generate `README.md` from `journal.tmp`.

## Execution

1. Read `journal.tmp`
2. Parse entries: `[FR]`, `DEPS`, `CONFIG`, `API`, `MODEL`, `INFRA`
3. Deduplicate identical items
4. Derive domain categories from actual requirements (do not use predefined categories)
5. Renumber: `[FR-001]` through `[FR-N]`
6. Aggregate technical specifications
7. Generate `README.md`
8. Delete `journal.tmp`

## Category Derivation

Analyze `[FR]` entries and group by semantic similarity:
- Extract primary noun/verb from each requirement
- Cluster related requirements under derived category name
- Use 3-7 categories based on actual content
- Category names must reflect actual system functionality

## README Structure

```markdown
# [Project Name]

## Overview
[2-3 sentences derived from aggregated functional requirements]

## Functional Requirements

### [Derived Category 1]
[FR-001]: [description]
[FR-002]: [description]

### [Derived Category 2]
[FR-003]: [description]

### [Derived Category N]
[FR-00N]: [description]

---

## Technical Specifications

### Dependencies

| Package | Version | Purpose |
|---------|---------|---------|

### Environment Configuration

| Variable | Description | Required | Default |
|----------|-------------|----------|---------|

### API Contracts

#### [Service Name]

| Method | Path | Request | Response | Description |
|--------|------|---------|----------|-------------|

### Data Models

#### [Model Name]

| Field | Type | Constraints |
|-------|------|-------------|

**Relationships:** [if any]

### Infrastructure

#### Compute
| Resource | Specification | Scale |
|----------|---------------|-------|

#### Storage
| Resource | Type | Capacity |
|----------|------|----------|

#### Networking
| Component | Configuration |
|-----------|---------------|

### Security
| Aspect | Implementation |
|--------|----------------|

### Observability
| Component | Purpose |
|-----------|---------|
```

## Aggregation Rules

- **DEPS:** Merge across files, keep highest version
- **CONFIG:** Dedupe by name, merge descriptions, infer defaults from code
- **API:** Group by service (derive from source path)
- **MODEL:** Merge fields from multiple sources
- **INFRA:** Consolidate by resource type

## Section Inclusion

Include section only if journal contains relevant entries:
- Skip empty sections entirely
- Skip Infrastructure if no INFRA entries
- Skip Security if no auth/encryption requirements found
- Skip Observability if no logging/monitoring requirements found

## Output

```
[GEN] README.md | N requirements | M specs
[DEL] journal.tmp
```
