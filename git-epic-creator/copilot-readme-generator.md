# README Generator

Generate `README.md` from `journal.tmp`.

## Execution

1. Read `journal.tmp`
2. Parse all entries by type: `[FR]`, `DEPS`, `CONFIG`, `API`, `MODEL`, `INFRA`
3. Deduplicate identical items
4. Group functional requirements by domain
5. Renumber: `[FR-001]` through `[FR-N]`
6. Aggregate technical specifications
7. Generate `README.md`
8. Delete `journal.tmp`

## Domain Categories

| Category | Assignment Keywords |
|----------|---------------------|
| Document Processing | PDF, text, extract, parse, ingest, file |
| Knowledge Graph | Neo4j, graph, node, cypher, relationship |
| Authentication | auth, token, JWT, OAuth, permission |
| API Gateway | endpoint, route, HTTP, REST |
| Data Storage | database, postgres, redis, cache |
| Integration | GitLab, webhook, external service |

## README Template

```markdown
# [Project Name]

## Functional Requirements

### Document Processing
[FR-001]: [description]

### Knowledge Graph
[FR-002]: [description]

### Authentication
[FR-003]: [description]

---

## Technical Specifications

### Runtime Dependencies

| Package | Version | Purpose |
|---------|---------|---------|
| fastapi | >=0.100.0 | REST API framework |
| neo4j | >=5.0.0 | Graph database driver |

### Environment Configuration

| Variable | Description | Required | Default |
|----------|-------------|----------|---------|
| NEO4J_URI | Graph database connection | Yes | - |
| CACHE_TTL | Redis cache duration | No | 300 |

### API Contracts

#### Service: [service_name]

| Method | Path | Request | Response | Description |
|--------|------|---------|----------|-------------|
| POST | /documents | `{file: binary}` | `{id: string}` | Upload document |
| GET | /documents/{id} | - | `{content: string}` | Retrieve content |

### Data Models

#### [ModelName]

| Field | Type | Constraints |
|-------|------|-------------|
| id | UUID | PK |
| name | string | NOT NULL |
| created_at | datetime | DEFAULT NOW |

**Relationships:**
- `ModelName` → `OtherModel` (FK: other_id)

### Infrastructure Requirements

#### Compute
| Resource | SKU/Size | Replicas |
|----------|----------|----------|
| AKS Node Pool | Standard_D4s_v3 | 3 |

#### Storage
| Resource | Type | Size |
|----------|------|------|
| PostgreSQL | Flexible Server | 128GB |
| Blob Storage | Hot | 500GB |

#### Networking
| Resource | Configuration |
|----------|---------------|
| VNet | 10.0.0.0/16 |
| Ingress | NGINX, TLS termination |
```

## Aggregation Rules

- **DEPS:** Merge across files, keep highest version constraint
- **CONFIG:** Dedupe by variable name, merge descriptions
- **API:** Group by service (derive from file path)
- **MODEL:** Merge field definitions from multiple sources
- **INFRA:** Consolidate into compute/storage/networking sections

## Output

```
[GEN] README.md | N requirements | M dependencies | K endpoints
[DEL] journal.tmp
```
