### Neo4j Retrieval Service — GraphRAG Context API (Local, Global, Drift Search)

This document defines the technical blueprint for the `neo4j_retrieval_service`, which provides read-only GraphRAG retrieval over a Neo4j knowledge graph populated by the ingestion pipeline. The service exposes a single, unified search endpoint that orchestrates:

- Hybrid local retrieval over entities (vector + keyword)
- Global retrieval via `__Community__` summaries (map-reduce style)
- Drift search expansion over `:RELATED` to gather adjacent, high-signal context

It reuses shared components for logging, app factory, and Neo4j connectivity, and mirrors the operational conventions of the existing `neo4j_ingestion_service` (Dockerfile, packaging, health checks) while remaining strictly read-only.

Reference for retrieval patterns and motivation: Integrating Microsoft GraphRAG Into Neo4j — Neo4j Developer Blog (`https://neo4j.com/blog/developer/microsoft-graphrag-neo4j/`).


## 1. Scope and Responsibilities

 - Read-only Neo4j retrieval, no write operations
 - Request in: user query and optional filters (e.g., `projectId`, limits)
 - Response out: structured context payload containing `entities`, `neighborEntities`, `documents`, and optional `finalAnswer`
 - Retrieval strategies implemented in a single orchestrated pipeline:
   - Hybrid local entity retriever (vector + keyword)
   - Global community summarization (map-reduce style)
   - Drift search expansion (graph expansion with drift budget and recency boosting)
 - Project-scoped queries where applicable


## 2. Architecture Overview

- FastAPI application, single process (no Celery)
- Reuse shared utilities:
  - `configuration.logging_config.configure_logging()`
  - `utils.app_factory.FastAPIFactory` for consistent CORS, Azure auth off by default
  - `utils.neo4j_client` for Neo4j driver lifecycle
- Health endpoint: `GET /health` returning `{ healthy: true }` if Neo4j connectivity succeeds
- Optional API auth at gateway (API Management) layer; service itself remains unauthenticated by default

Service adopts the operational conventions of `neo4j_ingestion_service`:

- Multi-stage Dockerfile (venv layer + runtime layer)
- `pyproject.toml` with `fastapi`, `uvicorn`, `neo4j`, `structlog`, `python-dotenv`, `pydantic-settings`, and `shared @ file:../shared`
- Environment-driven configuration


## 3. Neo4j Graph Model and Indexes

The ingestion (`importer.py`) populates a GraphRAG-shaped graph centered on:

- Nodes: `__Entity__`, `__Document__`, `__Community__`
- Relationships: `:RELATED` between entities (unique `id`)
- Vector index: `entity_embedding_index` on `__Entity__.embedding` (1536-d cosine)

Optional keyword/fulltext index for entity keywords:

```cypher
CALL db.index.fulltext.createNodeIndex(
  'ft_entities', ['__Entity__'], ['name','description','title']
) YIELD name;  // run once
```

Project scoping: if nodes include `projectId`, filter on it consistently.

Local (hybrid entity) search example:

```cypher
// Params: $k, $embedding, $query, $projectId, $limit
CALL {
  CALL db.index.vector.queryNodes('entity_embedding_index', $k, $embedding) YIELD node, score
  WHERE ($projectId IS NULL OR node.projectId = $projectId)
  RETURN node AS e, score
  UNION
  CALL db.index.fulltext.queryNodes('ft_entities', $query, {limit: $k}) YIELD node, score
  WHERE labels(node) = ['__Entity__'] AND ($projectId IS NULL OR node.projectId = $projectId)
  RETURN node AS e, score
}
WITH e, max(score) AS relevance
OPTIONAL MATCH (e)-[:RELATED]-(neighbor:__Entity__)
OPTIONAL MATCH (e)-->(d:__Document__)
RETURN e, relevance,
       coalesce(collect(DISTINCT neighbor), []) AS neighbor_entities,
       coalesce(collect(DISTINCT d), []) AS documents
ORDER BY relevance DESC
LIMIT toInteger($limit);
```

Drift search over `:RELATED` (entity graph):

```cypher
// Params: $seedEntityIds, $projectId, $maxHops, $maxNodes
MATCH (seed:__Entity__)
WHERE seed.id IN $seedEntityIds AND ($projectId IS NULL OR seed.projectId = $projectId)
CALL apoc.path.expandConfig(seed, {
  relationshipFilter: 'RELATED>',
  minLevel: 1,
  maxLevel: toInteger($maxHops),
  bfs: true,
  uniqueness: 'NODE_GLOBAL'
}) YIELD path
WITH nodes(path) AS ns
UNWIND ns AS n
WITH DISTINCT n
WHERE ($projectId IS NULL OR n.projectId = $projectId)
WITH n,
     coalesce(n.updatedAt, n.ingestedAt, datetime('1970-01-01')) AS t,
     size((n)--()) AS degree
WITH n,
     (duration.between(datetime('1970-01-01'), t).days / 365.0) AS recency_years,
     degree
WITH n,
     (0.7 * recency_years) + (0.3 * (1.0 / (degree + 1))) AS driftScore
RETURN n
ORDER BY driftScore DESC
LIMIT toInteger($maxNodes);
```

Global retriever communities:

```cypher
MATCH (c:__Community__)
WHERE c.level = $level AND ($projectId IS NULL OR c.projectId = $projectId)
RETURN c.full_content AS output;
```

Unified search responses should return `entities`, optional `neighborEntities`, and `documents`.


## 4. Unified Orchestrated Search

A single endpoint executes an orchestrated pipeline that combines local, global, and drift strategies into one cohesive result. High-level stages:

1) Query preparation
   - Normalize input, optional entity hints, projected `projectId`
   - Compute or accept query embedding

2) Local candidate retrieval (hybrid)
   - Vector search on `__Entity__.embedding` via `entity_embedding_index`
   - Optional keyword search via `ft_entities`
   - Merge and rank candidates using weighted fusion (e.g., `0.7 * vector + 0.3 * keyword`)

3) Drift expansion
   - Use top-N entities as seeds
   - Expand across `:RELATED` up to `maxHops`/`maxNodes`
   - Score nodes using recency boosts and degree penalties; keep best
   - Optionally attach directly connected `__Document__` nodes to selected entities

4) Global summarization
   - If `__Community__` nodes exist: fetch `full_content` at requested `level`
   - Map: run LLM per community content with question
   - Reduce: synthesize final answer from intermediate reports
   - Optionally condition map step on overlap with local/drift entities (e.g., keyword match) to limit cost

5) Result assembly
   - Return entities (seeds + drift), neighborEntities, documents, and optional `finalAnswer` from global step
   - Include scoring metadata and truncation indicators

Example hybrid search for entities (scoped by `projectId` if present):

```cypher
// Params: $projectId, $k, $embedding, $query
CALL {
  // Vector hits
  CALL db.index.vector.queryNodes('entity_embedding_index', $k, $embedding) YIELD node, score
  WHERE ($projectId IS NULL OR node.projectId = $projectId)
  RETURN node AS e, score, 'vector' AS source
  UNION
  // Keyword hits
  CALL db.index.fulltext.queryNodes('ft_entities', $query, {limit: $k}) YIELD node, score
  WHERE ($projectId IS NULL OR node.projectId = $projectId)
  RETURN node AS e, score, 'keyword' AS source
}
WITH e, max(score) AS relevance
OPTIONAL MATCH (e)-[:RELATED]-(neighbor:__Entity__)
OPTIONAL MATCH (e)-->(d:__Document__)
RETURN e,
       relevance,
       coalesce(collect(DISTINCT neighbor), []) AS neighbor_entities,
       coalesce(collect(DISTINCT d), []) AS documents
ORDER BY relevance DESC
LIMIT toInteger($limit);
```

 


Global communities fetch:

```cypher
MATCH (c:__Community__)
WHERE c.level = $level AND ($projectId IS NULL OR c.projectId = $projectId)
RETURN c.full_content AS output;
```


Drift expansion over `:RELATED` among entities:

- Controls: `maxHops` (default 2), `maxNodes` (cap), `driftBudget` (optional accumulated score threshold)
- Heuristics: recency boost (`updatedAt`/`ingestedAt`), degree penalty to avoid hubs

```cypher
// Params: $seedEntityIds, $projectId, $maxHops, $maxNodes
MATCH (seed:__Entity__)
WHERE seed.id IN $seedEntityIds AND ($projectId IS NULL OR seed.projectId = $projectId)
CALL apoc.path.expandConfig(seed, {
  relationshipFilter: 'RELATED>',
  minLevel: 1,
  maxLevel: toInteger($maxHops),
  bfs: true,
  uniqueness: 'NODE_GLOBAL'
}) YIELD path
WITH nodes(path) AS ns
UNWIND ns AS n
WITH DISTINCT n
WHERE ($projectId IS NULL OR n.projectId = $projectId)
// Optional: score by recency and degree
WITH n,
     coalesce(n.updatedAt, n.ingestedAt, datetime('1970-01-01')) AS t,
     size((n)--()) AS degree
WITH n,
     (duration.between(datetime('1970-01-01'), t).days / 365.0) AS recency_years,
     degree
WITH n,
     (0.7 * recency_years) + (0.3 * (1.0 / (degree + 1))) AS driftScore
RETURN n
ORDER BY driftScore DESC
LIMIT toInteger($maxNodes);
```

After selecting entity nodes, optionally fetch connected `__Document__` evidence:

```cypher
UNWIND $selectedEntityIds AS eid
MATCH (e:__Entity__ {id: eid})-[]->(d:__Document__)
RETURN e.id AS entityId, collect(DISTINCT d) AS documents;
```


## 5. API Specification

Base path: `/v1/retrieval`

- `GET /health` → `{ healthy: boolean }`

- `POST /search`
  - Request:
    ```json
    {
      "projectId": "<uuid>",
      "query": "Explain key entities around GraphRAG indexing and their links",
      "embedding": [/* optional float[1536] */],
      "local": { "k": 20, "weights": { "vector": 0.7, "keyword": 0.3 } },
      "drift": { "maxHops": 2, "maxNodes": 100 },
      "global": { "enabled": true, "level": 2, "limitCommunities": 50 }
    }
    ```
  - Response:
    ```json
    {
      "entities": [ {"id":"...","name":"...","score":0.82} ],
      "neighborEntities": [ {"id":"...","name":"..."} ],
      "documents": [ {"id":"...","title":"..."} ],
      "finalAnswer": "... optional synthesized answer ...",
      "meta": { "local": {"k":20}, "drift": {"expanded":87}, "global": {"level":2} }
    }
    ```

Notes:
- If `projectId` is omitted, results are not scoped (useful for admin/testing). In production, prefer scoping.
- The service does not perform mutations.


## 6. Configuration

Environment variables:
- `NEO4J_URI` (e.g., `bolt://neo4j:7687`)
- `NEO4J_USER`
- `NEO4J_PASSWORD`
- `ENTITY_VECTOR_INDEX` (default `entity_embedding_index`)
- `FT_INDEX_ENTITIES` (default `ft_entities`)
- `API_PORT` (default `8000`)

Optional embedding configuration:
- `EMBEDDING_PROVIDER` (e.g., `azure-openai`)
- `EMBEDDING_MODEL`
- `EMBEDDING_API_KEY` / endpoint settings


## 7. Packaging Blueprint

Mirror the ingestion service structure for consistency.

`Dockerfile` (multi-stage):
- Build venv, install `shared` then the retrieval service
- Copy `src/` at runtime image
- Expose `API_PORT`, add `/health` curl healthcheck
- Entrypoint: `python -m src.main`

`pyproject.toml` dependencies:
- `fastapi`, `uvicorn[standard]`, `neo4j`, `structlog`, `python-dotenv`, `pydantic-settings`, `shared @ file:../shared`
- Optional: `langchain` or `llama-index` if the service should run the global map-reduce internally; otherwise keep them out and let a caller orchestrate LLM steps.

`src/main.py` skeleton:
- Configure logging via `configure_logging()`
- Create FastAPI via `FastAPIFactory.create_app(..., enable_neo4j=True, enable_redis=False)`
- Add routers: `health`, `search`
- Use `utils.neo4j_client.get_neo4j_driver()` and short-lived sessions per request


## 8. Logging, Security, and SLOs

- Logging via shared `logging_config.py` with sensitive fields filtering and structlog JSON
- Health endpoint suitable for container orchestrator probes
- SLOs (targets): query latency P95 < 500ms for local retrieval (excluding LLM time), P95 < 2s under typical load when combining hybrid and moderate drift parameters
- No customer data; respect project scoping; read-only user


## 9. Example Cypher Snippets

Top-k entities by vector similarity and project scope:

```cypher
// Params: $embedding, $k, $projectId
CALL db.index.vector.queryNodes('entity_embedding_index', $k, $embedding) YIELD node, score
WHERE ($projectId IS NULL OR node.projectId = $projectId)
RETURN node AS e, score
ORDER BY score DESC
LIMIT toInteger($k);
```

Hybrid entity search (union vector + keyword):

```cypher
// Params: $projectId, $k, $embedding, $query
CALL {
  CALL db.index.vector.queryNodes('entity_embedding_index', $k, $embedding) YIELD node, score
  WHERE ($projectId IS NULL OR node.projectId = $projectId)
  RETURN node AS e, score
  UNION
  CALL db.index.fulltext.queryNodes('ft_entities', $query, {limit: $k}) YIELD node, score
  WHERE labels(node) = ['__Entity__'] AND ($projectId IS NULL OR node.projectId = $projectId)
  RETURN node AS e, score
}
WITH e, max(score) AS relevance
OPTIONAL MATCH (e)-[:RELATED]-(neighbor:__Entity__)
OPTIONAL MATCH (e)-->(d:__Document__)
RETURN e, coalesce(collect(DISTINCT neighbor), []) AS neighbor_entities, coalesce(collect(DISTINCT d), []) AS documents
ORDER BY relevance DESC
LIMIT toInteger($limit);
```


## 10. Build and Run

Local run (example):

```bash
docker build -t neo4j_retrieval_service:dev -f services/neo4j_retrieval_service/Dockerfile playground/git-epic-creator
docker run --rm -p 8000:8000 \
  -e NEO4J_URI=bolt://host.docker.internal:7687 \
  -e NEO4J_USER=neo4j -e NEO4J_PASSWORD=password \
  neo4j_retrieval_service:dev
```

API docs: `http://localhost:8000/docs`


## 11. Testing Strategy

- Unit tests for query builders and scoring combinators (pure functions)
- Integration tests (requires Neo4j container with test dataset): ensure local/global/drift endpoints return expected shapes and respect `projectId` scoping
- No shared constants imported into tests; prefer literal values for verification


## 12. Implementation Checklist

- [ ] Create service scaffold: Dockerfile, `pyproject.toml`, `src/main.py`, routers
- [ ] Wire `configure_logging`, `FastAPIFactory`, Neo4j driver
- [ ] Implement `/health`
- [ ] Implement `/v1/retrieval/search` unified orchestration (local + drift + optional global)
- [ ] Add environment-driven knobs (weights, limits) and sane defaults
- [ ] Write unit/integration tests
- [ ] Validate SLOs on representative data


## 13. Notes and References

- Neo4j Developer Blog on GraphRAG integration and retriever patterns: `https://neo4j.com/blog/developer/microsoft-graphrag-neo4j/`
- System Architecture and BRD documents under `ai_docs/` define the requirement-centric schema, project scoping, and vector index settings this service relies on.

