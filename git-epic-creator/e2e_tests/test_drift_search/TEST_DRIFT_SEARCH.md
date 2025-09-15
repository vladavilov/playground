# E2E Test Plan (TDD) — DRIFT Search for Neo4j (Python + LLM)

> This README describes the **tests-first** plan for validating **DRIFT Search** behavior against a Neo4j graph enriched with embeddings.
> We will leverage the existing Docker Compose stack, reuse the existing `openai_mock_service`, and the pre-seeded Cypher scripts in `cyphers.txt`. Where features are not yet implemented (e.g., DRIFT agent orchestration), tests will intentionally fail as part of TDD until the implementation is added.

---

## TL;DR (Adjusted for Existing Stack)

* We will:

  1. Start all services using Docker Compose (`e2e-tests` profile), including Neo4j and the already implemented `openai_mock_service`.
  2. Use the existing seed Cypher scripts at `e2e_tests/test_drift_search/infra/cyphers.txt` to populate the graph.
  3. Verify that required vector and full-text indexes already exist; do not create them if present.
  4. Do not implement an embedding service or DRIFT agent now; tests for the full DRIFT flow will initially fail by design (TDD) until the agent is implemented.
  5. Perform cleanup cautiously: if the environment does not support multi-database, skip creating/dropping the `drift_search_test` DB and operate on the default database instead.

---

Required env vars (example):

```
NEO4J_URI=bolt://localhost:7687
NEO4J_USER=neo4j
NEO4J_PASSWORD=pass
OAI_BASE_URL=http://openai-mock-service:8000   # provided by docker-compose for tests
```

---

## 1) Initialization Phase (tests reference existing infra)

### 1.1 Contract tests (skipped for now)

* The explicit README contract-parsing test is not required in this setup and can be omitted. Assumptions are documented here and enforced by subsequent integrity tests.

### 1.2 Database selection and compatibility

* Prefer a disposable test database named `drift_search_test` when the environment supports multi-database (`SHOW DATABASES`).
* If multi-database is NOT supported (e.g., community/single DB mode), operate on the default database and skip create/drop operations.
* Tests should detect support dynamically (e.g., via `supports_multi_db`) and adapt accordingly.

### 1.3 Seed data loading

* Use the existing cypher bundle `e2e_tests/test_drift_search/infra/cyphers.txt` to seed the graph.
* Integrity assertions remain the same (e.g., two chunks `{0,1}`, single `NEXT_CHUNK` 0→1, correct `FROM_CHUNK` scoping).

### 1.4 Embeddings

* No local embedding service is required. Embeddings are assumed to be prefilled by the seed scripts or prior processes.
* Tests should only verify integrity (e.g., vectors exist and have size 1536), not perform embedding writes.

### 1.5 Embedding integrity checks

* Tests: `test_01_embeddings::test_embeddings_written()` should assert:
  * `size(ch.embedding) = 1536` for all chunks.
  * `size(c.summary_embedding) = 1536` for communities that have `summary`.
* Do not attempt to compute or write embeddings during tests.

### 1.6 Indexes (verify-only)

* Do not create indexes in tests; only verify they exist and are usable.
* Expected indexes:
  * Vector: `graphrag_comm_index` on `c.summary_embedding` (1536, COSINE)
  * Vector: `chunk_idx` on `ch.embedding` (1536, COSINE)
  * Full-text: `community_summary_fts` on `(:Community).summary`
  * Full-text: `chunk_text_fts` on `(:Chunk).text`
* Test: `test_02_indexing::test_indexes_exist()` checks `SHOW INDEXES` and basic query invocations.

---

## 2) Test Implementation Phase (write before code)

### 2.1 Again, analyse `README.md` in `<placeholder>`

* **Test**: `test_03_primer::test_readme_controls_drirt_params()`

  * Parse README to assert configurable knobs are present/derived:

    * Primer top-K communities (`$k`)
    * Sample 1–3 chunks per community
    * Two passes of follow-ups (default)
  * These values should be **read** into the agent’s config, not hard-coded.

### 2.2 OpenAI mock service (reuse existing)

* Reuse the already implemented service at `services/openai_mock_service`.
* Tests should target `OAI_BASE_URL` (set to `http://openai-mock-service:8000` under docker-compose) and assume deterministic behavior for HyDE, Primer, Local Executor, and Aggregator roles.
* No new mock implementation is needed.

### 2.3 Assertions to implement (derived from the attached README)

#### A. Primer Phase

* **HyDE expansion happens**:

  * Assert that the agent calls OAI mock once with HyDE prompt and embeds the **HyDE** paragraph (not just the raw query).
* **Community retrieval via vector index**:

  * Assert Cypher call to:

    ```cypher
    CALL db.index.vector.queryNodes('graphrag_comm_index', $k, $qvec)
    ```
  * The test verifies `$k` matches config (from README), `$qvec` shape=1536.
* **Sample chunks per community**:

  * For each community in the primer result, the agent retrieves 1–3 chunks using **`chunk_idx`** filtering exactly as in README:

    ```cypher
    MATCH (c:Community) WHERE id(c) IN $communityIds
    CALL {
      WITH c, $qvec AS qvec
      MATCH (c)<-[:IN_COMMUNITY]-(:Node)-[:IN_CHUNK]->(ch:Chunk)
      WITH ch, qvec
      CALL db.index.vector.queryNodes('chunk_idx', 50, qvec) YIELD node AS cand, score
      WHERE cand = ch
      RETURN cand AS chunk, score
      ORDER BY score DESC LIMIT 3
    }
    ```
  * Assert **LIMIT ≤ 3** per community.
* **Primer output**:

  * OAI mock returns **initial\_answer** and **followups**; test asserts:

    * Each follow-up includes **target\_communities** (IDs or names resolvable to Neo4j IDs).

#### B. Follow-up Phase (2 passes)

* **Scoped retrieval**:

  * Assert that for each follow-up the agent restricts search to **only** chunks under the specified communities:

    ```cypher
    MATCH (c:Community) WHERE id(c) IN $cids
    MATCH (c)<-[:IN_COMMUNITY]-(:Node)-[:IN_CHUNK]->(ch:Chunk)
    ```
  * Then vector query against `chunk_idx` with `$qvec` (1536-d).
* **Neighborhood expansion**:

  * Assert that for each chosen chunk, the agent pulls 1-hop neighborhoods (nodes/relations/neighbor chunks) per README, not the entire graph.
* **Local executor output**:

  * JSON includes `citations` with **chunk IDs present in the retrieval set**.
  * `confidence` in `[0..1]` and `should_continue` aligns with the 2-pass setting.
* **Iteration control**:

  * Assert the agent stops after **2 passes** by default (or earlier if `should_continue=false`).

#### C. Final Aggregation

* **Aggregation prompt**:

  * Agent calls OAI mock with the tree JSON (primer + follow-ups).
* **Output shape**:

  * Assert JSON has:

    * `final_answer` (non-empty)
    * `key_facts` array where **each fact** has **citations** that are valid chunk IDs returned earlier
    * `residual_uncertainty` (string or list)
* **Determinism**:

  * With fixed mock, re-running test yields identical outputs.

#### D. Index & Embeddings Integrity

* **Vector indexes** exist and are usable\*\*:

  * `db.index.vector.queryNodes('graphrag_comm_index', 1, qvec)` returns rows (or empty set without error).
  * Same for `'chunk_idx'`.
* **Full-text indexes** exist\*\*:

  * `db.index.fulltext.queryNodes('community_summary_fts', 'bridge~1')` executes without error.
  * `db.index.fulltext.queryNodes('chunk_text_fts', 'arch~1')` executes without error.
* **Embeddings shape**:

  * `size(ch.embedding)=1536` for all chunks; `size(c.summary_embedding)=1536` for communities with `summary`.
* **Seed graph sanity**:

  * `(:Chunk {index:0})-[:NEXT_CHUNK]->(:Chunk {index:1})` exists exactly once.

> All the above asserts are **E2E**: start at Python agent entrypoint, end with Neo4j + mocks.

---

## 3) Implementation Notes (to satisfy the tests)

* **Embedding job**:

  * Use batched fetch → POST `/embed` → write-back with Cypher `UNWIND` as shown above.
  * Store as **lists of floats** on `ch.embedding` and `c.summary_embedding`.
* **neo4j-graphrag**:

  * Use the library’s **index management** to create/ensure vector and full-text indexes for:

    * `Chunk.text` (full-text), `Chunk.embedding` (vector)
    * `Community.summary` (full-text), `Community.summary_embedding` (vector)
  * If the library exposes helpers like `ensure_vector_index(...)` / `ensure_fulltext_index(...)`, call those in setup; otherwise fall back to the Cypher shown earlier.
* **Cypher execution**:

  * Use **parameterized** statements; never interpolate vectors as strings.
* **OAI mock**:

  * Distinguish roles via a request header (e.g., `X-LLM-Stage: hyde|primer|local|aggregate`) or prompt prefix. Return deterministic JSON.

---

## 4) Running the Tests (via docker-compose)

```
# Start full stack (including openai-mock-service and e2e-tests runner)
docker compose --profile e2e-tests up --build --remove-orphans

# The e2e-tests container will run pytest and exit. Test artifacts are mounted at:
# ./e2e_tests/test-results
```

---

## 5) Cleanup Phase (conditional)

### 5.1 Drop the database

* **Test**: `test_06_cleanup::test_drop_test_db()`
* **Behavior**:

  * If multi-database is supported: drop `drift_search_test` and assert removal from `SHOW DATABASES`.
  * If multi-database is NOT supported: skip this step and assert the default DB remains accessible.

---

## Appendix A — Minimal code stubs (non-executable sketches)

**`infra/embed_service.py`**

```python
# FastAPI stub returning deterministic 1536-d vectors
from fastapi import FastAPI
from pydantic import BaseModel
import numpy as np
import xxhash

app = FastAPI()

class EmbedReq(BaseModel):
    texts: list[str]
    model: str | None = "local-1536"

@app.post("/embed")
def embed(req: EmbedReq):
    vecs = []
    for t in req.texts:
        h = xxhash.xxh64(t).intdigest()
        rng = np.random.default_rng(h)
        v = rng.standard_normal(1536).astype(float)
        v = (v / np.linalg.norm(v)).tolist()
        vecs.append(v)
    return {"vectors": vecs}
```

**`infra/oai_mock.py`**

```python
from fastapi import FastAPI, Request
app = FastAPI()

@app.post("/chat/completions")
async def completions(req: Request):
    body = await req.json()
    prompt = str(body)
    if "HyDE" in prompt:
        return {"choices":[{"message":{"content":"Hypothetical paragraph about bridges and communities."}}]}
    if "DRIFT-Search Primer" in prompt:
        return {"choices":[{"message":{"content":'{"initial_answer":"Primer OK","followups":[{"question":"Q1","target_communities":[1]}],"rationale":"r"}'}}]}
    if "DRIFT-Search Local Executor" in prompt:
        return {"choices":[{"message":{"content":'{"answer":"Local OK","citations":[{"chunk_id":1,"span":"all"}],"new_followups":[],"confidence":0.9,"should_continue":false}'}}]}
    if "DRIFT-Search Aggregator" in prompt:
        return {"choices":[{"message":{"content":'{"final_answer":"Done","key_facts":[{"fact":"F1","citations":[1]}],"residual_uncertainty":""}'}}]}
    return {"choices":[{"message":{"content":"{}"}}]}
```

---

## Appendix B — What goes in `infra/init.cypher`

* The **entity + relationship** scripts you already generated in this chat for:

  * Chunk 0: bridge parts, history, mechanics, etc.
  * Chunk 1: types, classifications, force distributions, arch-bridge details.
* The **FROM\_CHUNK** connectors:

  * All entities for Chunk 0 → `FROM_CHUNK` to Chunk 0
  * **Only new entities** from the second script → `FROM_CHUNK` to Chunk 1
* The **NEXT\_CHUNK** edge:

  ```cypher
  CREATE CONSTRAINT chunk_index_unique IF NOT EXISTS
  FOR (c:Chunk) REQUIRE c.index IS UNIQUE;

  MATCH (c0:Chunk {index:0}), (c1:Chunk {index:1})
  MERGE (c0)-[:NEXT_CHUNK {description:'Chunk 1 follows chunk 0 in source order.'}]->(c1);
  ```

---

## Done

This plan keeps everything **test-driven**: we define the contract from the attached README, seed a controlled dataset, mock external services, assert each DRIFT phase behavior, and then drop the DB. Once these tests pass, you’ll have a reliable E2E harness for your Python agent.
