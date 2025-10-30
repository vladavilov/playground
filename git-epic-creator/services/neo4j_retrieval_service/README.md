# DRIFT Search for Neo4j (Python + LLM)

This document describes how to implement **DRIFT Search** (Microsoft
Research, 2024) on top of a **Neo4j graph** enriched with embeddings. It
adapts Microsoft's DRIFT method to your schema and Python environment.

------------------------------------------------------------------------

## API Interface

### POST /retrieve

Retrieves context from Neo4j graph using DRIFT search algorithm.

**Request:**
```json
{
  "query": "string",
  "top_k": 1,
  "project_id": "string"
}
```

**Response (200 OK - Data Found):**
```json
{
  "final_answer": "string",
  "key_facts": [
    {
      "fact": "string",
      "citations": [
        {"chunk_id": "uuid", "span": "text excerpt", "document_name": "Document Title"},
        "chunk_id_fallback_string"
      ]
    }
  ],
  "residual_uncertainty": "string"
}
```

**Citation Structure:**
- Citations are **nested within each key_fact**, not provided as a top-level array
- Each key_fact contains a `citations` array that supports that specific fact
- **Citation Format (Enriched):** Most citations are objects with:
  - `chunk_id`: UUID string of the source chunk
  - `span`: Text excerpt from the chunk
  - `document_name`: Human-readable source document title
- **Citation Format (Fallback):** If enrichment lookup fails, citations may be plain chunk ID strings
- This structure preserves the semantic link between facts and their supporting evidence

**Response (200 OK - No Data Found):**
Returns 200 (not 500) when no data exists in graph for the query:
```json
{
  "final_answer": "",
  "key_facts": [],
  "residual_uncertainty": "",
  "no_data_found": true
}
```

**Response (500 Internal Server Error):**
Only returned for actual infrastructure/connection failures (Neo4j down, OpenAI API failure, etc.)

**Note:** The service distinguishes between "no data in graph" (200 with empty result) and "service failure" (500). This allows upstream services to handle empty graphs gracefully without treating them as errors.

### Real-time Progress Updates

The service publishes real-time progress updates via Redis pub/sub during retrieval operations:

**Channel:** `ui:retrieval_progress`

**Message Format:**
```json
{
  "message_type": "retrieval_progress",
  "project_id": "uuid",
  "retrieval_id": "uuid",
  "phase": "initializing|expanding_query|retrieving_communities|executing_followup|aggregating_results|completed|error",
  "progress_pct": 0-100,
  "thought_summary": "Human-readable status",
  "details_md": "Markdown details (optional)",
  "message_id": "uuid",
  "timestamp": "ISO8601"
}
```

**Progress Phases:**
- `initializing` (0%): Session initialization
- `expanding_query` (20%): HyDE query expansion
- `retrieving_communities` (40%): Community retrieval from graph
- `executing_followup` (40-80%): Processing follow-up questions (iterative)
- `aggregating_results` (90%): Synthesizing final answer
- `completed` (100%): Retrieval complete

These messages are consumed by `ui_service` and displayed in the agent thought panel for real-time user feedback.

**Citation Display in Progress Messages:**
- **Follow-up phase** (`executing_followup`): Citations shown with document name and text preview: `[document_name] "citation text excerpt"`
- **Aggregation phase** (`aggregating_results`): Citations shown as deduplicated document names: `doc1, doc2, doc3`
- **Completion phase** (`completed`): Citations shown as deduplicated document names in brackets: `[doc1], [doc2], [doc3]`
- **Deduplication:** Document names appearing multiple times in citations are automatically deduplicated to prevent repetitive display

------------------------------------------------------------------------

## 1. Graph Schema

Your graph:

    (:__Document__)-[:HAS_CHUNK]->(:__Chunk__)
    (:__Chunk__)-[:HAS_ENTITY]->(:__Entity__)
    (:__Entity__)-[:IN_COMMUNITY]->(:__Community__ {level:0})
    (:__Chunk__)-[:IN_COMMUNITY]->(:__Community__ {level:0})
    (:__Community__ {level:0})-[:IN_COMMUNITY]->(:__Community__ {level:1})
    (:__Community__ {level:N-1})-[:IN_COMMUNITY]->(:__Community__ {level:N})

**Relationship Semantics:**
- `HAS_CHUNK`: Document is split into text chunks
- `HAS_ENTITY`: Chunk contains/mentions an entity extracted by GraphRAG
- `IN_COMMUNITY`: Node belongs to a community in the Leiden hierarchy
- `RELATED`: Entity has semantic relationship with another entity (with LLM-generated description)
- `IN_PROJECT`: All nodes scoped to a project for multi-tenancy

**Community Hierarchy:**
- Direction: child → parent (level 0 → level 1 → level N)
- Level 0: Leaf communities (direct entity membership)
- Level N: Aggregate communities (contain lower-level communities)
- Higher-level communities summarize their children's content

**Properties:**
-   Communities: `summary` (text describing entities/subcommunities), `embedding` (3072 floats)
-   Chunks: `text` (original content), `embedding` (3072 floats)
-   Entities: `title`, `description`, `embedding` (3072 floats)

### Schema Design Notes

**Why `HAS_ENTITY` instead of `FROM_CHUNK`?**
- Semantic clarity: "Chunk HAS Entity" reads naturally
- Query direction: Most queries start from chunks and expand to entities
- Consistent with `HAS_CHUNK` pattern (Document → Chunk → Entity)

**Community Hierarchy Direction:**
- Child → Parent direction: `(level 0)-[:IN_COMMUNITY]->(level 1)`
- Rationale: Entities "belong to" communities (upward direction)
- Query pattern: Start with entities, traverse up to aggregate communities

This differs from some GraphRAG reference implementations but provides clearer semantics for our use case.

### Properties

-   `:__Chunk__.embedding :: List[Float]` (already present)
-   `:__Community__.embedding :: List[Float]`

### Indexes (Neo4j 5+)

The vector index for communities should be created **once**:
cypher CREATE VECTOR INDEX graphrag_comm_index IF NOT EXISTS FOR (c:__Community__) ON (c.embedding) OPTIONS {indexConfig: { vector.dimensions: 3072, vector.similarity_function: 'COSINE' }};
> **Note:** Community embeddings should be computed once from each
> community's `summary` (using the same embedding model as for chunks)
> and stored in `c.embedding`.

------------------------------------------------------------------------

## 2. Citation Handling

### Citation Structure
**Citations are nested within key_facts, not provided as a top-level array.** This design:
- Preserves the semantic link between facts and their supporting evidence
- Allows each fact to have its own specific set of citations
- Simplifies aggregation logic by keeping citations with their facts throughout the pipeline

**Citation Flow:**
1. **Local Executor**: Generates answers with rich `citations` array (chunk_id, span, document_name)
2. **Aggregation (LLM)**: LLM consolidates facts and returns chunk IDs as strings in `key_facts`
3. **Enrichment (Post-Processing)**: Maps chunk ID strings back to full citation objects from followup results
4. **Final Response**: Each `key_fact` contains `fact` string + `citations` array with full metadata

**Why Enrichment?** The LLM aggregator naturally returns chunk IDs as strings, but downstream services (ai_requirements_service, ai_tasks_service) need full citation metadata (document names, text previews). Post-processing enrichment bridges this gap by looking up chunk IDs in the followup results and replacing strings with rich citation objects.

### Document Name Retrieval
At the local executor level, citations include the source document name for each chunk reference. The retrieval query fetches:
```cypher
coalesce(d.title, d.id, 'unknown') AS document_name
```

This ensures:
- Primary: Use document's `title` property (set during ingestion)
- Fallback 1: Use document's `id` if title is empty
- Fallback 2: Use "unknown" if document is not found

**Note:** The ingestion service ensures `title` is always populated (from `title` field or `metadata.file_name` fallback), so "unknown" should only appear if the `HAS_CHUNK` relationship is missing.

### Citation Scope Design
Citations are built from the **full retrieved chunk set**, not just the chunks sent to the LLM prompt. This design choice:
- **Why:** Context window limits may require truncating chunks for LLM prompts (via `MAX_CHUNKS_FOR_PROMPT`)
- **Benefit:** If LLM correctly cites a chunk from the retrieval set (even if truncated from prompt), we still map it correctly
- **Risk:** If LLM hallucinates chunk IDs, they won't map (fallback to "unknown")
- **Mitigation:** Prompts instruct LLM to cite from provided context; validation logs when mappings fail

**Code Flow:**
1. Retrieve top-K chunks via vector search (`_scoped_chunks_expanded`)
2. Truncate for prompt (`_truncate_for_prompt` with `MAX_CHUNKS_FOR_PROMPT`)
3. LLM generates answer with citations
4. Map citations using **full retrieved set** (not truncated)

This is working as designed and provides better citation coverage than mapping only to truncated chunks.

### Citation Validation & Quality Enforcement

The service implements multi-layered citation validation to prevent "[unknown]" document names from appearing in downstream services:

**Layer 1: Prompt Engineering (Prevention)**
- `local_executor_prompt` explicitly lists valid chunk IDs in the prompt
- Instructs LLM: "You MUST use chunk_id values from the list below"
- Reduces hallucination rate by providing explicit constraints

**Layer 2: Model Validation (Detection)**
- `Citation` Pydantic model validates chunk_id at parse time
- Logs warnings when chunk_id is None or empty (indicates LLM output error)
- Normalizes whitespace and coerces types for consistency

**Layer 3: Post-Processing Validation (Filtering)**
- `_create_minimal_followup_result` filters invalid citations before aggregation
- Rejects citations where:
  - chunk_id is None or empty string
  - chunk_id not in the retrieved chunk set (hallucination)
- Logs detailed warnings with citation index, chunk_id, and span preview for debugging

**Logging & Observability:**
- `citation_model_null_chunk_id`: Pydantic validator detected None chunk_id
- `citation_model_empty_chunk_id`: Pydantic validator detected empty/whitespace chunk_id
- `citation_validation_null_chunk_id`: Post-processing filtered None chunk_id
- `citation_validation_unmatched_chunk_id`: Post-processing filtered hallucinated chunk_id
- `citation_validation_summary`: Aggregate stats (total, valid, filtered counts)

### Internal Response Models

The service uses Pydantic models to validate LLM responses at each pipeline stage:

**LocalExecutorResponse** (Follow-up stage):
```python
{
  "answer": str,
  "citations": [{"chunk_id": str, "span": str, "document_name": str}],
  "new_followups": [{"question": str}],
  "confidence": float,
  "should_continue": bool
}
```

**AggregatorResponse** (After LLM + Enrichment):
```python
{
  "final_answer": str,
  "key_facts": [
    {
      "fact": str, 
      "citations": [  # Mixed format after enrichment
        {"chunk_id": str, "span": str, "document_name": str},  # Enriched
        str  # Fallback if chunk ID not found in followup results
      ]
    }
  ],
  "residual_uncertainty": str
}
```

**Citation Enrichment Process:**
1. LLM aggregator returns chunk IDs as **strings** in `key_facts.citations`
2. Post-processing `_enrich_citations_from_followups()` looks up each chunk ID in `followup_results`
3. Found chunk IDs are replaced with full citation objects `{chunk_id, span, document_name}`
4. Not-found chunk IDs remain as strings (logged as warning)
5. Result contains **mixed format**: mostly enriched dicts, some strings if lookup fails

This ensures downstream services (ai_requirements_service, ai_tasks_service) receive full citation metadata without requiring the LLM to generate complex nested structures.

## 3. DRIFT Search Workflow

DRIFT combines **global primer search** with **local follow-ups** for
improved coverage.

### Phases

1.  **Primer**
    -   Expand query with HyDE (hypothetical document expansion).
    -   Embed query + HyDE.
    -   Retrieve top-K communities by embedding similarity **using hierarchical level filtering**:
        -   Query highest-level communities first (global aggregate summaries)
        -   Fall back to lower levels if insufficient results
        -   Ensures DRIFT starts with broad context before drilling down
    -   Gather 1--3 representative chunks per community.
    -   LLM produces:
        -   Initial coarse answer
        -   Follow-up questions with target communities.
2.  **Follow-ups**
    -   For each follow-up:
        -   Restrict scope to target communities.
        -   Retrieve top-N chunks (vector + optional BM25).
        -   Expand with nearest neighbors (nodes, relations, chunks).
        -   Build citation mapping: `chunk_id` → `document_name` from full retrieved set
        -   LLM produces:
            -   Intermediate answer (with citations referencing chunk_id)
            -   0--3 new follow-ups
            -   Confidence score
        -   Map LLM citations to include document names for traceability
    -   Iterate for 2 passes (default).
3.  **Final Aggregation**
    -   Collect Q/A tree (primer + follow-ups).
    -   Aggregate with LLM into:
        -   Final concise answer
        -   Key facts with citations
        -   Residual uncertainties.

------------------------------------------------------------------------

## 4. Primer Phase Details

### HyDE Expansion

**Prompt:**

    You are assisting a retrieval system. Write a short, factual paragraph that would likely appear in an ideal answer to this user question.

    Question: "{user_query}"
    Hypothetical answer paragraph:

### Hierarchical Community Retrieval

**Strategy:** DRIFT primer phase queries communities using **hierarchy-aware filtering** to start with aggregate context:

1. **Get max hierarchy level** for project: `MATCH (c:__Community__)-[:IN_PROJECT]->(:__Project__) RETURN max(c.level)`
2. **Query top-level first** (level N): Aggregate communities with broadest summaries
3. **Fall back to level N-1** if fewer than k/2 results found
4. **Fallback to all levels** if no hierarchy exists (level = 0 for all)

**Implementation:**
```python
max_level = repo.get_max_community_level(project_id)
rows = repo.vector_query_communities_by_level(
    index_name='graphrag_comm_index',
    k=5,
    qvec=query_vector,
    project_id=project_id,
    level=max_level  # Query highest level first
)
```

**Cypher (level-aware):**
```cypher
MATCH (p:__Project__ {id: $projectId})
MATCH (c:__Community__)-[:IN_PROJECT]->(p)
WHERE c.level = $level  // Filter by hierarchy level
WITH collect(c) AS candidates, p
CALL db.index.vector.queryNodes($name, $k * 2, $qvec)
YIELD node AS n, score
WHERE n IN candidates
RETURN n AS node, score
ORDER BY score DESC LIMIT $k
```

**Why hierarchical?**
- **Broader context first**: Level 2+ communities aggregate multiple lower-level communities
- **Efficient drilling**: Avoid leaf-level noise in initial phase
- **Better follow-ups**: LLM generates more targeted questions from high-level summaries
### Sample Chunks per Community
```cypher
MATCH (c:__Community__)-[:IN_PROJECT]->(p:__Project__ {id: $projectId})
WHERE c.community IN $communityIds 
CALL { 
  WITH c, $qvec AS qvec, p
  // Get chunks that belong to this community
  MATCH (c)<-[:IN_COMMUNITY]-(ch:__Chunk__)-[:IN_PROJECT]->(p)
  WITH ch, qvec 
  // Vector search within community's chunks
  CALL db.index.vector.queryNodes('chunk_idx', 50, qvec) 
  YIELD node AS cand, score 
  WHERE cand = ch 
  RETURN cand AS chunk, score 
  ORDER BY score DESC LIMIT 3 
} 
RETURN c, collect({chunk: chunk, score: score}) AS top_chunks;
```
### Primer Prompt

    You are DRIFT-Search Primer.
    Input: user question + community summaries + sample chunks.

    Tasks:
    - Draft initial answer (note uncertainty if needed).
    - Generate 2–6 follow-up questions with target communities.

    Return JSON: { initial_answer, followups:[{question, target_communities:[...] }], rationale }

    User question: {input here}, community details: {input here}, sample chunks: {input here}

------------------------------------------------------------------------

## 5. Follow-up Phase Details

### Scoped Retrieval

Scoped retrieval means we only search within the **chunks belonging to
specific target communities** (instead of the entire graph), which
improves efficiency and precision.

```cypher
MATCH (p:__Project__ {id: $projectId})
MATCH (c:__Community__)-[:IN_PROJECT]->(p)
WHERE c.community IN $cids AND c.project_id = $projectId
MATCH (c)<-[:IN_COMMUNITY]-(ch:__Chunk__)-[:IN_PROJECT]->(p)
WITH DISTINCT ch.id AS chunk_id, p
CALL db.index.vector.queryNodes('chunk_idx', 200, $qvec) 
YIELD node AS cand, score 
WHERE cand.id = chunk_id
RETURN chunk_id, score 
ORDER BY score DESC LIMIT 30;
```
### Neighborhood Expansion

For each selected chunk, fetch its **nearest neighbors** (1-hop nodes,
relations, and chunks). This ensures local context is added without
exploding the search space.

```cypher
UNWIND $chunkIds AS cid 
MATCH (p:__Project__ {id: $projectId})
MATCH (ch:__Chunk__)-[:IN_PROJECT]->(p) WHERE ch.id = cid 
// Get entities in this chunk
OPTIONAL MATCH (ch)-[:HAS_ENTITY]->(e:__Entity__)-[:IN_PROJECT]->(p)
// Get related entities and their relationships
OPTIONAL MATCH (e)-[r:RELATED]->(e2:__Entity__)-[:IN_PROJECT]->(p)
// Get neighboring chunks that contain related entities
OPTIONAL MATCH (ch2:__Chunk__)-[:HAS_ENTITY]->(e2)-[:IN_PROJECT]->(p)
WHERE ch2 <> ch
RETURN cid, 
       ch.text AS chunk_text, 
       collect(DISTINCT {id: e.id, title: e.title, description: e.description}) AS entities,
       collect(DISTINCT {id: e2.id, title: e2.title}) AS related_entities,
       collect(DISTINCT {type: type(r), description: r.description}) AS relationships,
       collect(DISTINCT ch2.id) AS neighbor_chunk_ids;
```
### Follow-up Prompt

    You are DRIFT-Search Local Executor.
    Input: follow-up question + retrieved chunks + graph neighborhoods.

    Tasks:
    - Answer follow-up using ONLY provided context.
    - Cite chunk IDs where evidence comes from.
    - Propose 0–3 additional follow-ups (if needed).
    - Assign confidence [0..1] and whether to continue.

    Return JSON:
    { answer, citations:[{chunk_id, span}], new_followups:[...], confidence, should_continue }

------------------------------------------------------------------------

## 6. Aggregation Phase

### Prompt

    You are DRIFT-Search Aggregator.
    User question: {question}
    Q/A tree (primer + follow-ups): {tree_json}

    Tasks:
    1. Produce final concise answer.
    2. List key facts with citations (chunk IDs as strings, nested within each fact).
    3. Note any residual uncertainty.

    Return JSON:
    { final_answer, key_facts:[{fact, citations:["chunk_id_1", "chunk_id_2"] }], residual_uncertainty }
    
    Note: Citations are chunk ID strings nested within each key_fact to preserve the semantic link
    between facts and their supporting evidence.

------------------------------------------------------------------------

## 7. Configuration

### Azure OpenAI Environment Variables (Required)

This service uses `AzureChatOpenAI` and `AzureOpenAIEmbeddings` connectors from LangChain, which require specific Azure OpenAI parameters:

```bash
# Azure OpenAI Configuration
OAI_BASE_URL=https://your-resource.openai.azure.com/     # Azure endpoint (no /openai suffix)
OAI_MODEL=gpt-4o                                         # Azure deployment name for chat (NOT model name)
OAI_KEY=your-azure-openai-key                            # Azure OpenAI API key
OAI_API_VERSION=2024-02-15-preview                       # Azure OpenAI API version

# Azure OpenAI Embeddings
OAI_EMBED_MODEL_NAME=text-embedding-3-small              # Embedding model name (for tiktoken)
OAI_EMBED_DEPLOYMENT_NAME=text-embedding-3-small         # Azure deployment name for embeddings

# LLM Parameters
OAI_TIMEOUT_SEC=10.0                                     # HTTP/LLM client timeout
LLM_TEMPERATURE=0.0                                      # Temperature for LLM requests (0.0-2.0)
```

**Important Notes:**
- `OAI_MODEL` is used as the `deployment_name` parameter in `AzureChatOpenAI`
- The endpoint path is constructed as: `{OAI_BASE_URL}/openai/deployments/{deployment_name}/chat/completions?api-version={OAI_API_VERSION}`
- Without proper `deployment_name`, requests will result in 404 errors

### Neo4j Configuration

```bash
NEO4J_URI=bolt://localhost:7687
NEO4J_USERNAME=neo4j
NEO4J_PASSWORD=neo4j123
NEO4J_DATABASE=neo4j
```

### Vector Index Configuration

```bash
COMMUNITY_VECTOR_INDEX_NAME=graphrag_comm_index
CHUNK_VECTOR_INDEX_NAME=graphrag_chunk_index
VECTOR_INDEX_DIMENSIONS=3072
VECTOR_INDEX_SIMILARITY=cosine
```

### Redis Configuration

Redis is required for publishing real-time progress updates to the UI:

```bash
REDIS_URL=redis://redis:6379          # Redis connection URL (required)
```

The service publishes progress messages to the `ui:retrieval_progress` channel. Redis client is initialized by `FastAPIFactory` with `enable_redis=True` in `main.py`.

### Retry Configuration

Rate limiting protection uses shared retry settings:

```bash
RETRY_MAX_ATTEMPTS=3                  # Max retry attempts for rate limits (default: 3)
RETRY_BACKOFF_BASE_SEC=2              # Base backoff in seconds (default: 2)
RETRY_BACKOFF_FACTOR=2                # Exponential backoff factor (default: 2)
RETRY_BACKOFF_MAX_SEC=60              # Max backoff ceiling (default: 60)
```

The service automatically retries HTTP 429 (rate limit) errors with exponential backoff + jitter.

------------------------------------------------------------------------