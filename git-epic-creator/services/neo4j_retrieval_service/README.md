# DRIFT Search for Neo4j (Python + LLM)

This document describes how to implement **DRIFT Search** (Microsoft
Research, 2024) on top of a **Neo4j graph** enriched with embeddings. It
adapts Microsoft's DRIFT method to your schema and Python environment.

------------------------------------------------------------------------

## 1. Graph Schema

Your graph:

    (:__Chunk__)<-[:FROM_CHUNK]-(:Node)
    (:__Community__ {level:0})<-[:IN_COMMUNITY]-(:Node)
    (:__Community__ {level:1})<-[:IN_COMMUNITY]-(:__Community__ {level:0})
    (:__Community__ {level:N})<-[:IN_COMMUNITY]-(:__Community__ {level:N-1})

-   Communities have `summary` (text) describing contained nodes or
    subcommunities.
-   Nodes/Chunks hold the original content.
-   Higher-level communities summarize their children.

### Properties

-   `:__Chunk__.embedding :: List[Float]` (already present)
-   `:__Community__.embedding :: List[Float]`

### Indexes (Neo4j 5+)

The vector index for communities should be created **once**:
cypher CREATE VECTOR INDEX graphrag_comm_index IF NOT EXISTS FOR (c:__Community__) ON (c.embedding) OPTIONS {indexConfig: { vector.dimensions: 1536, vector.similarity_function: 'COSINE' }};
> **Note:** Community embeddings should be computed once from each
> community's `summary` (using the same embedding model as for chunks)
> and stored in `c.embedding`.

------------------------------------------------------------------------

## 2. DRIFT Search Workflow

DRIFT combines **global primer search** with **local follow-ups** for
improved coverage.

### Phases

1.  **Primer**
    -   Expand query with HyDE (hypothetical document expansion).
    -   Embed query + HyDE.
    -   Retrieve top-K communities by embedding similarity.
    -   Gather 1--3 representative chunks per community.
    -   LLM produces:
        -   Initial coarse answer
        -   Follow-up questions with target communities.
2.  **Follow-ups**
    -   For each follow-up:
        -   Restrict scope to target communities.
        -   Retrieve top-N chunks (vector + optional BM25).
        -   Expand with nearest neighbors (nodes, relations, chunks).
        -   LLM produces:
            -   Intermediate answer (with citations)
            -   0--3 new follow-ups
            -   Confidence score
    -   Iterate for 2 passes (default).
3.  **Final Aggregation**
    -   Collect Q/A tree (primer + follow-ups).
    -   Aggregate with LLM into:
        -   Final concise answer
        -   Key facts with citations
        -   Residual uncertainties.

------------------------------------------------------------------------

## 3. Primer Phase Details

### HyDE Expansion

**Prompt:**

    You are assisting a retrieval system. Write a short, factual paragraph that would likely appear in an ideal answer to this user question.

    Question: "{user_query}"
    Hypothetical answer paragraph:

### Community Retrieval
cypher CALL db.index.vector.queryNodes('graphrag_comm_index', $k, $qvec) YIELD node, score RETURN node, score ORDER BY score DESC;
### Sample Chunks per Community
cypher MATCH (c:__Community__) WHERE id(c) IN $communityIds CALL { WITH c, $qvec AS qvec MATCH (c)<-[:IN_COMMUNITY]-(:Node)-[:FROM_CHUNK]->(ch:__Chunk__) WITH ch, qvec CALL db.index.vector.queryNodes('chunk_idx', 50, qvec) YIELD node AS cand, score WHERE cand = ch RETURN cand AS chunk, score ORDER BY score DESC LIMIT 3 } RETURN c, collect({chunk: chunk, score: score}) AS top_chunks;
### Primer Prompt

    You are DRIFT-Search Primer.
    Input: user question + community summaries + sample chunks.

    Tasks:
    - Draft initial answer (note uncertainty if needed).
    - Generate 2–6 follow-up questions with target communities.

    Return JSON: { initial_answer, followups:[{question, target_communities:[...] }], rationale }

    User question: {input here}, community details: {input here}, sample chunks: {input here}

------------------------------------------------------------------------

## 4. Follow-up Phase Details

### Scoped Retrieval

Scoped retrieval means we only search within the **chunks belonging to
specific target communities** (instead of the entire graph), which
improves efficiency and precision.
cypher WITH $qvec AS qvec, $cids AS cids MATCH (c:__Community__) WHERE id(c) IN cids MATCH (c)<-[:IN_COMMUNITY]-(:Node)-[:FROM_CHUNK]->(ch:__Chunk__) WITH DISTINCT ch, qvec CALL db.index.vector.queryNodes('chunk_idx', 200, qvec) YIELD node AS cand, score WHERE cand = ch RETURN cand AS chunk, score ORDER BY score DESC LIMIT 30;
### Neighborhood Expansion

For each selected chunk, fetch its **nearest neighbors** (1-hop nodes,
relations, and chunks). This ensures local context is added without
exploding the search space.
cypher UNWIND $chunkIds AS cid MATCH (ch:__Chunk__) WHERE id(ch)=cid OPTIONAL MATCH (n)-[:FROM_CHUNK]->(ch) OPTIONAL MATCH (n)-[r]->(m)-[:FROM_CHUNK]->(ch2) RETURN cid, ch.text AS chunk_text, collect(DISTINCT {id:id(n), label:labels(n)}) AS nodes1, collect(DISTINCT {id:id(m), label:labels(m)}) AS nodes2, collect(DISTINCT {type:type(r)}) AS rels, collect(DISTINCT id(ch2)) AS neighbor_chunk_ids;
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

## 5. Aggregation Phase

### Prompt

    You are DRIFT-Search Aggregator.
    User question: {question}
    Q/A tree (primer + follow-ups): {tree_json}

    Tasks:
    1. Produce final concise answer.
    2. List key facts with citations (chunk IDs).
    3. Note any residual uncertainty.

    Return JSON:
    { final_answer, key_facts:[{fact, citations:[...] }], residual_uncertainty }

------------------------------------------------------------------------

## 6. Configuration

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
VECTOR_INDEX_DIMENSIONS=1536
VECTOR_INDEX_SIMILARITY=cosine
```

------------------------------------------------------------------------