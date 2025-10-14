# OpenAI Mock Service

Local OpenAI-compatible API mock for GraphRAG and AI workflow testing.

## Quick Start

```powershell
docker-compose build openai-mock-service
docker-compose up openai-mock-service -d
```

Runs on `http://localhost:8010/v1`

**Important:** The service downloads the `jina-embeddings-v2-base-en` model during build time (controlled by `PRECACHE_MODELS` build arg). First build may take several minutes depending on network speed.

---

## Architecture

### Service Structure

```
src/
├── main.py                 # FastAPI app entry point
├── config.py               # Environment configuration
├── auth.py                 # Authentication logic
├── build_init.py          # Build-time model downloader
├── handlers/               # Prompt-specific response generators (23 handlers)
│   ├── base.py            # BaseHandler + HandlerRegistry
│   ├── drift_search.py    # 6 DRIFT-search handlers
│   ├── graphrag.py        # 3 GraphRAG handlers
│   ├── deepeval.py        # 4 DeepEval metric handlers
│   ├── workflow.py        # 4 AI workflow handlers
│   ├── search.py          # 5 search/summarization handlers
│   └── fallback.py        # 1 default graph generator
├── embeddings/
│   ├── loader.py          # Local Jina model loader
│   └── service.py         # EmbeddingService
└── routers/
    ├── health.py          # Health check
    ├── models.py          # Model listing
    ├── chat.py            # Chat completions (handler orchestration)
    ├── embeddings.py      # Embeddings generation
    └── azure.py           # Azure OpenAI wrappers
```

### Handler System

**Strategy Pattern:** Each handler implements `BaseHandler` with:
- `can_handle(messages, combined_text, lower_text) -> bool`
- `generate_response(messages, combined_text, model) -> str`

**Registry:** Handlers registered in priority order (most specific first). First matching handler processes request.

**23 Handlers (1-1 mapping guaranteed):**

| Group | Count | Handlers |
|-------|-------|----------|
| **DRIFT-search** | 6 | HyDE, Primer, LocalExecutor, Aggregator, SearchSystem, Reduce |
| **GraphRAG** | 3 | Extraction, CommunityReportGraph, CommunityReportText |
| **DeepEval** | 4 | Claims, Truths, GEval, Statements |
| **AI Workflow** | 4 | Analyst, Engineer, Auditor, Strategist |
| **Search/Summarization** | 5 | ExtractClaims, GlobalSearch, BasicSearch, QuestionGen, SummarizeDescriptions |
| **Fallback** | 1 | Default graph generator |

**Detection Examples:**
- `DriftHydeHandler`: `"hyde" in text OR "hypothetical answer paragraph"`
- `DriftPrimerHandler`: `"you are drift-search primer"`
- `AnalystHandler`: `"senior requirements analyst" + "intents" schema`
- `EngineerHandler`: `"requirements engineer" + BR/FR schema`

**No Overlaps:** Each handler has mutually exclusive detection pattern.

---

## API Endpoints

### Standard OpenAI API

| Method | Path | Router | Description |
|--------|------|--------|-------------|
| GET | `/health` | health | Health check (no auth) |
| GET | `/models` | models | List models |
| GET | `/v1/models` | models | List models (v1) |
| POST | `/chat/completions` | chat | Chat completions |
| POST | `/v1/chat/completions` | chat | Chat completions (v1) |
| POST | `/embeddings` | embeddings | Generate embeddings |
| POST | `/v1/embeddings` | embeddings | Generate embeddings (v1) |

### Azure OpenAI API

| Method | Path | Router | Description |
|--------|------|--------|-------------|
| GET | `/openai/deployments` | azure | List deployments |
| POST | `/openai/deployments/{deployment}/chat/completions` | azure | Chat (Azure) |
| POST | `/v1/openai/deployments/{deployment}/chat/completions` | azure | Chat (Azure v1) |
| POST | `/openai/deployments/{deployment}/embeddings` | azure | Embeddings (Azure) |
| POST | `/v1/openai/deployments/{deployment}/embeddings` | azure | Embeddings (Azure v1) |

**Azure Delegation:** Azure endpoints map `{deployment}` → `model` field, then delegate to standard endpoints. DRY: single `ensure_model_in_body()` helper.

---

## Request/Response Examples

### Chat Completions

**Request:**
```json
POST /v1/chat/completions
{
  "model": "gpt-4.1",
  "messages": [{"role": "user", "content": "Hello"}]
}
```

**Response:**
```json
{
  "id": "cmpl-mock-000",
  "object": "chat.completion",
  "created": 1700000000,
  "model": "gpt-4.1",
  "choices": [{
    "index": 0,
    "message": {"role": "assistant", "content": "<handler-specific-response>"},
    "finish_reason": "stop"
  }],
  "usage": {"prompt_tokens": 3, "completion_tokens": 2, "total_tokens": 5}
}
```

### Embeddings

**Request:**
```json
POST /v1/embeddings
{
  "model": "text-embedding-3-large",
  "input": ["text1", "text2"]
}
```

**Response:**
```json
{
  "object": "list",
  "data": [
    {"object": "embedding", "index": 0, "embedding": [0.01, 0.02, ...]},
    {"object": "embedding", "index": 1, "embedding": [0.03, 0.04, ...]}
  ],
  "model": "text-embedding-3-large",
  "usage": {"prompt_tokens": 0, "total_tokens": 0}
}
```

**Embeddings:** Local Jina model (`jina-embeddings-v2-base-en`), dimension fitting to target model.

---

## Authentication

**Required for all endpoints except `/health`**

**Methods:**
1. Bearer token: `Authorization: Bearer {OAI_KEY}`
2. API key header: `api-key: {OAI_KEY}` or `x-api-key: {OAI_KEY}`

**Configuration:** Set `OAI_KEY` env var. If not set, auth is disabled.

---

## Configuration

### Environment Variables

| Variable | Default | Description |
|----------|---------|-------------|
| `API_PORT` | `8000` | HTTP port |
| `OAI_KEY` | - | API key (optional, enables auth) |
| `OAI_MODEL` | `gpt-4.1` | Chat model name |
| `OAI_EMBED_MODEL` | `text-embedding-3-large` | Embeddings model name |
| `VECTOR_INDEX_DIMENSIONS` | `1536` | Embedding vector size |
| `HF_HOME` | `/app/models/hf-cache` | Hugging Face cache directory |
| `TRANSFORMERS_CACHE` | `/app/models/hf-cache` | Fallback for HF_HOME |
| `EMBED_MODEL_DIR` | `/app/models/embeddings/jina-embeddings-v2-base-en` | Local embeddings model path |

### Build Arguments

| Variable | Default | Description |
|----------|---------|-------------|
| `PRECACHE_MODELS` | `true` | Download embeddings model during build (set to `false` to skip) |

**Note:** Use `docker-compose.env` for consistent test environment.

---

## Error Responses

| Code | Cause |
|------|-------|
| 400 | Bad Request (missing `model`, `messages`, or `input`) |
| 401 | Unauthorized (invalid/missing API key) |
| 500 | Internal Server Error (embedding model failure) |

---

## Handler Detection Patterns (Key Examples)

### DRIFT-Search Workflow
1. **DriftHydeHandler**: `"hyde" OR "hypothetical answer paragraph"`
2. **DriftPrimerHandler**: `"you are drift-search primer"`
3. **DriftLocalExecutorHandler**: `"you are drift-search local executor"` (branches by followup)
4. **DriftAggregatorHandler**: `"you are drift-search aggregator"` (parses Q/A tree)
5. **DriftSearchSystemHandler**: `"Format your response in JSON" + "score between 0 and 100" + "follow_up_queries"`
6. **DriftReduceHandler**: `"---Data Reports---" + "data in the reports"`

### GraphRAG
7. **GraphRAGExtractionHandler**: `"entity_types:" + "text:" + "format each entity as"` OR continuation/loop prompts
8. **CommunityReportGraphHandler**: `"write a comprehensive report of a community" + "findings"` (no date_range)
9. **CommunityReportTextHandler**: `"date range" + "importance rating"` (with date_range)

### DeepEval Metrics
10. **DeepEvalClaimsHandler**: `"extract" + "claims"` OR `"list of facts"`
11. **DeepEvalTruthsHandler**: `"truths"` OR `"verify" + "claims"`
12. **DeepEvalGEvalHandler**: `"give a reason for the score"` OR `"score" + "reason"`
13. **DeepEvalStatementsHandler**: `"statements"` OR `"break down" + "response"`

### AI Workflow (Requirements Engineering)
14. **AnalystHandler**: System: `"senior requirements analyst" + "intents" schema`
15. **EngineerHandler**: System: `"requirements engineer"` OR `"business_requirements"` OR `"functional_requirements"`
16. **AuditorHandler**: System: `"senior requirements QA reviewer"` OR `"severity" + "suggestions"`
17. **StrategistHandler**: User: `"axis_scores"` OR `"questions"`

### Search & Summarization
18. **ExtractClaimsHandler**: `"-target activity-" + "extract all entities" + "Format each claim as ("`
19. **GlobalSearchHandler**: `"JSON formatted as follows" + "points" + "map"`
20. **BasicSearchHandler**: `"---Data tables---" + "data in the tables"` (different from DriftReduceHandler)
21. **QuestionGenerationHandler**: `"generate a bulleted list of" + "Use - marks as bullet points"`
22. **SummarizeDescriptionsHandler**: `"generating a comprehensive summary" + "Description List:"`

### Fallback
23. **FallbackGraphHandler**: Always matches (lowest priority). Returns Smallpdf-grounded graph.

**Disambiguation:** DriftReduceHandler uses "**reports**", BasicSearchHandler uses "**tables**".

---

## Deployment

**Docker:**
- Python 3.12+ slim-bookworm
- FastAPI + uvicorn
- Minimal dependencies (no Azure/Postgres/Redis)
- Listen: `0.0.0.0:${API_PORT}`
- Network: `git_epic_creator_network`
- Healthcheck: `GET /health`

**Image Optimization:**
- No shared service libraries (to minimize dependencies)
- Local Jina embeddings model (`jinaai/jina-embeddings-v2-base-en`) cached at build time using `build_init.py`
- Model saved with `save_pretrained()` to ensure proper Hugging Face format with `config.json` and all required files
- No persistence, metrics, or tracing

**Build Process:**
1. Install dependencies (shared + openai_mock_service)
2. Run `python -m build_init` (if `PRECACHE_MODELS=true`)
   - Downloads `jinaai/jina-embeddings-v2-base-en` from Hugging Face
   - Saves to `/app/models/embeddings/jina-embeddings-v2-base-en` using `save_pretrained()`
   - Verifies `config.json` exists
3. Runtime loads model from local directory (offline mode)

---

## Testing

**E2E Tests:**
- DRIFT-search workflow (HyDE → Primer → LocalExecutor → Aggregator)
- AI workflow (Analyst → Engineer → Auditor)
- DeepEval metrics validation
- GraphRAG entity extraction and community reports

**Test Files:**
- `e2e_tests/src/test_neo4j_drift_search.py`
- `e2e_tests/src/test_ui_workflow.py`
- `e2e_tests/src/test_document_workflow.py`

---

## Development Notes

### Behavior Rules
- All responses are deterministic (no LLM calls)
- Request content analyzed via pattern matching
- Responses are fixed JSON/text per handler
- No streaming support

### Non-Goals
- ❌ Persistence, metrics, tracing, rate limiting
- ❌ Retries/backoff (client-side concern)
- ❌ Unit tests (service exists only for E2E unblocking)

### Compatibility
- Satisfies fnllm OpenAI client (used by GraphRAG)
- Supports batch embeddings input
- No streaming (GraphRAG doesn't use it)

---

## References

- Docker network: `git_epic_creator_network` (defined in `docker-compose.yml`)
- Runs in Docker alongside other services, addressable by container name
