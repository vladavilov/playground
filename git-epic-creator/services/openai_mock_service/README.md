# OpenAI Mock Service

Local OpenAI-compatible API mock for GraphRAG and AI workflow testing.

## Quick Start

```powershell
docker-compose build openai-mock-service
docker-compose up openai-mock-service -d
```

Runs on `http://localhost:8010/v1`

Note: Embeddings are mocked deterministically; no model download is required.

---

## Architecture

### Service Structure

```
src/
├── main.py                 # FastAPI app entry point
├── auth.py                 # Authentication logic
├── handlers/               # Prompt-specific response generators (23 handlers)
│   ├── base.py            # BaseHandler + HandlerRegistry
│   ├── drift_search.py    # 6 DRIFT-search handlers
│   ├── graphrag.py        # 3 GraphRAG handlers
│   ├── deepeval.py        # 4 DeepEval metric handlers
│   ├── workflow.py        # 4 AI workflow handlers
│   ├── search.py          # 5 search/summarization handlers
│   └── fallback.py        # 1 default graph generator
├── embeddings/
│   └── service.py         # EmbeddingService (deterministic mock vectors)
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

**28 Handlers (1-1 mapping guaranteed):**

| Group | Count | Handlers |
|-------|-------|----------|
| **DRIFT-search** | 6 | HyDE, Primer, LocalExecutor, Aggregator, SearchSystem, Reduce |
| **GraphRAG** | 3 | Extraction, CommunityReportGraph, CommunityReportText |
| **DeepEval** | 7 | Claims, Truths, FaithfulnessReason, GEval, Statements, AnswerRelevancyScore, Verdicts |
| **AI Tasks Service** | 5 | RequirementsAnalyst, BacklogEngineer, ConsistencyAuditor, Evaluator, ClarificationStrategist |
| **AI Workflow** | 4 | Analyst, Engineer, Auditor, Strategist |
| **Search/Summarization** | 5 | ExtractClaims, GlobalSearch, BasicSearch, QuestionGen, SummarizeDescriptions |
| **Fallback** | 1 | Default graph generator |

**Detection Examples:**
- `DriftHydeHandler`: `"hyde" in text OR "hypothetical answer paragraph"`
- `DriftPrimerHandler`: `"you are drift-search primer"`
- `TasksRequirementsAnalystHandler`: `"senior technical architect analyzing requirements" + "intents/entities/constraints"`
- `TasksConsistencyAuditorHandler`: `"quality assurance lead" + "conducting a rigorous audit"`
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

**Embeddings:** Deterministic pseudo-random vectors sized to `VECTOR_INDEX_DIMENSIONS`.

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
| `API_PORT` | `8000` | HTTP port (from shared `AppSettings.API_PORT`) |
| `OAI_KEY` | - | API key (optional, enables auth) (from shared `LlmConfig.OAI_KEY`) |
| `OAI_MODEL` | `gpt-4o-mini` | Chat model name (from shared `LlmConfig.OAI_MODEL`) |
| `OAI_EMBED_MODEL_NAME` | `text-embedding-3-small` | Embeddings model name (from shared `LlmConfig.OAI_EMBED_MODEL_NAME`) |
| `OAI_EMBED_DEPLOYMENT_NAME` | - | Azure deployment name for embeddings (from shared `LlmConfig.OAI_EMBED_DEPLOYMENT_NAME`) |
| `VECTOR_INDEX_DIMENSIONS` | `3072` | Embedding vector dimensions (default: 3072, configurable via shared `VectorIndexEnv.VECTOR_INDEX_DIMENSIONS`) |

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

### AI Tasks Service (Backlog Generation)
14. **TasksRequirementsAnalystHandler**: System: `"senior technical architect analyzing requirements" + "intents" + "entities" + "constraints"`
15. **TasksBacklogEngineerHandler**: System: `"senior technical lead" + "agile expert"` + User: `"### Requirements" + "### Technical Context"`
16. **TasksConsistencyAuditorHandler**: System: `"quality assurance lead" + "conducting a rigorous audit"` + User: `"### Requirements" + "### Backlog"`
17. **TasksEvaluatorHandler**: System: `"backlog evaluator"` + User: `"### Requirements" + "### Backlog"` (supports OLD & NEW formats)
18. **TasksClarificationStrategistHandler**: System: `"clarification strategist" + "below target"` + User: `"### Requirements" + "### Current Scores" + "Target:"`

### AI Workflow (Requirements Engineering)
19. **AnalystHandler**: System: `"senior requirements analyst" + "intents" schema`
20. **EngineerHandler**: System: `"requirements engineer"` OR `"business_requirements"` OR `"functional_requirements"`
21. **AuditorHandler**: System: `"senior requirements QA reviewer"` OR `"severity" + "suggestions"`
22. **StrategistHandler**: User: `"axis_scores"` OR `"questions"`

### Search & Summarization
23. **ExtractClaimsHandler**: `"-target activity-" + "extract all entities" + "Format each claim as ("`
24. **GlobalSearchHandler**: `"JSON formatted as follows" + "points" + "map"`
25. **BasicSearchHandler**: `"---Data tables---" + "data in the tables"` (different from DriftReduceHandler)
26. **QuestionGenerationHandler**: `"generate a bulleted list of" + "Use - marks as bullet points"`
27. **SummarizeDescriptionsHandler**: `"generating a comprehensive summary" + "Description List:"`

### Fallback
28. **FallbackGraphHandler**: Always matches (lowest priority). Returns Smallpdf-grounded graph.

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

**Image:** Minimal; no model downloads; no persistence, metrics, or tracing


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
