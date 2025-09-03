### OpenAI Mock Service — Local Startup

Use this mock locally to support the Neo4j ingestion service with an OpenAI-compatible API.

References:
- GraphRAG caller uses an OpenAI-compatible client via fnllm; ensure request/response compatibility with Chat Completions and Embeddings as used by this provider: [fnllm OpenAI providers `models.py`](https://raw.githubusercontent.com/microsoft/graphrag/refs/heads/main/graphrag/language_model/providers/fnllm/models.py).
- This service runs in the same Docker network as other services in `docker-compose.yml` and may be addressed by its container name.

### Quick start (Windows PowerShell)

```powershell
# From this directory
./start-local.ps1 -Port 8010 -Key KEY -Model gpt-4.1 -EmbedModel text-embedding-3-small
```

- The script creates a `.venv`, installs the service, sets `HF_HOME`, and runs on `http://localhost:8010/v1`.

### Scope
- Expose a tiny subset of OpenAI API:
  - GET `/v1/models`
  - POST `/v1/chat/completions`
  - POST `/v1/embeddings`
- Always return predefined hardcoded payloads. No streaming support.
- Optional static bearer auth if configured; otherwise allow all.

### API Contracts
- GET `/v1/models`
  - 200 OK (fixed): `{ object: "list", data: [{ id: <OAI_MODEL>, object: "model", owned_by: "mock" }, { id: <OAI_EMBED_MODEL>, object: "model", owned_by: "mock" }] }`.

- POST `/v1/chat/completions`
  - Request: accept `{ model, messages }` and ignore all other fields and values.
  - Response (fixed, non-stream only):
    - 200 OK:
      `{ id: "cmpl-mock-000", object: "chat.completion", created: 1700000000, model: <OAI_MODEL>, choices: [{ index: 0, message: { role: "assistant", content: "MOCK_RESPONSE" }, finish_reason: "stop" }], usage: { prompt_tokens: 3, completion_tokens: 2, total_tokens: 5 } }`.

- POST `/v1/embeddings`
  - Request: `{ model: string, input: string | string[] }` (content ignored).
  - Response (fixed): `{ object: "list", data: [{ object: "embedding", index: i, embedding: [0.001,0.002,0.003,0.004,0.005,0.006,0.007,0.008] } ...repeated per input item...], model: <OAI_EMBED_MODEL>, usage: { prompt_tokens: 0, total_tokens: 0 } }`.

### Authentication
- If `OAI_COMPLETION/EMBEDDINGS_KEY` is set, require header `Authorization: Bearer ${OAI_*_KEY}` for all `/v1/*` endpoints.
- Otherwise, skip auth. Health endpoint is always public.

### Health
- GET `/health` → `200 { status: "ok" }`.

### Configuration (env)
- `API_PORT` (default `8000`): HTTP port.
- `OAI_COMPLETION_KEY`: Optional bearer token to require.
- `OAI_EMBEDDINGS_KEY`: Optional bearer token to require.
- `OAI_MODEL` (default `gpt-4.1` from `docker-compose.env`): Used in responses.
- `OAI_EMBED_MODEL` (default `text-embedding-3-large`): Used in responses.
- `OAI_COMPLETION_URL`: Optional completion endpoint URL (for reference only).
- `OAI_EMBEDDINGS_URL`: Optional embeddings endpoint URL (for reference only).

Note: Use the shared env file `docker-compose.env` so values align with the test environment.

### Behavior Rules
- Ignore request content entirely; always return the fixed payloads above.
- Never call external services; all data is synthesized.
- Return JSON with OpenAI-like shapes; `created` is the fixed value `1700000000`.

### Errors
- 401 if auth required and missing/invalid.
- 400 for malformed JSON or missing required fields (`model`, `messages` for chat; `model`, `input` for embeddings).
- 404 for unknown paths; 405 for wrong methods.

### Deployment
- Containerized FastAPI/uvicorn app (Python 3.10+), minimal dependencies only.
- Listen on `0.0.0.0:${API_PORT}`; expose `${API_PORT}`.
- Join the `git_epic_creator_network` bridge network defined in `docker-compose.yml`.
- Healthcheck: GET `http://localhost:${API_PORT}/health` 200.

### Shared Library Reuse (decision)
- app_factory.py: Do not import. It pulls Azure/Auth/DB/Redis/Blob imports at module load, increasing dependencies. Implement a tiny FastAPI app inline with a single `/health` route.
- error_handler.py: Do not import. It depends on `neo4j` and registers broad handlers; unnecessary for this minimal mock. Use default FastAPI error handling and explicit 401/400 where needed.
- azure_token_provider.py: Not applicable. This service neither calls outbound services nor needs Azure tokens.
- Outcome: No shared dependencies; smallest possible image and startup surface.

### Non-Goals
- No persistence, metrics backend, tracing, or rate limiting.
- No retries/backoff logic; client-side concerns.
- No unit/integration tests; this service exists only to unblock E2E flows.

### Compatibility Notes
- Designed to satisfy fnllm OpenAI client expectations used by GraphRAG; embeddings batch input must be supported. No streaming.

### Minimal Example
Request to `/v1/chat/completions`:
```json
{ "model": "gpt-4.1", "messages": [{ "role": "user", "content": "Hello" }] }
```
Response (example):
```json
{ "id": "cmpl-mock-000", "object": "chat.completion", "created": 1700000000, "model": "gpt-4.1", "choices": [{ "index": 0, "message": { "role": "assistant", "content": "MOCK_RESPONSE" }, "finish_reason": "stop" }], "usage": { "prompt_tokens": 3, "completion_tokens": 2, "total_tokens": 5 } }
```

