GitLab Client Service — Technical Requirements and API

## Overview

The `gitlab_client_service` provides a thin, reliable REST façade over GitLab. It normalizes responses and exposes purpose-built endpoints to:
- Fetch epics and issues for a project (with filters and pagination)
- Supply normalized data to `ai_tasks_service` for duplicate mapping/backlog generation
- Apply a reviewed backlog to GitLab with idempotent create/update and linking
 - Cache embeddings for titles and serve them via list endpoints

This service uses `python-gitlab` for all GitLab communication and follows shared conventions for configuration, logging, errors, and retries.

Reference: [python-gitlab v6.x](https://python-gitlab.readthedocs.io/en/stable/)

## Goals

- Provide stable, simple REST endpoints for UI and `ai_tasks_service`
- Normalize GitLab records to predictable models: `{ id, title, description, state, web_url, labels[] }`
- Support label/state filtering and pagination
- Safely apply a generated backlog with idempotency and partial-failure reporting

## Non-goals

- AI backlog generation (handled by `ai_tasks_service`)
- Full pass-through to all GitLab APIs (only the required subset with safe defaults)

## Architecture

- Framework: FastAPI via `shared.utils.app_factory.FastAPIFactory`
- GitLab client: `python-gitlab` configured via environment and shared config
- Resilience: timeouts, retries with exponential backoff, and rate-limit handling
- Observability: health endpoint, structured logging, request IDs, metrics hooks

### Local Components

Local to this microservice:
 - `GitLabSettings` (Pydantic): encapsulates all config/env vars
 - `GitLabClientFactory`: builds configured `python-gitlab` client
 - `IdempotencyStore`: simple in-memory/Redis-backed dedupe keyed by `idempotency_key`
 - `ErrorMapper`: maps python-gitlab exceptions to unified error schema
 - `RedisCacheClient`: Redis connection and namespacing for embeddings and pub/sub
 - `EmbeddingClient`: wrapper over embedding provider (e.g., OpenAI/Azure OpenAI/local)
 - `ProgressNotifier`: publishes Redis pub/sub messages for long-running tasks

### Shared components:
- `GitLabModels`: Pydantic models for normalized entities and list responses

## Configuration

Environment variables (defaults in parentheses):
- `GITLAB_BASE_URL` (required): e.g., `https://gitlab.example.com`
- `HTTP_TIMEOUT_SEC` (`30`)
- `HTTP_MAX_RETRIES` (`3`)
- `HTTP_RETRY_BACKOFF_FACTOR` (`2.0`)
- `DEFAULT_PAGE_SIZE` (`1000`)

Embeddings and cache (reuse from `docker-compose.env`; same names/values):
- `REDIS_URL`
- `REDIS_PASSWORD`
- `REDIS_DB`
- `REDIS_PUBSUB_CHANNEL_PREFIX`
- `OAI_BASE_URL`
- `OAI_KEY`
- `OAI_API_VERSION`
- `OAI_EMBED_MODEL`
- `OAI_EMBED_BATCH`
- `OAI_EMBED_CONCURRENCY`

The client uses a persistent session, smart retries, and rate-limit handling per the docs.

## Authentication & Authorization

Use this mechanism as the canonical flow.

### Mechanism for GitLab integration

Delegated user mode:
1) User signs in via Azure AD (Azure token is in session).
2) ui-service initiates GitLab OAuth Authorization Code flow (PKCE). With GitLab Azure SSO, the user typically isn’t prompted again.
3) ui-service receives GitLab `access_token` (+ `refresh_token`) with required scopes (`read_api` for read-only; `api` for writes).
4) ui-service stores tokens server-side keyed by the app’s user id (`oid`) and refreshes them proactively.
5) ui-service calls gitlab-client-service with GitLab access token using `GitLab-Access-Token: <gitlab_access_token>` and local jwt token using `Authorization: Bearer <local_jwt>`. gitlab-client-service will use the GitLab access token to call GitLab.
6) For intra-platform calls, the UI sends a short-lived local S2S JWT in `Authorization: Bearer <local_jwt>`.

All endpoints, including `POST /gitlab/projects/{project_id}/cache-embeddings`, use the same headers: `GitLab-Access-Token` for GitLab access, and `Authorization: Bearer <local_jwt>` for intra-platform auth.

## Data Models (Normalized)

Normalized models are shared; JSON field names must match exactly.

### GitLabWorkItem
```
{
  "kind": "epic" | "issue",
  "id": string,
  "title": string,
  "description": string,
  "state": "opened" | "closed",
  "labels": [string],
  "web_url": string,
  "title_embedding": number[] | null
}
```

### ListResponse<T>
```
{
  "items": T[],
  "pagination": {
    "page": number,
    "per_page": number,
    "next_page": number | null,
    "prev_page": number | null,
    "total": number | null
  }
}
```

### ApplyBacklogRequest
```
{
  "project_id": string,
  "prompt_id": string,
  "epics": [ { "id": string | null, "title": string, "description": string, "labels": [string] } ],
  "issues": [ { "id": string | null, "title": string, "description": string, "labels": [string], "epic_id": string | null } ]
}
```

### ApplyBacklogResponse
```
{
  "results": {
    "epics": [ { "input_index": number, "action": "created" | "updated" | "unchanged", "id": string, "web_url": string } ],
    "issues": [ { "input_index": number, "action": "created" | "updated" | "unchanged", "id": string, "web_url": string } ]
  },
  "errors": [ { "scope": "epic" | "issue", "input_index": number, "message": string, "gitlab_status?": number } ]
}
```

## REST APIs

Base path: `/gitlab`

### Projects
- `GET /gitlab/projects/{project_id}/backlog` → `ListResponse<GitLabWorkItem>`
  - Query: `labels`, `state=opened|closed|all` (default `opened`), `include_descendants` (default `true`), `page`, `per_page`
  - Note: Epics are group-level; resolve owning group. If epics disabled, return `items: []` and header `X-GitLab-Epics-Disabled: true`.
  - Returns both epics and issues
  - Query: `labels`, `state`, `search`, `page`, `per_page`
  - Embeddings: `title_embedding` is populated from Redis cache if present (no on-the-fly computation)

### Cache embeddings
- `POST /gitlab/projects/{project_id}/cache-embeddings`
  - Description: Precompute and cache title embeddings for all epics and issues in the project.
  - Auth: Same headers as other endpoints (`GitLab-Access-Token`, `Authorization`)
  - Behavior:
    - Retrieves all epics and issues for the project (fetches all pages)
    - Computes embeddings for each `title` using the configured `EmbeddingClient`
    - Stores vectors in Redis with keys `<project_id>:<work_item_id>`
    - Replaces any existing cache for the project on each request (previous cache is cleared before storing new values)
    - Publishes progress via Redis pub/sub on channel `${EMBEDDINGS_PUBSUB_PREFIX}<project_id>`
  - Pub/Sub message schema (examples):
    - `{ "event": "started", "project_id": "123" }`
    - `{ "event": "progress", "project_id": "123", "scanned": 150, "total": 320 }`
    - `{ "event": "embedded", "project_id": "123", "completed": 80, "total": 320 }`
    - `{ "event": "cached", "project_id": "123", "cached": 320 }`
    - `{ "event": "completed", "project_id": "123" }`
    - `{ "event": "error", "project_id": "123", "message": "..." }`

### Apply backlog
- `POST /gitlab/projects/{project_id}/apply-backlog` → `ApplyBacklogResponse`
  - Idempotent: identical payloads yield `unchanged` when no diffs
  - State: created/updated items are `opened`
  - Labels: unknown labels are ignored (may be reported as non-fatal warnings)
  - Linking: resolves created epic ids to link subsequent issues

### Health
- `GET /health` → liveness/readiness (process, GitLab reachability `/version`, Redis connectivity, embedding provider check)

## Integration Contracts

### ai_tasks_service → gitlab_client_service
- Uses `GET /gitlab/projects/{project_id}/backlog`
- Requires normalized fields: `{ kind, id, title, description, state, web_url, title_embedding }` (labels optional)

### UI Service → gitlab_client_service
- On Submit, calls `POST /gitlab/projects/{project_id}/apply-backlog` (optional `dry_run=true` for pre-validation)

## Implementation Notes

- Use `python-gitlab` with persistent sessions, retries, and rate-limit handling
- Use built-in pagination and surface pagination metadata
- Epics: `gl.groups.get(group_id).epics.list(...)` with filters; for nested projects, allow `include_descendants`
- Issues: `gl.projects.get(project_id).issues.list(...)` with filters (labels, state, search)
- Writes: create/update idempotently; if `id` present update diffs only; otherwise create
- Support `dry_run` to validate/plan without mutating GitLab
- Prefer server-side filtering; if client-side filtering is applied, set header `X-Client-Filtered: true`
- Embeddings cache:
  - Cache keys: `<project_id>:<work_item_id>`; values are numeric arrays (validated length if `EMBEDDING_DIM` provided)
  - `POST /gitlab/projects/{project_id}/cache-embeddings` clears and rebuilds cache for the project
  - List endpoints read `title_embedding` from cache only; they do not compute embeddings on demand

## Errors

Unified error response:
```
{
  "error": { "code": string, "message": string, "gitlab_status": number | null, "details": object | null }
}
```
Common codes: `invalid_request`, `unauthorized`, `forbidden`, `not_found`, `rate_limited`, `gitlab_unavailable`, `conflict`, `idempotency_key_required`, `cache_unavailable`, `embedding_unavailable`.

## Health, Logging, and Metrics

- `/health` returns ready only when dependencies are reachable
- Propagate/accept `X-Request-ID`
- Emit metrics: request count/duration by endpoint, GitLab error rates, retry counts

## Security & Compliance

- Store credentials only in env/secrets manager; never in repo
- Enforce input validation; avoid logging PII or sensitive descriptions
- Respect TLS/verification settings; never log tokens; mask headers

## Examples

### Fetch issues
Request:
```
GET /gitlab/projects/123/issues?labels=backend,security&state=opened&page=1&per_page=50
```
Response:
```
{
  "items": [
    { "kind": "issue", "id": "1345", "iid": "17", "title": "Fix JWT validation", "description": "...", "state": "opened", "labels": ["backend","security"], "web_url": "https://gitlab/..." }
  ],
  "pagination": { "page": 1, "per_page": 50, "next_page": 2, "prev_page": null, "total": null }
}
```

### Apply backlog
### Cache embeddings
Request:
```
POST /gitlab/projects/123/cache-embeddings
Headers:
  Authorization: Bearer <local_jwt>
  GitLab-Access-Token: <gitlab_access_token>
```
Pub/Sub channel: `embeddings:projects:123`
Example message:
```
{ "event": "completed", "project_id": "123" }
```
Request:
```
POST /gitlab/projects/123/apply-backlog
{
  "project_id": "123",
  "prompt_id": "...",
  "epics": [ { "id": null, "title": "Security Hardening", "description": "...", "labels": ["security"] } ],
  "issues": [ { "id": null, "title": "Rotate service keys", "description": "...", "labels": ["security"], "epic_id": null } ]
}
```
Response:
```
{
  "results": {
    "epics": [ { "input_index": 0, "action": "created", "id": "(pending)", "web_url": "(pending)" } ],
    "issues": [ { "input_index": 0, "action": "created", "id": "(pending)", "web_url": "(pending)" } ]
  },
  "errors": []
}
```

## Implementation Notes for Engineers

- Normalize GitLab objects to the schemas above; avoid leaking raw fields
- Resolve group for epics via the project’s namespace; prefer nearest ancestor with epics enabled; allow `include_descendants`
- Idempotent writes: no-op when no changes; otherwise update diffs only

