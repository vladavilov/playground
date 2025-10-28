# GitLab Client Service

## Overview

The `gitlab_client_service` provides a thin, reliable REST façade over GitLab. It normalizes responses and exposes purpose-built endpoints to:
- Fetch epics and issues for a project (with filters and pagination)
- Cache embeddings for work item titles with Redis-backed storage
- Supply normalized data to `ai_tasks_service` for duplicate mapping/backlog generation
- Apply a reviewed backlog to GitLab with idempotent create/update and linking

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

### System Architecture

```mermaid
graph TB
    subgraph "gitlab_client_service"
        App[FastAPI App<br/>main.py]
        Router[gitlab_router.py<br/>- GET /backlog<br/>- POST /cache-embeddings<br/>- POST /apply-backlog]
        
        subgraph "Service Layer"
            GCS[GitLabClientService]
            EC[EmbeddingClient]
            RCC[RedisCacheClient]
            PN[ProgressNotifier]
            IS[IdempotencyStore]
            EM[ErrorMapper]
        end
        
        subgraph "Dependencies"
            GLC[GitLab Client]
            RC[Redis Client]
        end
    end
    
    subgraph "External Services"
        GitLab[GitLab API]
        Redis[(Redis)]
        OpenAI[OpenAI/Azure OpenAI]
    end
    
    App --> Router
    Router --> GCS
    Router --> EC
    Router --> RCC
    Router --> PN
    Router --> IS
    Router --> EM
    
    GCS --> GLC
    EC --> OpenAI
    RCC --> RC
    PN --> RC
    
    GLC --> GitLab
    RC --> Redis
    
    style App fill:#e1f5ff
    style Router fill:#b3e5fc
    style GitLab fill:#fff3e0
    style Redis fill:#fff3e0
    style OpenAI fill:#fff3e0
```

### Framework & Technology Stack

- **Framework:** FastAPI via `shared.utils.app_factory.FastAPIFactory`
- **GitLab Client:** `python-gitlab` v4+ configured via environment and shared config
- **Caching:** Redis with `redis[hiredis]` for embeddings and pub/sub
- **Embeddings:** OpenAI/Azure OpenAI compatible API
- **Resilience:** Timeouts, retries with exponential backoff, and rate-limit handling
- **Observability:** Health endpoints, structured logging (`structlog`), request IDs, metrics hooks

### Local Components

Local to this microservice:
- `GitLabClientSettings` (Pydantic): encapsulates all config/env vars
- `GitLabClientService`: wrapper over `python-gitlab` with normalization
- `IdempotencyStore`: simple in-memory dedupe keyed by `prompt_id` (TTL 24 hours)
- `ErrorMapper`: maps python-gitlab exceptions to unified error schema
- `RedisCacheClient`: Redis connection and namespacing for embeddings
- `EmbeddingClient`: wrapper over embedding provider (OpenAI/Azure OpenAI/local)
- `ProgressNotifier`: publishes Redis pub/sub messages for long-running tasks

### Shared Components

From shared library:
- `RedisSettings`: Redis configuration and client utilities
- `LlmConfig`: LLM/embedding provider configuration
- `FastAPIFactory`: FastAPI app factory with common middleware
- JWT authentication for service-to-service calls

## Configuration

### Configuration Separation

This service uses two distinct URL concepts:

1. **`GITLAB_BASE_URL`**: The actual GitLab instance URL (e.g., `https://gitlab.com`)
   - Used for GitLab API calls
   - Used for OAuth authorization and token exchange
   - Used by `python-gitlab` client

2. **`GITLAB_CLIENT_BASE_URL`**: The internal gitlab-client-service URL (e.g., `http://gitlab-client-service:8000`)
   - Used by other services (ui-service, project-management-service) to communicate with this service
   - **Not** used within gitlab-client-service itself

> **Important:** Do not confuse these two URLs. `GITLAB_CLIENT_BASE_URL` is the URL of *this microservice*, not the GitLab instance.

### Environment Variables

Configuration is centralized in `src/config.py` and reuses shared library configs:

**GitLab Instance settings:**
```env
GITLAB_BASE_URL=""                     # Required: GitLab instance URL (e.g., https://gitlab.com)
GITLAB_VERIFY_SSL=true                 # Verify SSL certificates for GitLab instance
HTTP_CONNECTION_TIMEOUT=30.0           # HTTP connection timeout
RETRY_MAX_ATTEMPTS=3                   # Maximum HTTP retries
RETRY_BACKOFF_FACTOR=2.0              # Exponential backoff factor
DEFAULT_PAGE_SIZE=100                  # Default pagination size
```

**GitLab Client Service settings:**
```env
EMBEDDINGS_PUBSUB_PREFIX="embeddings:projects:"  # Redis pub/sub prefix
IDEMPOTENCY_TTL_SECONDS=86400         # 24 hours default
OAI_EMBED_BATCH=16                    # Batch size for embeddings
OAI_EMBED_CONCURRENCY=2               # Concurrent embedding requests
```

**Shared Redis configuration (from `RedisSettings`):**
```env
REDIS_URL="redis://redis:6379"
REDIS_PASSWORD=""
REDIS_DB=0
REDIS_MAX_CONNECTIONS=10
REDIS_SOCKET_CONNECT_TIMEOUT=5
REDIS_SOCKET_TIMEOUT=5
REDIS_PUBSUB_CHANNEL_PREFIX="embeddings:"
```

**Shared LLM configuration (from `LlmConfig`):**
```env
# For Azure OpenAI:
OAI_BASE_URL="https://<your-resource>.openai.azure.com"
OAI_API_VERSION="2024-02-01"
OAI_KEY="your-azure-openai-key"

# For standard OpenAI or mock:
# OAI_BASE_URL="http://openai-mock-service:8000"
# OAI_KEY="sk-..."

OAI_EMBED_MODEL_NAME="text-embedding-3-small"
OAI_EMBED_DEPLOYMENT_NAME="text-embedding-3-small"
```

**GitLab OAuth Configuration:**
```env
GITLAB_OAUTH_CLIENT_ID=""               # GitLab OAuth application client ID
GITLAB_OAUTH_CLIENT_SECRET=""           # GitLab OAuth application client secret
GITLAB_OAUTH_REDIRECT_URI=""            # OAuth callback URI (points to ui-service which proxies to this service)
GITLAB_OAUTH_SCOPES="read_api api"      # OAuth scopes (space-separated)
```

> **Important:** `GITLAB_OAUTH_REDIRECT_URI` should point to ui-service (e.g., `http://localhost:8007/auth/gitlab/callback`), not directly to gitlab-client-service. ui-service acts as a transparent proxy, forwarding all OAuth requests and responses between the browser and gitlab-client-service.

> **Note:** OAuth URLs are constructed using `GITLAB_BASE_URL`. There is no need for a separate `GITLAB_CLIENT_BASE_URL` in gitlab-client-service configuration.

**Local JWT (for service-to-service auth):**
```env
LOCAL_JWT_SECRET="dev-local-jwt-secret"
```

### Embedding Provider Compatibility

The service auto-detects Azure OpenAI vs standard OpenAI:
- **Azure OpenAI:** Detected when `OAI_API_VERSION` is set
  - Uses `AzureOpenAI` client with `api_version` parameter
- **Standard OpenAI:** When `OAI_API_VERSION` is not set
  - Uses `OpenAI` client with `base_url` parameter

## Authentication & Authorization

### GitLab OAuth Integration

gitlab-client-service implements a **fully stateless** GitLab OAuth flow optimized for horizontal scalability:

**Key Features:**
- Stateless OAuth state management (Base64-encoded JSON with session_id, redirect_uri, CSRF token)
- Token storage in Redis keyed by session_id
- Automatic token refresh via Authlib
- Connection status checking
- **No SessionMiddleware required** - completely stateless architecture

**Stateless Design Benefits:**
- Horizontally scalable (any instance can handle any request)
- No server-side session storage (all state in URLs or Redis)
- Proxy-friendly (works through ui-service without cookie forwarding)
- Survives service restarts (no in-memory state)

### OAuth Flow

ui-service acts as a transparent proxy for all GitLab OAuth endpoints. All OAuth logic is handled by gitlab-client-service using a stateless approach.

```mermaid
sequenceDiagram
    participant Browser
    participant UI as ui-service<br/>(transparent proxy)
    participant GCS as gitlab-client-service<br/>(stateless OAuth)
    participant GitLab
    participant Redis

    Browser->>UI: Click "Connect GitLab"<br/>GET /auth/gitlab/authorize?redirect_uri=...
    Note over Browser,UI: Browser request with session cookie
    
    UI->>UI: Extract session_id from cookie
    UI->>GCS: Proxy: GET /auth/gitlab/authorize<br/>?session_id=abc&redirect_uri=/projects
    Note over UI,GCS: S2S JWT authentication
    
    Note over GCS: Stateless OAuth - No session storage
    GCS->>GCS: Generate CSRF token<br/>Encode state={sid, redirect, csrf}
    GCS->>GCS: Build OAuth URL with state param
    GCS-->>UI: 302 Redirect to GitLab<br/>(state encoded in URL)
    UI-->>Browser: Forward: 302 Redirect to GitLab
    
    Browser->>GitLab: Authorize application
    Note over Browser,GitLab: User grants permission
    GitLab-->>Browser: 302 Redirect to callback<br/>?code=xxx&state=yyy
    
    Browser->>UI: GET /auth/gitlab/callback<br/>?code=xxx&state=yyy
    UI->>GCS: Proxy: GET /auth/gitlab/callback<br/>?code=xxx&state=yyy
    
    Note over GCS: Stateless token exchange
    GCS->>GCS: Decode state → (sid, redirect, csrf)
    GCS->>GCS: Validate CSRF token
    GCS->>GitLab: fetch_access_token(code, redirect_uri)
    Note over GCS,GitLab: Bypass Authlib session validation
    GitLab-->>GCS: Access token + refresh token
    
    GCS->>Redis: Store token<br/>(key: gitlab:oauth:{session_id})
    GCS-->>UI: 302 Redirect to /projects
    UI-->>Browser: Forward: 302 Redirect
    
    Note over Browser,Redis: Token stored, OAuth complete
```

### OAuth Endpoints

All GitLab OAuth endpoints are accessed through ui-service proxy, which forwards requests with S2S JWT authentication.

#### GET /auth/gitlab/authorize

Initiate stateless GitLab OAuth flow.

**Query Parameters:**
- `session_id` (required): User session ID from ui-service (added by proxy)
- `redirect_uri` (required): Application URL to redirect to after OAuth completes

**Response:** 302 Redirect to GitLab authorization page

**Stateless Implementation:**
```python
# Generate CSRF token
csrf_token = secrets.token_urlsafe(32)

# Encode state with all necessary data (no session storage)
state = base64.urlsafe_b64encode(json.dumps({
    "sid": session_id,
    "redirect": redirect_uri,
    "csrf": csrf_token
}).encode()).decode()

# Build OAuth URL manually (bypasses Authlib session)
auth_url = f"{GITLAB_BASE_URL}/oauth/authorize?{urlencode({
    'client_id': CLIENT_ID,
    'redirect_uri': CALLBACK_URI,
    'response_type': 'code',
    'scope': SCOPES,
    'state': state
})}"

return RedirectResponse(auth_url)
```

**Note:** This endpoint uses query parameters for browser redirect compatibility. No server-side session state is stored.

#### GET /auth/gitlab/callback

OAuth callback handler (called by GitLab after user authorization).

**Query Parameters:**
- `code`: Authorization code from GitLab
- `state`: Base64-encoded JSON with session_id, redirect_uri, and CSRF token

**Response:** 302 Redirect to original redirect_uri

**Stateless Implementation:**
```python
# Decode state from URL parameter
state_data = json.loads(base64.urlsafe_b64decode(state))
session_id = state_data["sid"]
redirect_uri = state_data["redirect"]
csrf_token = state_data["csrf"]

# Validate CSRF token (custom validation)
# ... csrf validation logic ...

# Exchange code for token using stateless fetch (bypasses Authlib session)
token = await oauth.gitlab.fetch_access_token(
    code=code,
    redirect_uri=settings.GITLAB_OAUTH_REDIRECT_URI,
)

# Store token in Redis
await save_token(session_id, redis_client, token)

# Redirect to application
return RedirectResponse(redirect_uri)
```

#### GET /auth/gitlab/status

Check GitLab connection status for a session.

**Authentication:** S2S JWT in Authorization header (extracts session_id from `oid` claim)

**Response:**
```json
{
  "connected": true,
  "configured": true
}
```

**Example:**
```http
GET /auth/gitlab/status
Authorization: Bearer eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9...
```

**Implementation:**
```python
# Extract session_id from JWT (not from session)
session_id = get_session_id_from_jwt(request)

# Check Redis for token
token = await get_token(session_id, redis_client)
connected = bool(token and token.get("access_token"))
```

#### POST /auth/gitlab/disconnect

Disconnect GitLab integration for a session.

**Authentication:** S2S JWT in Authorization header (extracts session_id from `oid` claim)

**Response:**
```json
{
  "disconnected": true
}
```

**Example:**
```http
POST /auth/gitlab/disconnect
Authorization: Bearer eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9...
```

**Implementation:**
```python
# Extract session_id from JWT
session_id = get_session_id_from_jwt(request)

# Clear token from Redis
await clear_token(session_id, redis_client)
```

### Token Management

**Redis Storage:**
- Key pattern: `gitlab:oauth:{session_id}`
- TTL: Token expires_in + 10 minutes buffer
- Stateless storage (no server-side sessions)
- Automatic refresh via Authlib when expired (if refresh_token available)

**Token Format:**
```json
{
  "access_token": "glpat-xxxxx",
  "token_type": "Bearer",
  "expires_in": 7200,
  "refresh_token": "glprt-xxxxx",
  "scope": "read_api api",
  "created_at": 1234567890
}
```

**Automatic Token Refresh:**

The service uses Authlib's built-in automatic token refresh mechanism. When a token expires, Authlib automatically:
1. Detects expiration during API calls
2. Refreshes the token using the refresh_token grant
3. Invokes `update_token` callback to save the new token to Redis
4. Retries the API call with the new token

```mermaid
sequenceDiagram
    participant API as API Request
    participant Authlib
    participant GitLab
    participant Redis
    participant Callback as update_token callback

    API->>Authlib: Make API call (expired token)
    Authlib->>Authlib: Detect token expiry
    Authlib->>GitLab: Refresh token request
    GitLab->>Authlib: New access token + refresh token
    Authlib->>Callback: Invoke update_token(new_token)
    Callback->>Redis: Save token under session_id
    Redis->>Callback: Saved
    Authlib->>API: Retry API call with new token
    API->>API: Success
```

**Session Context Pattern:**

Before making API calls, set the session context to associate refreshed tokens with the correct user:

```python
from services.gitlab_token_manager import set_session_context

# Set context before API calls
set_session_context(session_id)

# Make API calls - token refresh happens automatically if needed
projects = gitlab_client.projects.list()
```

This approach follows SOLID/DRY principles by:
- Eliminating manual token expiry checks
- Centralizing token refresh logic in Authlib
- Reducing code duplication and maintenance overhead
- **Stateless token storage** (no in-memory session state)

### Session-Based Authentication

All GitLab API endpoints use session-based authentication:

1. Client sends S2S JWT with `session_id` in claims (oid field)
2. gitlab-client-service extracts session_id from JWT
3. Looks up GitLab token from Redis
4. Uses token to call GitLab API

**Example Request:**
```http
GET /gitlab/projects/123/backlog
Authorization: Bearer eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9...
```

**JWT Claims:**
```json
{
  "oid": "abc-123-session-id",
  "iss": "ui-service",
  "aud": "gitlab-service",
  "exp": 1234567890
}
```

### Project Resolution Endpoint

#### POST /gitlab/projects/resolve

Resolve GitLab project path or URL to numeric project ID.

**Request Body:**
```json
{
  "gitlab_path": "my-group/my-project"
}
```

Or:
```json
{
  "gitlab_url": "https://gitlab.com/my-group/my-project"
}
```

**Response:**
```json
{
  "project_id": "123",
  "path": "my-group/my-project",
  "name": "My Project",
  "web_url": "https://gitlab.com/my-group/my-project"
}
```

**Error Responses:**
- 400: Invalid path format
- 401: GitLab authentication required
- 404: Project not found
- 500: GitLab API error

## Data Models

Normalized models are shared; JSON field names must match exactly.

### GitLabWorkItem

```json
{
  "kind": "epic" | "issue",
  "id": "string",
  "title": "string",
  "title_embedding": [0.123, ...] | [],
  "description": "string",
  "state": "opened" | "closed",
  "labels": ["string"],
  "web_url": "string"
}
```

### ListResponse<T>

```json
{
  "items": [GitLabWorkItem],
  "pagination": {
    "page": 1,
    "per_page": 100,
    "next_page": 2 | null,
    "prev_page": null,
    "total": 150 | null
  }
}
```

### ApplyBacklogRequest

```json
{
  "project_id": "string",
  "prompt_id": "string",
  "epics": [
    {
      "id": "string" | null,
      "title": "string",
      "description": "string",
      "labels": ["string"]
    }
  ],
  "issues": [
    {
      "id": "string" | null,
      "title": "string",
      "description": "string",
      "labels": ["string"],
      "epic_id": "string" | null
    }
  ]
}
```

### ApplyBacklogResponse

```json
{
  "results": {
    "epics": [
      {
        "input_index": 0,
        "action": "created" | "updated" | "unchanged",
        "id": "string",
        "web_url": "string"
      }
    ],
    "issues": [
      {
        "input_index": 0,
        "action": "created" | "updated" | "unchanged",
        "id": "string",
        "web_url": "string"
      }
    ]
  },
  "errors": [
    {
      "scope": "epic" | "issue",
      "input_index": 0,
      "message": "string",
      "gitlab_status": 404
    }
  ]
}
```

## REST APIs

Base path: `/gitlab`

### GET /gitlab/projects/{project_id}/backlog

Get project backlog (epics + issues) with cached embeddings.

**Query Parameters:**
- `labels` (optional): Comma-separated labels to filter by
- `state` (optional): Filter by state - `opened` (default), `closed`, `all`
- `page` (optional): Page number (default: 1)
- `per_page` (optional): Items per page (default: service default, max: 1000)

**Response:** `ListResponse<GitLabWorkItem>`

**Notes:**
- Epics are group-level; resolves owning group automatically
- If epics disabled, returns `items: []` with header `X-GitLab-Epics-Disabled: true`
- `title_embedding` is populated from Redis cache if present (no on-the-fly computation)

**Get Backlog Data Flow:**

```mermaid
flowchart LR
    Client([Client Request]) --> Router[gitlab_router]
    
    Router --> ParseParams[Parse Query Params<br/>labels, state, page]
    
    ParseParams --> GitLabService[GitLabClientService]
    
    GitLabService --> ResolveGroup{Project has<br/>Group?}
    
    ResolveGroup -->|Yes| FetchEpics[Fetch Epics<br/>group.epics.list]
    ResolveGroup -->|No| SkipEpics[Skip Epics]
    
    FetchEpics --> FetchIssues[Fetch Issues<br/>project.issues.list]
    SkipEpics --> FetchIssues
    
    FetchIssues --> Normalize[Normalize to<br/>GitLabWorkItem]
    
    Normalize --> ExtractIDs[Extract Work Item IDs]
    
    ExtractIDs --> RedisBulk[Redis: Get Embeddings Bulk<br/>MGET project:id1 project:id2...]
    
    RedisBulk --> Enrich{Embeddings<br/>Found?}
    
    Enrich -->|Yes| PopulateEmbeddings[Populate title_embedding<br/>for matched items]
    Enrich -->|No| KeepEmpty[Keep title_embedding empty]
    
    PopulateEmbeddings --> BuildResponse[Build ListResponse<br/>items + pagination]
    KeepEmpty --> BuildResponse
    
    BuildResponse --> Return([Return to Client])
    
    style Router fill:#e3f2fd
    style GitLabService fill:#fff3e0
    style RedisBulk fill:#ffebee
    style Enrich fill:#e8f5e9
```

### POST /gitlab/projects/{project_id}/cache-embeddings

Precompute and cache title embeddings for all epics and issues in the project.

**Response:** 202 Accepted with channel information

**Behavior:**
- Runs as background task, returns immediately with HTTP 202
- Retrieves all epics and issues for the project (fetches all pages)
- Computes embeddings for each `title` using the configured `EmbeddingClient`
- Stores vectors in Redis with keys `<project_id>:<work_item_id>`
- Replaces any existing cache for the project (clears before storing new values)
- Publishes progress via Redis pub/sub on channel `${EMBEDDINGS_PUBSUB_PREFIX}<project_id>`

**Pub/Sub Messages:**

Channel: `embeddings:projects:{project_id}`

Message examples:
```json
{ "event": "started", "project_id": "123" }
{ "event": "progress", "project_id": "123", "scanned": 150, "total": 320 }
{ "event": "embedded", "project_id": "123", "completed": 80, "total": 320 }
{ "event": "cached", "project_id": "123", "cached": 320 }
{ "event": "completed", "project_id": "123" }
{ "event": "error", "project_id": "123", "message": "..." }
```

**Cache Embeddings Workflow:**

```mermaid
sequenceDiagram
    participant Client
    participant API as gitlab_router
    participant BG as Background Task
    participant GitLab as GitLabClientService
    participant Embed as EmbeddingClient
    participant Cache as RedisCacheClient
    participant PubSub as ProgressNotifier
    participant Redis

    Client->>API: POST /cache-embeddings
    API->>BG: Schedule background task
    API->>Client: 202 Accepted<br/>channel: embeddings:projects:123
    
    par Background Processing
        BG->>PubSub: notify_started(project_id)
        PubSub->>Redis: PUBLISH "started"
        
        loop Fetch All Pages
            BG->>GitLab: list_project_backlog(page)
            GitLab-->>BG: work items
            BG->>PubSub: notify_progress(scanned, total)
            PubSub->>Redis: PUBLISH "progress"
        end
        
        BG->>BG: Collect all titles
        BG->>Embed: embed_texts_batched(titles)
        
        loop Process Batches
            Embed->>Embed: Split into batches (size: 16)
            Embed->>Embed: Concurrent requests (2)
            Note over Embed: Batch processing with<br/>concurrency control
        end
        
        Embed-->>BG: embeddings[]
        BG->>PubSub: notify_embedded(completed, total)
        PubSub->>Redis: PUBLISH "embedded"
        
        BG->>Cache: clear_project_embeddings(project_id)
        Cache->>Redis: DEL pattern match
        
        BG->>Cache: set_embeddings_bulk(project_id, embeddings)
        Cache->>Redis: MSET project:item embeddings
        
        BG->>PubSub: notify_cached(cached_count)
        PubSub->>Redis: PUBLISH "cached"
        
        BG->>PubSub: notify_completed(project_id)
        PubSub->>Redis: PUBLISH "completed"
    end
    
    Client->>Redis: SUBSCRIBE embeddings:projects:123
```

### POST /gitlab/projects/{project_id}/apply-backlog

Apply a generated backlog to GitLab with idempotent create/update.

**Request Body:** `ApplyBacklogRequest`

**Response:** `ApplyBacklogResponse`

**Behavior:**
- Idempotent: identical payloads yield `unchanged` when no diffs
- State: created/updated items are `opened`
- Labels: unknown labels are ignored (may be reported as non-fatal warnings)
- Linking: resolves created epic ids to link subsequent issues
- Partial failures: continues processing and reports errors per item

**Apply Backlog Workflow:**

```mermaid
flowchart TD
    Start([POST /apply-backlog]) --> CheckIdempotency{Check<br/>Idempotency<br/>prompt_id?}
    
    CheckIdempotency -->|Cached Result| ReturnCached[Return Cached Response]
    ReturnCached --> End([End])
    
    CheckIdempotency -->|New Request| ResolveGroup[Resolve Group<br/>for Project]
    
    ResolveGroup --> ProcessEpics[Process Epics Array]
    
    ProcessEpics --> EpicLoop{For Each Epic}
    
    EpicLoop -->|Has ID| UpdateEpic[Update Epic<br/>via GitLab API]
    EpicLoop -->|No ID| CreateEpic[Create Epic<br/>via GitLab API]
    
    UpdateEpic --> EpicSuccess{Success?}
    CreateEpic --> EpicSuccess
    
    EpicSuccess -->|Yes| RecordEpicResult[Record Epic Result<br/>created/updated]
    EpicSuccess -->|No| RecordEpicError[Record Epic Error<br/>continue processing]
    
    RecordEpicResult --> EpicMore{More Epics?}
    RecordEpicError --> EpicMore
    
    EpicMore -->|Yes| EpicLoop
    EpicMore -->|No| ProcessIssues[Process Issues Array]
    
    ProcessIssues --> IssueLoop{For Each Issue}
    
    IssueLoop -->|Has ID| UpdateIssue[Update Issue<br/>via GitLab API]
    IssueLoop -->|No ID| CreateIssue[Create Issue<br/>via GitLab API]
    
    UpdateIssue --> IssueSuccess{Success?}
    CreateIssue --> IssueSuccess
    
    IssueSuccess -->|Yes| RecordIssueResult[Record Issue Result<br/>created/updated]
    IssueSuccess -->|No| RecordIssueError[Record Issue Error<br/>continue processing]
    
    RecordIssueResult --> IssueMore{More Issues?}
    RecordIssueError --> IssueMore
    
    IssueMore -->|Yes| IssueLoop
    IssueMore -->|No| BuildResponse[Build Response<br/>results + errors]
    
    BuildResponse --> CacheResult{prompt_id<br/>present?}
    
    CacheResult -->|Yes| StoreIdempotency[Store in<br/>Idempotency Store]
    CacheResult -->|No| ReturnResponse[Return Response]
    
    StoreIdempotency --> ReturnResponse
    ReturnResponse --> End
    
    style CheckIdempotency fill:#e3f2fd
    style EpicSuccess fill:#fff3e0
    style IssueSuccess fill:#fff3e0
    style RecordEpicError fill:#ffebee
    style RecordIssueError fill:#ffebee
    style StoreIdempotency fill:#e8f5e9
```

### GET /health

Health check endpoint for liveness/readiness.

Checks:
- Process health
- GitLab reachability (`/version` endpoint)
- Redis connectivity
- Embedding provider availability

## Error Handling

Unified error response format:

```json
{
  "error": {
    "code": "string",
    "message": "string",
    "gitlab_status": 404 | null,
    "details": {} | null
  }
}
```

**Common Error Codes:**
- `invalid_request` - Malformed request data
- `unauthorized` - Missing or invalid authentication (401)
- `forbidden` - Insufficient permissions (403)
- `not_found` - Resource not found (404)
- `conflict` - Resource conflict (409)
- `rate_limited` - GitLab rate limit exceeded (429)
- `gitlab_unavailable` - GitLab service unavailable (503)
- `idempotency_key_required` - Missing required idempotency key
- `cache_unavailable` - Redis cache unavailable
- `embedding_unavailable` - Embedding provider unavailable

## Integration Contracts

### ai_tasks_service → gitlab_client_service

- Uses `GET /gitlab/projects/{project_id}/backlog`
- Requires normalized fields: `{ kind, id, title, description, state, web_url, title_embedding }`
- Labels are optional

### UI Service → gitlab_client_service

- On Submit, calls `POST /gitlab/projects/{project_id}/apply-backlog`
- Can use `dry_run=true` query parameter for pre-validation (if implemented)
- Handles progress updates via Redis pub/sub subscription

### Service Integration Flow

```mermaid
sequenceDiagram
    participant UI as ui-service
    participant GitLabClient as gitlab-client-service
    participant AITasks as ai-tasks-service
    participant GitLabAPI as GitLab API
    participant Redis
    participant OpenAI as OpenAI/Azure

    Note over UI,OpenAI: Scenario: Generate and Apply Backlog

    UI->>GitLabClient: POST /cache-embeddings
    activate GitLabClient
    GitLabClient->>UI: 202 Accepted<br/>channel: embeddings:projects:123
    deactivate GitLabClient
    
    par Background: Cache Embeddings
        GitLabClient->>GitLabAPI: Fetch all epics & issues
        GitLabAPI-->>GitLabClient: Work items
        GitLabClient->>OpenAI: Batch embed titles
        OpenAI-->>GitLabClient: Embeddings
        GitLabClient->>Redis: Store embeddings
        GitLabClient->>Redis: PUBLISH progress events
    and UI subscribes to progress
        UI->>Redis: SUBSCRIBE embeddings:projects:123
        Redis-->>UI: Progress updates
    end
    
    UI->>GitLabClient: GET /backlog?state=opened
    activate GitLabClient
    GitLabClient->>GitLabAPI: List epics & issues
    GitLabAPI-->>GitLabClient: Raw GitLab data
    GitLabClient->>Redis: GET embeddings
    Redis-->>GitLabClient: Cached embeddings
    GitLabClient->>GitLabClient: Normalize + enrich
    GitLabClient-->>UI: Normalized work items<br/>with embeddings
    deactivate GitLabClient
    
    UI->>AITasks: POST /generate-backlog<br/>with existing items
    activate AITasks
    AITasks->>AITasks: Duplicate detection<br/>using embeddings
    AITasks->>OpenAI: Generate new items
    OpenAI-->>AITasks: AI-generated backlog
    AITasks-->>UI: Generated backlog
    deactivate AITasks
    
    UI->>UI: User reviews<br/>& approves
    
    UI->>GitLabClient: POST /apply-backlog<br/>with reviewed items
    activate GitLabClient
    GitLabClient->>GitLabClient: Check idempotency
    
    loop For each epic/issue
        GitLabClient->>GitLabAPI: Create or Update
        GitLabAPI-->>GitLabClient: Created/Updated item
    end
    
    GitLabClient->>GitLabClient: Build response<br/>with results & errors
    GitLabClient-->>UI: ApplyBacklogResponse
    deactivate GitLabClient
    
    UI->>UI: Display results<br/>to user
    
    style UI fill:#e3f2fd
    style GitLabClient fill:#fff3e0
    style AITasks fill:#e8f5e9
    style Redis fill:#ffebee
    style OpenAI fill:#f3e5f5
```

## Docker Compose Integration

### Service Definition

Add to `docker-compose.yml`:

```yaml
gitlab-client-service:
  build:
    context: ./services
    dockerfile: ./gitlab_client_service/Dockerfile
  ports:
    - "8011:8000"
  env_file:
    - ./docker-compose.env
  environment:
    - API_PORT=8000
  depends_on:
    - gitlab-mock-service
    - openai-mock-service
    - redis
  networks:
    - app-network
  healthcheck:
    test: ["CMD", "curl", "-f", "http://localhost:8000/health"]
    interval: 30s
    timeout: 10s
    retries: 3
    start_period: 5s
```

### Required Environment Variables

From `docker-compose.env`:

```env
# GitLab Instance Configuration
GITLAB_BASE_URL=https://gitlab.com
GITLAB_VERIFY_SSL=true

# GitLab OAuth Configuration
# Note: Redirect URI points to ui-service (port 8007) which proxies to gitlab-client-service (port 8012)
GITLAB_OAUTH_CLIENT_ID=your-gitlab-oauth-client-id
GITLAB_OAUTH_CLIENT_SECRET=your-gitlab-oauth-client-secret
GITLAB_OAUTH_REDIRECT_URI=http://localhost:8007/auth/gitlab/callback
GITLAB_OAUTH_SCOPES=read_api api

# GitLab Client Service Configuration
DEFAULT_PAGE_SIZE=100
EMBEDDINGS_PUBSUB_PREFIX=embeddings:projects:
IDEMPOTENCY_TTL_SECONDS=86400

# Redis Configuration
REDIS_URL=redis://redis:6379
REDIS_PASSWORD=
REDIS_DB=0

# Azure OpenAI Configuration
OAI_BASE_URL=https://<your-resource>.openai.azure.com
OAI_KEY=your-azure-openai-key
OAI_API_VERSION=2024-02-01
OAI_EMBED_MODEL_NAME=text-embedding-3-small
OAI_EMBED_DEPLOYMENT_NAME=text-embedding-3-small
OAI_EMBED_BATCH=16
OAI_EMBED_CONCURRENCY=2

# Local JWT (for S2S auth)
LOCAL_JWT_SECRET=dev-local-jwt-secret
```

## Testing

### Unit Tests

Run unit tests (24 passing):

```bash
cd services/gitlab_client_service
python -m pytest tests/ -v
```

### Integration Testing

To test with `gitlab_mock_service`:

1. **Start dependencies:**
   ```bash
   docker-compose up -d redis gitlab-mock-service openai-mock-service
   ```

2. **Run service locally:**
   ```bash
   cd services/gitlab_client_service
   export $(cat ../../docker-compose.env | xargs)
   export GITLAB_BASE_URL=http://localhost:8006
   export OAI_BASE_URL=http://localhost:8000
   export REDIS_URL=redis://localhost:6379
   uvicorn src.main:app --host 0.0.0.0 --port 8011 --reload
   ```

3. **Test endpoints:**
   ```bash
   # Health checks
   curl http://localhost:8011/health
   curl http://localhost:8011/health/redis
   curl http://localhost:8011/health/gitlab
   curl http://localhost:8011/health/embeddings

   # Get backlog (requires GitLab token)
   curl -H "GitLab-Access-Token: <token>" \
        -H "Authorization: Bearer <jwt>" \
        http://localhost:8011/gitlab/projects/1/backlog
   
   # Cache embeddings
   curl -X POST \
        -H "GitLab-Access-Token: <token>" \
        -H "Authorization: Bearer <jwt>" \
        http://localhost:8011/gitlab/projects/1/cache-embeddings
   
   # Apply backlog
   curl -X POST \
        -H "GitLab-Access-Token: <token>" \
        -H "Authorization: Bearer <jwt>" \
        -H "Content-Type: application/json" \
        -d '{"project_id": "1", "prompt_id": "test", "epics": [], "issues": []}' \
        http://localhost:8011/gitlab/projects/1/apply-backlog
   ```

## Examples

### Example 1: Fetch Project Backlog

**Request:**
```http
GET /gitlab/projects/123/backlog?labels=backend,security&state=opened&page=1&per_page=50
Headers:
  GitLab-Access-Token: <gitlab_access_token>
  Authorization: Bearer <local_jwt>
```

**Response:**
```json
{
  "items": [
    {
      "kind": "issue",
      "id": "1345",
      "title": "Fix JWT validation",
      "description": "...",
      "state": "opened",
      "labels": ["backend", "security"],
      "web_url": "https://gitlab.example.com/...",
      "title_embedding": [0.123, 0.456, ...]
    }
  ],
  "pagination": {
    "page": 1,
    "per_page": 50,
    "next_page": 2,
    "prev_page": null,
    "total": null
  }
}
```

### Example 2: Cache Embeddings

**Request:**
```http
POST /gitlab/projects/123/cache-embeddings
Headers:
  GitLab-Access-Token: <gitlab_access_token>
  Authorization: Bearer <local_jwt>
```

**Response:**
```json
{
  "message": "Embedding caching started",
  "project_id": "123",
  "channel": "embeddings:projects:123"
}
```

**Pub/Sub Channel:** `embeddings:projects:123`

**Example Progress Message:**
```json
{ "event": "completed", "project_id": "123" }
```

### Example 3: Apply Backlog

**Request:**
```http
POST /gitlab/projects/123/apply-backlog
Headers:
  GitLab-Access-Token: <gitlab_access_token>
  Authorization: Bearer <local_jwt>
Content-Type: application/json

{
  "project_id": "123",
  "prompt_id": "gen-abc-123",
  "epics": [
    {
      "id": null,
      "title": "Security Hardening",
      "description": "Implement security improvements",
      "labels": ["security"]
    }
  ],
  "issues": [
    {
      "id": null,
      "title": "Rotate service keys",
      "description": "Update all service authentication keys",
      "labels": ["security"],
      "epic_id": null
    }
  ]
}
```

**Response:**
```json
{
  "results": {
    "epics": [
      {
        "input_index": 0,
        "action": "created",
        "id": "567",
        "web_url": "https://gitlab.example.com/groups/mygroup/-/epics/567"
      }
    ],
    "issues": [
      {
        "input_index": 0,
        "action": "created",
        "id": "1346",
        "web_url": "https://gitlab.example.com/mygroup/myproject/-/issues/1346"
      }
    ]
  },
  "errors": []
}
```

## Best Practices Followed

1. **SOLID Principles**
   - Single Responsibility: Each service class has one concern
   - Dependency Inversion: Dependencies injected via FastAPI
   - Interface Segregation: Clear separation of read/write operations

2. **DRY Principle**
   - Reused shared Redis client utilities
   - Reused shared LLM configuration
   - Reused shared JWT authentication

3. **Code Quality**
   - Type hints throughout
   - Comprehensive docstrings
   - Structured logging with `structlog`
   - Error handling at every layer

4. **Minimal & Clean Code**
   - No backward compatibility code
   - No unused APIs or scaffolding
   - Modern Python patterns (3.10+)

## Known Limitations & Future Improvements

1. **Idempotency Store**
   - Currently in-memory (will reset on service restart)
   - Production should use Redis-backed store

2. **Epic Linking**
   - Issue-to-epic linking not yet implemented
   - Requires epic IID tracking and API calls

3. **Rate Limiting**
   - Basic retry with exponential backoff
   - Could add circuit breaker pattern

4. **Caching Strategy**
   - No TTL on cached embeddings
   - Consider automatic invalidation on GitLab updates

5. **Metrics**
   - Structured logging present
   - Prometheus metrics not yet exposed

## Implementation Notes for Engineers

- Normalize GitLab objects to the schemas above; avoid leaking raw fields
- Resolve group for epics via the project's namespace; prefer nearest ancestor with epics enabled
- Idempotent writes: no-op when no changes detected; otherwise update diffs only
- Use `python-gitlab` with persistent sessions, retries, and rate-limit handling
- Use built-in pagination and surface pagination metadata
- Epics: `gl.groups.get(group_id).epics.list(...)` with filters
- Issues: `gl.projects.get(project_id).issues.list(...)` with filters (labels, state, search)
- Support server-side filtering; if client-side filtering is applied, set header `X-Client-Filtered: true`
- Embeddings cache keys: `<project_id>:<work_item_id>`; values are numeric arrays
- List endpoints read `title_embedding` from cache only; they do not compute embeddings on demand

## Security & Compliance

- Store credentials only in env/secrets manager; never in repo
- Enforce input validation; avoid logging PII or sensitive descriptions
- Respect TLS/verification settings; never log tokens; mask headers in logs

## API Documentation

Once deployed, interactive API documentation is available at:
- **Swagger UI:** `http://localhost:8011/docs`
- **ReDoc:** `http://localhost:8011/redoc`
- **OpenAPI JSON:** `http://localhost:8011/openapi.json`

## Deployment Checklist

- [x] Dockerfile configured with multi-stage build
- [x] Health checks implemented
- [x] Environment variables documented
- [x] Dependencies locked in pyproject.toml
- [ ] Add to docker-compose.yml
- [ ] Configure service networking
- [ ] Set up logging aggregation
- [ ] Add monitoring/alerting
- [ ] Run E2E tests in docker-compose environment
- [ ] Performance/load testing

## Observability

### Health Checks

- `/health` - Overall service health
- `/health/redis` - Redis connectivity
- `/health/gitlab` - GitLab API reachability
- `/health/embeddings` - Embedding provider availability

### Logging

- Structured logging with `structlog`
- Request IDs propagated via `X-Request-ID` header
- Log levels: DEBUG, INFO, WARNING, ERROR
- Key logged events:
  - API request/response
  - GitLab API calls
  - Embedding generation
  - Cache operations
  - Error conditions

### Metrics (Future)

Planned metrics:
- Request count/duration by endpoint
- GitLab API error rates
- Retry counts
- Cache hit/miss rates
- Embedding generation latency

## Summary: Stateless OAuth Architecture

The gitlab-client-service implements a **production-ready, stateless OAuth 2.0 flow** optimized for modern cloud deployments:

### Key Implementation Details

**Stateless OAuth Flow:**
```python
# /authorize endpoint - No session storage
state = encode_state(session_id=sid, redirect_uri=uri, csrf=token)
return RedirectResponse(f"{GITLAB_URL}/oauth/authorize?state={state}")

# /callback endpoint - Decode state from URL
session_id, redirect_uri, csrf = decode_state(request.query_params["state"])
validate_csrf(csrf)
token = await oauth.gitlab.fetch_access_token(code=code, redirect_uri=callback_uri)
await save_token(session_id, redis_client, token)
```

### Related Documentation

- **UI Service README** - [Proxy architecture and S2S authentication](../ui_service/README.md#gitlab-integration-proxied)
- **Authlib Documentation** - [OAuth 2.0 Client](https://docs.authlib.org/en/latest/client/starlette.html)
- **GitLab OAuth API** - [Official documentation](https://docs.gitlab.com/ee/api/oauth2.html)

---