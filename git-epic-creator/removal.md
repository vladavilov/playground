# Proposal: move auth + policy enforcement into `authentication_service` and keep a thin SSE bridge (`sse_bridge_service`)

## Executive summary (updated)

You can **remove almost all responsibilities** from the old gateway control plane and keep only a thin SSE bridge (`services/sse_bridge_service`), if you relocate the **authentication + policy enforcement** responsibilities into `authentication_service`.

1) **Browser session + policy enforcement for Envoy `ext_authz`** (`/auth/*` session establishment + `/authz` decision + header injection)  
2) **Redis Pub/Sub → SSE bridging** (`/events`) stays, but the service becomes *extremely thin* (`sse_bridge_service`)

Revised target split:

- **Move authentication (MSAL session) + gateway policy enforcement (`/authz`) → `authentication_service`** ✅ feasible and valuable  
- **Keep `/events` in `sse_bridge_service`**, but make it SSE-only (no sessions, no MSAL, no token exchange) ✅ feasible and low risk

Below is a feasibility/value assessment and a concrete implementation plan.

---

## Current architecture (as implemented)

### Envoy (data plane)

Envoy is the single entrypoint and:

- routes `/auth/*` → `authentication_service` (ext_authz disabled)  
- calls `authentication_service /authz` via `envoy.filters.http.ext_authz`  
- routes protected APIs (`/project`, `/workflow`, `/tasks`, `/gitlab`) to microservices with injected headers  
- routes `/events` to `sse_bridge_service` (SSE)

Compose config: `envoy/envoy-compose.yaml`  
K8s config: `k8s/base/envoy-configmap.yaml`

### `sse_bridge_service` (SSE-only)

Owns:

- **SSE** (`/events`): subscribes to Redis pub/sub and streams SSE to browser clients (auth enforced by Envoy)

### `authentication_service`

Currently owns:

- (removed) `/auth/exchange`: legacy Azure token -> LOCAL JWT exchange (replaced by Envoy `ext_authz`)
- `/auth/userinfo`: validate LOCAL JWT (legacy path)
- `/auth/s2s/mint`: mint the gateway’s **service-to-service (S2S) JWT** (guarded by `API_GATEWAY_MINT_SECRET`)

### `frontend_service`

Currently:

- consumes SSE using `new EventSource("/events")` (same-origin)
- does **not** implement `/events` server-side
- runs with `react-router-serve build/server/index.js` in production (`npm start`)

---

## Target architecture (updated)

Keep `sse_bridge_service`, but only for `/events`.

1) **Auth/session + `ext_authz` policy enforcement** → `authentication_service`  
2) **SSE `/events` stays in `sse_bridge_service`** (SSE-only service)  
3) Envoy becomes:
   - `/auth/*` → `authentication_service`
   - `ext_authz` calls `authentication_service /authz`
   - `/events` → `sse_bridge_service` (with ext_authz enabled so SSE is protected)

---

## Feasibility + value assessment

### A) Move auth/session + `/authz` into `authentication_service`

**Feasible:** yes.

**What it requires**

- Add `SessionMiddleware` to `authentication_service` and port the MSAL router from the old gateway control plane
  - The cookie is the **single state mechanism** for “who is logged in”.
- Implement `/authz` in `authentication_service` (same contract as today: returns 200/403 and injected headers).

**High-value simplification opportunity**

Today `authentication_service /authz` can mint the S2S JWT in-process (no HTTP hop).

If `/authz` lives inside `authentication_service`, it can mint the S2S JWT **directly in-process** via `TokenService.mint_gateway_service_token(...)`.

That removes from the gateway layer:

- the cross-service `/authz -> /auth/s2s/mint` HTTP hop
- the need for `upstream_http_client` and `S2STokenCache` *in an authz service* (the auth service can mint in-process)

**Net effect**

- fewer moving parts
- less latency per request (no mint HTTP call)
- cleaner “auth owns auth” boundary

### B) Keep `/events` in `sse_bridge_service`, but make it SSE-only

**Feasible:** yes, and it’s the **lowest-risk** way to keep SSE behavior stable.

**What “SSE-only” means**

- Keep only:
  - Redis pub/sub subscription
  - event-name mapping + streaming semantics
  - health endpoint (optional, or rely on Envoy health)
- Remove:
  - `SessionMiddleware`
  - MSAL `/auth/*` routes
  - `/authz`
  - any token exchange/minting code

**How SSE stays protected**

Envoy keeps `ext_authz` enabled for `/events`, and `/authz` moves to `authentication_service`.
So the SSE service does not need to understand cookies or tokens; Envoy blocks unauthenticated clients.

---

## Recommendation (updated)

### Recommended target (matches your updated direction)

1) Move **all authentication burden** (MSAL session endpoints + `/authz`) into `authentication_service`
2) Keep `sse_bridge_service` but make it **SSE-only** (`/events` only)
3) Keep `frontend_service` UI-only (no Redis coupling, no custom server)

---

## Implementation plan (phased)

### Phase 0 — Baseline safety checks (no behavior changes)

- Confirm Envoy currently calls `ext_authz` for:
  - `/project`, `/workflow`, `/tasks`, `/gitlab`, `/auth/gitlab/*`, `/events`
- Confirm frontend uses `EventSource("/events")` (same-origin)

Acceptance:
- `docker compose config` passes
- `kubectl kustomize` base + overlays pass

### Phase 1 — Move MSAL session endpoints from the old gateway control plane → `authentication_service`

**Files**

- **Move/port** MSAL browser auth router to `authentication_service`
- Update `authentication_service/src/main.py` to:
  - add `SessionMiddleware`
  - include the new router (mounted at `/auth` to preserve URLs)
- Ensure `/auth/login`, `/auth/callback`, `/auth/me`, `/auth/logout` remain unchanged for the browser.

Acceptance:
- Browser login works (session cookie set, `/auth/me` reflects it)
- No dependency on `/auth/exchange` for browser path (removed)

[x] Phase 1 completed (tests: `services/authentication_service` pytest; manifests: `kubectl kustomize k8s/base` + `kubectl kustomize k8s/overlays/local`).

### Phase 2 — Move `/authz` from the old gateway control plane → `authentication_service`

**Core change**

- Implement `POST|GET /authz` in `authentication_service`:
  - reads session (`oid`, `roles`)
  - enforces the same RBAC policy / x-user-id allowlist
  - mints S2S token **directly** using `TokenService.mint_gateway_service_token(...)`
  - returns headers: `authorization`, `x-user-id` (allowlist)

**Files**

- Port existing `/authz` policy logic into `authentication_service/src/routers/authz_router.py`
- Delete use of `S2STokenCache` in this flow (optional; token mint is cheap and TTL is short).

**Envoy updates**

- Update ext_authz target from the old gateway control plane → `authentication-service`
  - `k8s/base/envoy-configmap.yaml`
  - `envoy/envoy-compose.yaml`
- Update clusters accordingly.

Acceptance:
- All protected routes succeed end-to-end through Envoy (microservices see S2S auth)
- Unauthenticated requests are denied

[x] Phase 2 completed (tests: `services/authentication_service` pytest; manifests: `kubectl kustomize k8s/base` + `kubectl kustomize k8s/overlays/local`; config: `docker compose config`).

### Phase 3 — Thin `sse_bridge_service` down to SSE-only

- Ensure the service contains only `/events` (+ `/health`).
- Ensure `/events` keeps existing event-name mapping and streaming behavior.
- Keep Envoy ext_authz enabled for `/events` (auth is enforced by `authentication_service /authz`).
- Update Envoy routing:
  - `/auth/*` → `authentication_service`
  - `/authz` (ext_authz target) → `authentication_service`
  - `/events` → `sse_bridge_service`

Acceptance:
- Browser SSE still works (`EventSource("/events")`)
- Unauthenticated SSE is blocked by Envoy (not by the SSE service)

[x] Phase 3 completed (tests: `services/sse_bridge_service` pytest; manifests: `kubectl kustomize k8s/base` + `kubectl kustomize k8s/overlays/local`; config: `docker compose config`).

---

## MCP server integration with Envoy (`neo4j_retrieval_mcp_server`)

### What the MCP server does today

From `services/neo4j_retrieval_mcp_server`:

- VS Code’s MCP client performs OAuth against Azure AD **by opening a browser window** (SSO), per the MCP auth spec.
- After interactive SSO completes, the MCP client sends an **Azure AD access token** on subsequent MCP calls as `Authorization: Bearer ...` (i.e., it mimics a browser-style “bearer token per request” client).
- MCP server uses Azure Bearer tokens and calls upstream APIs via Envoy (Envoy `ext_authz` -> injected S2S headers).

### Problem with “direct-to-microservices” after the gateway migration

Microservices are now **S2S-only** (they expect the gateway-injected service token + optional `x-user-id`), so the MCP server should **stop calling microservices directly** and should call them **through Envoy**.

### Proposed MCP → Envoy flow (recommended)

1) VS Code authenticates to Azure AD and calls the MCP server with `Authorization: Bearer <azure_access_token>`.
2) MCP server forwards requests to Envoy (not to microservices), e.g.:
   - `GET/POST /project/*`, `/workflow/*`, `/tasks/*`, `/gitlab/*`, `/code-graph/*`
3) MCP server includes the same `Authorization: Bearer <azure_access_token>` header when calling Envoy.
4) Envoy `ext_authz` calls `authentication_service /authz`.
5) `authentication_service /authz` must support **non-browser callers**:
   - If a browser session cookie exists → use session (`oid`, `roles`)
   - Else if `Authorization: Bearer <azure_access_token>` exists → validate Azure token (JWKS) and extract (`oid`, `roles`)
6) `authentication_service /authz` mints S2S token in-process and returns injected headers:
   - `Authorization: Bearer <s2s_token>`
   - `x-user-id: <oid>` (allowlist)
7) Envoy forwards to microservices with injected headers.

**Important operational detail (reauth)**

If the MCP server calls Envoy with an expired/invalid Azure token, Envoy will deny via `ext_authz` (typically 403).
The MCP server should translate that outcome into an MCP-auth-friendly **401** so VS Code re-runs OAuth automatically (mirroring the initial “no token” behavior).

### Required Envoy change for MCP support

Envoy must forward `Authorization` to `ext_authz` service.
In both:

- `envoy/envoy-compose.yaml`
- `k8s/base/envoy-configmap.yaml`

add `authorization` to `authorization_request.allowed_headers`.

### Where “MSAL” fits

MSAL is a browser-oriented library and remains part of the **browser auth** flow hosted by `authentication_service`.
The MCP flow uses **VS Code’s built-in OAuth MCP client**, which performs interactive login by launching the browser.
So MCP does not use MSAL; it uses the standard OAuth discovery + browser-based Azure SSO flow that VS Code implements.


---

## Risks / gotchas

- **Session cookie scope:** If auth endpoints move to `authentication_service`, cookie domain/path/secure flags must still match browser expectations.
- **GitLab OAuth:** `/auth/gitlab/*` must continue to route to `gitlab-client-service` (and not be shadowed by `/auth` route ordering in Envoy).
- **MCP header plumbing:** Envoy must forward `authorization` to `ext_authz` for MCP support, but only the *minimal set* of headers should be allowed to avoid accidental header leakage.
- **`/authz` dual-mode auth:** `authentication_service /authz` must handle both browser cookies and bearer tokens deterministically (cookie-first vs bearer-first), and reject ambiguous/unsafe combinations.
- **Auth service “scope creep”:** `authentication_service` becomes “BFF-like” (auth + session + policy enforcement). That might be acceptable for a PoC, but it’s a deliberate coupling.


