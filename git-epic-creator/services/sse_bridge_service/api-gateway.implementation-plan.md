# API Gateway (Envoy + Python control-plane) â€” Full Implementation Plan

This document expands `api-gateway.implementation-plan.md` into **sequential, AI-coding-agent-ingestable tasks**, grounded in the *current* repo implementation (`sse_bridge_service`, `authentication_service`, `shared`, `k8s`, `infra`).

## Non-negotiable requirements (from the draft plan)

- **All requests go through the API gateway.** Frontend and any other browser-facing apps never call microservices directly.
- **Internal S2S tokens replace user-bound tokens.** Microservices must **not** require user-bound JWTs anymore.
- **Azure AD remains external identity** for browser users (MSAL in gateway).
- **Preserve existing SSE event-name mapping** on `/events` (the mapping logic in `sse_bridge_service` stays identical).
- **No backwards compatibility.** We will change services/contracts and delete obsolete routes.

## Repo reality check (what already exists)

- `sse_bridge_service` provides:
  - Redis→SSE bridge with explicit event-name mapping: `services/sse_bridge_service/src/routers/sse_router.py`
- `authentication_service` already provides:
  - (removed) `/auth/exchange` (legacy Azure access token -> LOCAL_JWT)
  - `/auth/userinfo` and `/auth/validate`
  - Implementation: `services/authentication_service/src/routers/token_router.py`, `services/authentication_service/src/services/token_service.py`
- `shared` already provides:
  - HMAC JWT helpers: `services/shared/src/utils/jwt_utils.py`
  - FastAPI auth deps for **gateway S2S tokens**: `services/shared/src/utils/local_auth.py`
- `k8s` routes `/events` to `sse-bridge-service` and `/auth*` to `authentication-service`.

## Important correction (Azure authority env var)

`AzureAuthSettings.AZURE_AD_AUTHORITY` is expected to be a **base authority** (e.g. `https://login.microsoftonline.com`) and code composes URLs using `/{tenantId}`:

- `shared/srcuration/azure_auth_config.py` computes `OPENID_CONFIG_URL = f"{AZURE_AD_AUTHORITY}/{AZURE_TENANT_ID}/..."`.
- `authentication_service` composes `authority = f"{AZURE_AD_AUTHORITY}/{AZURE_TENANT_ID}"` for MSAL-based browser login.

But `k8s/basemap.yaml` currently sets:
- `AZURE_AD_AUTHORITY: "https://login.microsoftonline.com/${AZURE_TENANT_ID}"`

That will lead to double-tenant URLs. This plan includes a task to fix that to:
- `AZURE_AD_AUTHORITY: "https://login.microsoftonline.com"`

## Target architecture (final)

### Traffic

Browser â†’ NGINX Ingress â†’ **Envoy (data plane)** â†’ upstream services

### Responsibilities

- **Envoy**
  - routing, retries, timeouts
  - circuit breakers + outlier detection
  - access logs/metrics/tracing hooks
  - calls `authentication_service /authz` via `ext_authz` and injects headers upstream

- **Python `sse_bridge_service` (SSE bridge)**
  - Azure AD login via MSAL â†’ session cookie
  - authorization checks (policy enforcement)
  - `ext_authz` endpoint `/authz` returning headers for Envoy injection:
    - `Authorization: Bearer <S2S_JWT>` (service-bound)
    - `x-user-id` **only** where required
  - `/events` SSE bridge (existing mapping preserved)

- **`authentication_service`**
  - uses Envoy `ext_authz` + cookie session / Azure Bearer; no `/auth/exchange`
  - adds **`/auth/s2s/mint`** for **service-bound** tokens (sub=api-gateway)

- **Microservices**
  - validate only **service-bound** JWT (and require caller is gateway via `sub`)
  - do **not** use roles/oid from JWT for end-user permissions
  - accept `x-user-id` only where needed:
    - `project-management-service` (DB visibility and attribution)
    - `gitlab-client-service` (lookup GitLab OAuth token in Redis)

## Conventions (must be consistent across tasks)

- **Service JWT claim contract**
  - `sub = "api-gateway"`
  - `iss = "authentication-service"`
  - `aud = "internal-services"` (recommended; keep simple)
  - `iat`, `exp`
  - No user claims (`oid`, `roles`, etc.)

- **Trusted user header contract**
  - `x-user-id: <Azure oid>` is injected only by gateway and only on allowed routes.
  - Services must treat `x-user-id` as trusted **only when service token is valid and `sub=api-gateway`**.

- **No direct service exposure**
  - K8s NetworkPolicies must enforce only gateway/Envoy can reach backend services.

---

## [x] Task 0 â€” Confirm the two global policy knobs

### Goal
Freeze the two choices that affect multiple services and avoid rework.

### Decisions to confirm
- **Audience model**
  - **Recommended**: single audience `aud="internal-services"`.
  - Rationale: `shared/utils/jwt_utils.verify_jwt` currently disables audience verification anyway.
- **`x-user-id` propagation allowlist**
  - **Recommended allowlist**:
    - `/project/*` â†’ `project-management-service`
    - `/gitlab/*` and `/auth/gitlab/*` â†’ `gitlab-client-service`
  - Disallow for `/workflow/*`, `/tasks/*`, `/code-graph/*`, etc.

### Decision outcome (confirmed)
- **Audience model**: `aud="internal-services"` for gateway-minted service tokens
- **`x-user-id` allowlist**: `/project/*`, `/gitlab/*`, `/auth/gitlab/*` only

### Output
- Document as constants inside gateway policy module introduced in Task 4.

### Acceptance criteria
- Decisions are encoded as code constants and referenced by gateway logic (not duplicated across files).

---

## [x] Task 1 â€” Add missing K8s deployment for `authentication-service`

### Goal
Ensure cluster has `authentication-service` reachable at `http://authentication-service:80` (consistent with `AUTH_SERVICE_URL` defaults).

### Files to add/change
- **Add**: `k8s/base/services/authentication-service.yaml`
- **Update**: `k8s/base/kustomization.yaml` (include the new resource)
- **Optional**: `k8s/README.md` (mention the service, and that gateway uses it)

### Implementation details
- Deployment:
  - label `tier: backend`
  - mount secrets via CSI (`agentic-ai-secrets`) like other services
  - pass `LOCAL_JWT_SECRET` env from `agentic-ai-secrets`
- Service:
  - ClusterIP, port 80 â†’ container 8000

### Acceptance criteria
- `kubectl get svc -n agentic-ai authentication-service` shows the Service.
- Other pods can reach `http://authentication-service:80/health`.

---

## [x] Task 2 â€” Implement service-token minting: `POST /auth/s2s/mint`

### Goal
Add a **service-bound** token mint endpoint for the gateway to retrieve a cached S2S token.

### Files to change
- `services/authentication_service/src/routers/token_router.py`
- `services/authentication_service/src/services/token_service.py`
- `services/authentication_service/README.md`
- `services/authentication_service/tests/test_*.py` (new tests)

### Endpoint contract

**Request**
- `POST /auth/s2s/mint`
- Header: `X-Api-Gateway-Secret: <secret>` (POC authn for caller)
- JSON body:
  - `aud` (optional): `"internal-services"`

**Response**
```json
{
  "access_token": "<jwt>",
  "token_type": "Bearer",
  "expires_in": 600
}
```

### Token claims
- `sub`: `"api-gateway"`
- `iss`: `"authentication-service"`
- `aud`: `"internal-services"`
- `iat`, `exp`

### Notes
- No `/auth/exchange` (removed). Browser path uses sessions; non-browser callers use Azure Bearer via Envoy.
- TTL:
  - recommended: **10 minutes** (keep short; gateway caches and refreshes).

### Tests (minimum)
- Reject missing header secret.
- Reject wrong secret.
- Successful mint returns JWT that verifies with `LOCAL_JWT_SECRET` and includes `sub=api-gateway`.

### Acceptance criteria
- New endpoint works in isolation and does not break existing auth-service endpoints.

---

## [x] Task 3 â€” Add shared dependency for verifying gateway service tokens

### Goal
Give all microservices a consistent dependency to validate â€œcaller is api-gatewayâ€.

### Files to change
- `services/shared/src/utils/local_auth.py`

### Implementation details
- Add a new dataclass (or reuse `LocalUser` but **do not** overload it):
  - `LocalServiceCaller { sub: str, token: str }`
- Add dependency:
  - `get_gateway_service_verified(expected_sub: str = "api-gateway")`
    - extracts bearer token
    - verifies signature + exp via `verify_jwt`
    - checks `claims["sub"] == expected_sub`
    - optionally checks `claims["iss"] == "authentication-service"` (recommended)

### Acceptance criteria
- Services can `Depends(get_gateway_service_verified)` and reliably reject non-gateway tokens.

---

## [x] Task 4 — Implement Envoy `ext_authz` endpoint in `authentication_service`: `POST/GET /authz`

### Goal
Make `authentication_service` the **single policy enforcement point** and the provider of injected headers.

### Files to add/change
- **Add**: `services/authentication_service/src/routers/authz_router.py`
- **Update**: `services/authentication_service/src/main.py` (include router)

### Functional behavior

#### 1) Authentication check
- Require:
  - `request.session["sid"]` exists
  - `request.session["oid"]` exists
- Optionally (recommended):
  - validate the sessionâ€™s `local_jwt` with `authentication-service /auth/userinfo`
  - if invalid: deny and force re-login

#### 2) Authorization policy
- Build a policy table with:
  - route prefix (e.g. `/project/projects`)
  - HTTP method
  - required role(s)
- Use roles from `request.session["roles"]` (set by existing login flow).

Keep policy logic **only in gateway**; downstream services will be simplified later.

#### 3) Header injection output (for Envoy)
- Always inject:
  - `Authorization: Bearer <service_s2s_jwt>` (token minted via Task 2)
- Conditionally inject:
  - `x-user-id: <oid>` only for allowed routes (Task 0 allowlist)

### Implementation guidance (token caching)
- Cache service token for ~60â€“300 seconds, refresh when exp is near.
- Cache key can be a single in-memory entry in each instance (POC). Envoy will load-balance; short TTL is fine.

### Suggested response format
Return `200` on allow, `403` on deny; include injected headers in response headers.

Example allow response:
```python
return JSONResponse(
    status_code=200,
    content={"ok": True},
    headers={
        "authorization": f"Bearer {svc_token}",
        "x-user-id": user_id,  # only when allowed
    },
)
```

### Acceptance criteria
- Unauthenticated session â†’ deny.
- Authenticated but missing required role â†’ deny.
- Authorized â†’ allow and returns S2S Authorization header.

---

## [x] Task 5 — Remove Python reverse proxying from the gateway layer (Envoy owns routing)

### Goal
Make the gateway layer minimal: keep `/auth/*` + `/authz` in `authentication_service`, keep `/events` in `sse_bridge_service`.

### Files to change
- `services/sse_bridge_service/src/main.py`
- `services/sse_bridge_service/README.md`

### Implementation details
- Delete proxy endpoints:
  - `/project/{path:path}`
  - `/workflow/{path:path}`
  - `/tasks/{path:path}`
  - `/gitlab/{path:path}`
  - `/code-graph/{path:path}`
  - `/auth/gitlab/{path:path}` proxying (this becomes Envoy routing + `x-user-id` header)
- No `/config` endpoint. UI configuration lives in `frontend_service` as a constant (`UI_CONFIG`).

### Acceptance criteria
- `sse_bridge_service` does not forward API calls to internal services (SSE only).

---

## [x] Task 6 â€” Require authenticated session for `/events` (keep mapping identical)

### Goal
Lock down SSE so it matches the â€œgateway enforces authâ€ requirement.

### Files to change
- `services/sse_bridge_service/src/routers/sse_router.py`

### Implementation details
- Before connecting, check session:
  - require `sid` and `oid`
- Keep event mapping logic unchanged (do not change the channelâ†’event mapping rules).

### Acceptance criteria
- `/events` without session cookie returns 401/403.
- Existing SSE event names stay unchanged.

---

## [x] Task 7 â€” Introduce Envoy gateway in K8s (data plane)

### Goal
Add Envoy as the routing layer and configure `ext_authz` against `authentication_service`.

### Files to add/change
- **Add**: `k8s/base/services/envoy-gateway.yaml` (Deployment + Service)
- **Add**: `k8s/base/envoy-configmap.yaml` (Envoy bootstrap + listeners)
- **Update**: `k8s/base/ingress.yaml` to route gateway paths to `envoy-gateway`
- **Update**: `k8s/base/kustomization.yaml`
- **Update**: `k8s/README.md`

### Routing requirements
- Ingress routes to Envoy:
  - `/auth`, ``, `/project`, `/workflow`, `/tasks`, `/gitlab`, `/events`, `/code-graph`
- Envoy routes:
  - `/auth/*`, `/authz` → `authentication-service`
  - `/events` → `sse-bridge-service`
  - `/project/*` â†’ `project-management-service`
  - `/workflow/*` â†’ `ai-requirements-service`
  - `/tasks/*` â†’ `ai-tasks-service`
  - `/gitlab/*` and `/auth/gitlab/*` â†’ `gitlab-client-service`
  - `/code-graph/*` â†’ `code-graph-ingestion-service`

### Envoy filter requirements
- `ext_authz` filter:
  - calls `http://authentication-service/authz`
  - uses response headers as injected upstream headers

### Config snippet (conceptual)
```yaml
http_filters:
  - name: envoy.filters.http.ext_authz
    typed_config:
      "@type": type.googleapis.com/envoy.extensions.filters.http.ext_authz.v3.ExtAuthz
      http_service:
        server_uri:
          uri: http://authentication-service
          cluster: authentication_service
          timeout: 1s
        path_prefix: /authz
      failure_mode_allow: false
```

### Acceptance criteria
- Browser â†’ ingress â†’ envoy â†’ ext_authz â†’ upstream works for `/project/*`.
- `/events` streams without buffering (ensure ingress + envoy timeouts allow SSE).

---

## [x] Task 8 â€” Update NetworkPolicies to enforce â€œonly gateway reaches backendâ€

### Goal
Enforce â€œall requests go through gatewayâ€ at the network layer.

### Files to change
- `k8s/base/network-policy.yaml`

### Implementation details
- Allow ingress from NGINX controller â†’ **envoy-gateway** only.
- Allow ingress to backend pods â†’ **envoy-gateway** only.
- Allow envoy-gateway → authentication-service and envoy-gateway → sse-bridge-service.
- Ensure frontend-service cannot directly reach backend tier.

### Acceptance criteria
- Direct calls from `frontend-service` to backend services fail.
- Only Envoy can reach backend services.

---

## [x] Task 9 â€” Update microservices to accept only service-bound S2S tokens

### Goal
Remove end-user auth dependencies from services; enforce user permissions at gateway.

### Files to change (minimum set from current code usage)
- `services/project_management_service/src/routers/project_router.py`
- `services/ai_tasks_service/src/routers/tasks_router.py`
- `services/ai_requirements_service/src/routers/workflow_router.py`
- `services/neo4j_retrieval_service/src/routers/retrieval_router.py` (and any other service using `get_local_user_verified`)

### Implementation details
- Replace:
  - `current_user: LocalUser = Depends(get_local_user_verified)`
- With:
  - `caller = Depends(get_gateway_service_verified)`
- Remove role checks from services:
  - anything like `require_roles_local(["Admin"])` must move to gateway `/authz` policy.
- Where a service needs â€œwho is the userâ€, read from header:
  - `x-user-id` (only when allowed by gateway)

### Acceptance criteria
- Services reject calls without valid service token (`sub != api-gateway`).
- Services no longer interpret JWT roles as user permissions.

---

## [x] Task 10 â€” `project-management-service`: enforce visibility using `x-user-id`

### Goal
Keep DB-level visibility and attribution without user-bound JWT.

### Files to change
- `services/project_management_service/src/routers/project_router.py`
- `services/project_management_service/src/services/project_service.py` (and any DB query helpers)
- `services/project_management_service/tests/*`

### Implementation details
- Require `x-user-id` for user-scoped endpoints (list/get/update where user identity matters).
- Use `x-user-id` instead of `current_user.oid`.
- Preserve existing DB behavior, but driven by header.

### Acceptance criteria
- Projects returned are filtered by `x-user-id`.
- Create/update actions attribute changes to `x-user-id`.

---

## [x] Task 11 â€” GitLab identity propagation: remove query param hack, use `x-user-id`

### Goal
Stop relying on `oid` inside JWT and remove `user_id` query param mutation.

### Files to change
- `services/gitlab_client_service/src/dependencies.py`
- `services/gitlab_client_service/README.md`
- (legacy, removed) `proxy_router.py` logic that injected `user_id` query params

### Implementation details
- In `gitlab-client-service`:
  - verify gateway service token (`sub=api-gateway`)
  - extract `x-user-id` header and use it as Redis key for token storage/lookup
- Remove:
  - any expectation that `claims["oid"]` exists in the JWT
  - any reliance on `user_id` query parameter for OAuth flows

### Acceptance criteria
- GitLab OAuth status/API calls succeed with:
  - service token proving caller is gateway
  - `x-user-id` selecting the userâ€™s GitLab token

---

## [x] Task 12 â€” Secrets + infra wiring for mint authentication (POC secret)

### Goal
Provision and wire `API_GATEWAY_MINT_SECRET` so only gateway can mint service tokens.

### Files to change
- `infra/modules/secrets.bicep`
- `infra/main.bicep`
- `k8s/base/secret-provider.yaml`
- `k8s/base/services/sse-bridge-service.yaml`
- `k8s/base/services/authentication-service.yaml` (added in Task 1)

### Implementation details
- Add Key Vault secret:
  - `ApiGateway-MintSecret` (name choice is flexible; keep consistent)
- CSI map it into `agentic-ai-secrets` and expose as:
  - `API_GATEWAY_MINT_SECRET`
- `authentication_service` mints S2S tokens in-process for `ext_authz`.
- `authentication_service` uses it to validate `X-Api-Gateway-Secret`.

### Acceptance criteria
- No secrets are placed in ConfigMap.
- Gateway can mint S2S token inside cluster.

---

## [x] Task 13 â€” Fix Azure authority env var in K8s ConfigMap

### Goal
Prevent double-tenant URL composition errors.

### Files to change
- `k8s/basemap.yaml`

### Change
- From:
  - `AZURE_AD_AUTHORITY: "https://login.microsoftonline.com/${AZURE_TENANT_ID}"`
- To:
  - `AZURE_AD_AUTHORITY: "https://login.microsoftonline.com"`

### Acceptance criteria
- `shared` computed URLs (openid config, token URL) are correct.

---

## [x] Task 14 â€” Update docs (only existing README files + this plan)

### Goal
Align docs with the new gateway model.

### Files to change
- `services/sse_bridge_service/README.md`
- `services/authentication_service/README.md`
- `k8s/README.md`

### Acceptance criteria
- Docs no longer claim `sse_bridge_service` proxies backend APIs directly.
- Docs describe ingress â†’ Envoy â†’ services.

---

## Appendix A â€” Concrete file inventory by area

### SSE bridge (`sse_bridge_service`)
- `services/sse_bridge_service/src/main.py`
- `services/sse_bridge_service/src/routers/sse_router.py`

### Auth service
- `services/authentication_service/src/routers/token_router.py`
- `services/authentication_service/src/services/token_service.py`

### Shared auth utilities
- `services/shared/src/utils/jwt_utils.py`
- `services/shared/src/utils/local_auth.py`

### K8s
- `k8s/base/ingress.yaml`
- `k8s/base/network-policy.yaml`
- `k8s/basemap.yaml`
- `k8s/base/secret-provider.yaml`
- `k8s/base/services/sse-bridge-service.yaml`
- `k8s/base/services/*` (add `authentication-service.yaml`, add `envoy-gateway.yaml`)

### Infra
- `infra/main.bicep`
- `infra/modules/secrets.bicep`


