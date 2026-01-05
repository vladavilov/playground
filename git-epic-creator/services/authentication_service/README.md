# Authentication Service

Centralized authentication service that owns:

- **Browser login + session** (`/auth/login`, `/auth/callback`, `/auth/me`, `/auth/logout`)
- **Envoy external authorization** (`/authz`) used by `envoy-gateway` (`ext_authz`)
- **Azure token user discovery** (`GET /auth/azure/userinfo`) used by browser-based MCP clients
- **S2S token minting** (`POST /auth/s2s/mint`) for internal callers that still use this contract

> IMPORTANT: The legacy "Azure token -> LOCAL JWT" exchange endpoints (`/auth/exchange`, `/auth/userinfo`, `/auth/validate`)
> were removed. In the current architecture, clients send **Azure AD tokens** to **Envoy**, and Envoy injects **S2S tokens**
> when calling backend services.

## Architecture

```mermaid
flowchart TB
  Browser[Browser UI] -->|cookie session| Envoy[envoy-gateway]
  MCP[VS Code MCP client] -->|Bearer Azure token| Envoy

  Envoy -->|ext_authz: /authz| Auth[authentication-service]

  Envoy -->|/auth/* (no ext_authz)| Auth
  Envoy -->|/events| SSE[sse-bridge-service]
  Envoy -->|/project, /workflow, /tasks, /gitlab, /retrieve, /code-graph| Backend[Backend services]

  Auth -->|issues S2S token| Envoy
  Envoy -->|injects: Authorization (S2S), x-user-id (allowlist)| Backend
```

## Endpoints

### Browser session endpoints (MSAL)

- `GET /auth/login`: start Azure AD login flow (redirect)
- `GET /auth/callback`: OAuth callback (sets session cookie, redirects to app)
- `GET /auth/me`: session status
- `POST /auth/logout`: clear session

### Envoy `ext_authz`

#### `GET|POST /authz`

Envoy calls this endpoint for protected routes to:

- authenticate the caller (cookie session OR Bearer Azure token)
- authorize the request (RBAC / policy)
- mint and inject an **S2S** token as `Authorization: Bearer <svc-jwt>`
- optionally inject `x-user-id` (only for allowlisted paths)

### Azure token user discovery (OIDC-like)

#### `GET /auth/azure/userinfo`

Validates an **Azure AD access token** and returns an OIDC-like userinfo payload.

**Request Headers:**

```
Authorization: Bearer <azure_ad_access_token>
```

**Response (200 OK):**

```json
{
  "sub": "oid-from-azure-ad",
  "preferred_username": "user@example.com",
  "email": "user@example.com",
  "roles": ["User", "Admin"]
}
```

### S2S token minting (legacy contract; still supported)

#### `POST /auth/s2s/mint`

Mint a **service-bound** token for internal callers using a shared secret.

**Request Headers:**

```
X-Api-Gateway-Secret: <secret>
```

**Request Body:**

```json
{
  "aud": "internal-services"
}
```

**Response (200 OK):**

```json
{
  "access_token": "<jwt>",
  "token_type": "Bearer",
  "expires_in": 600
}
```

## Configuration

### Environment Variables

| Variable | Default | Description |
|----------|---------|-------------|
| `API_PORT` | `8020` | Service port |
| `AZURE_AD_AUTHORITY` | `https://login.microsoftonline.com` | Azure AD authority (or mock auth URL) |
| `AZURE_TENANT_ID` | - | Azure AD tenant ID |
| `AZURE_CLIENT_ID` | - | Azure AD client ID |
| `AZURE_AD_VERIFY_SSL` | `true` | SSL verification (false for mock auth in dev) |
| `SESSION_SECRET_KEY` | - | **Required** for secure sessions (unless `ALLOW_INSECURE_SESSION=true`) |
| `SESSION_COOKIE_NAME` | `ui_session` | Session cookie name |
| `SESSION_MAX_AGE` | `1209600` | Max session age (seconds) |
| `SESSION_SAME_SITE` | `lax` | Cookie SameSite |
| `ALLOW_INSECURE_SESSION` | `false` | Allow insecure dev sessions |
| `LOCAL_JWT_SECRET` | - | **Required**: signing secret for injected S2S JWTs |
| `API_GATEWAY_MINT_SECRET` | - | **Required**: shared secret for `/auth/s2s/mint` |

## Development

### Local Setup

```bash
cd services
pip install -e ./shared
pip install -e ./authentication_service

cd authentication_service
python -m src.main
```

### Docker

```bash
docker build -t authentication-service -f authentication_service/Dockerfile ./services

docker run -p 8020:8020 \
  -e LOCAL_JWT_SECRET=dev-local-jwt-secret \
  -e SESSION_SECRET_KEY=dev-session-secret \
  -e AZURE_TENANT_ID=your-tenant-id \
  -e AZURE_CLIENT_ID=your-client-id \
  authentication-service
```

## Security Notes

- `LOCAL_JWT_SECRET` must be consistent across all backend services that verify S2S tokens.
- Azure audience verification is intentionally disabled for Bearer-token mode to support MCP clients with varying `aud` values.


