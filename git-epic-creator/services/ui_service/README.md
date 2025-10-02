# UI Service

Serves a Tailwind HTML/JS UI with integrated Azure AD authentication and GitLab OAuth. Exposes an SSE endpoint `/events` that bridges Redis pubsub channel `ui:project_progress` to the browser.

## Architecture

The UI Service implements a modern, secure authentication architecture with clear separation of concerns:

1. **Azure SSO** - MSAL Python-based Microsoft identity platform integration
2. **Internal S2S Authentication** - Enhanced JWT tokens with comprehensive claims
3. **GitLab Integration** - Dedicated OAuth flow with automatic token refresh and revocation

## Features

- **MSAL Python Integration**: Official Microsoft library for Azure AD authentication
- **Redis-backed Token Cache**: Distributed, scalable session management (key pattern: `msal_cache:{session_id}`)
- **Enhanced S2S Tokens**: Comprehensive claims including oid, tid, aud, iss, roles
- **Automatic Token Refresh**: Proactive refresh for both Azure and GitLab tokens (5-minute threshold)
- **Conditional Access Support**: Proper handling of MFA and CA challenges
- **GitLab Token Management**: Automatic refresh, validation via `/oauth/token/info`, and revocation via `/oauth/revoke`
- **Separated Concerns**: Dedicated routers for Azure SSO, GitLab OAuth, and proxying
- **Visual Authentication UI**: Green/red indicator, user profile display, logout button

## Configuration

Environment variables:

### Session Management
- `SESSION_SECRET_KEY` - Secret key for session cookie HMAC (required in production)
- `SESSION_COOKIE_NAME` - Session cookie name (default: `ui_session`)
- `SESSION_MAX_AGE` - Session max age in seconds (default: 14 days)
- `SESSION_SAME_SITE` - Session SameSite policy (default: `lax`)
- `ALLOW_INSECURE_SESSION` - Allow HTTP cookies for development/mock auth (default: `false`, set `true` for local dev)

### Redis
- `REDIS_URL` - Redis connection URL (required, e.g., `redis://redis:6379`)

### Azure AD (via shared configuration)
- `AZURE_TENANT_ID` - Azure AD tenant ID (required)
- `AZURE_CLIENT_ID` - Azure AD application client ID (required)
- `AZURE_CLIENT_SECRET` - Azure AD application client secret (required)
- `AZURE_SCOPE_DESCRIPTION` - Scope description (default: `user_impersonation`)
- `AZURE_AD_AUTHORITY` - Azure AD authority URL (default: `https://login.microsoftonline.com`, use `http://host.docker.internal:8005` for mock)

### GitLab OAuth
- `GITLAB_BASE_URL` - Base URL of GitLab instance
- `GITLAB_OAUTH_CLIENT_ID` - GitLab OAuth client ID
- `GITLAB_OAUTH_CLIENT_SECRET` - GitLab OAuth client secret
- `GITLAB_OAUTH_REDIRECT_URI` - GitLab OAuth redirect URI
- `GITLAB_OAUTH_SCOPES` - Space/comma separated scopes (default: `read_api`)
- `GITLAB_VERIFY_SSL` - Verify TLS for GitLab calls (default: `true`)

### S2S Authentication
- `LOCAL_JWT_SECRET` - Shared secret for S2S JWT tokens (required)
- `LOCAL_JWT_ALG` - JWT algorithm (default: `HS256`)

### MSAL Logging
- `MSAL_LOG_LEVEL` - MSAL log level: DEBUG, INFO, WARNING, ERROR (default: `INFO`)

### Downstream Services
- `PROJECT_MANAGEMENT_SERVICE_URL` - Project management service URL
- `AI_WORKFLOW_SERVICE_URL` - AI workflow service URL

## API Endpoints

### Azure SSO
- `GET /auth/login` - Initiate Azure AD login flow
- `GET /auth/callback` - Azure AD callback handler
- `GET /auth/me` - Get current user authentication status
- `POST /auth/logout` - Logout and clear tokens

### GitLab Integration
- `GET /auth/gitlab/authorize` - Initiate GitLab OAuth flow
- `GET /auth/gitlab/callback` - GitLab OAuth callback handler
- `GET /auth/gitlab/status` - Check GitLab connection status
- `POST /auth/gitlab/disconnect` - Disconnect GitLab integration

### API Proxying
- `GET/POST/PUT/DELETE /project/*` - Proxy to project management service
- `GET/POST/PUT/DELETE /workflow/*` - Proxy to AI workflow service
- `GET /config` - Get UI configuration

### Server-Sent Events
- `GET /events` - SSE stream for project progress updates

### UI Components
- Authentication indicator (green when authenticated, red when not)
- User profile display (shows username from `preferred_username` claim)
- Logout button (visible only when authenticated)
- Connection status (SSE stream status)

## Authentication Flow

### Azure SSO Flow

1. **User Login**:
   - User accesses `/auth/login`
   - MSAL generates authorization URL with PKCE
   - User redirects to Azure AD for authentication

2. **Callback Processing**:
   - Azure AD redirects to `/auth/callback` with authorization code
   - MSAL exchanges code for tokens using `acquire_token_by_authorization_code`
   - Tokens stored in Redis-backed MSAL token cache
   - User claims extracted and stored in session

3. **Token Refresh**:
   - MSAL automatically refreshes tokens via `acquire_token_silent`
   - Refresh occurs when cached token is expired or missing
   - No manual refresh logic required

4. **Conditional Access**:
   - Detects `interaction_required` errors
   - Stores claims challenge for re-authentication
   - Prompts user for MFA when required

### S2S Authentication Flow

1. **Request Received**:
   - Browser sends authenticated request to UI service
   - Session validated against MSAL token cache

2. **S2S Token Minting**:
   - UI service mints short-lived JWT (10 minutes)
   - Token includes comprehensive claims:
     - `oid` - User object ID
     - `tid` - Tenant ID
     - `aud` - Target service identifier
     - `iss` - Issuer (ui-service)
     - `roles` - User roles
     - `preferred_username` - User's email/UPN

3. **Token Forwarding**:
   - S2S JWT sent in `Authorization: Bearer <token>` header
   - Downstream services validate using shared secret
   - Services extract user identity and roles for authorization

4. **Token Caching**:
   - Minted S2S tokens cached for 60 seconds
   - Reduces signing overhead for frequent requests
   - Cache invalidated on session changes

### GitLab Integration Flow

1. **Authorization**:
   - OAuth client registered once at service startup
   - User initiates `/auth/gitlab/authorize`
   - Redirects to GitLab OAuth with PKCE
   - State parameter validated for CSRF protection

2. **Token Storage**:
   - GitLab tokens stored separately in Redis (key pattern: `gitlab_token:{session_id}`)
   - Includes access token, refresh token, expiration, and scopes

3. **Automatic Refresh**:
   - Token manager checks expiration before use
   - Proactively refreshes if expiring within 5 minutes
   - Refresh failures trigger re-authentication

4. **Token Validation**:
   - Uses GitLab's `/oauth/token/info` endpoint (efficient)
   - Returns token metadata: scopes, resource owner ID, expiration

5. **Token Revocation**:
   - On disconnect, calls GitLab's `/oauth/revoke` endpoint
   - Properly revokes token on GitLab server before local cleanup
   - Ensures tokens are invalidated server-side

6. **Token Forwarding**:
   - GitLab token included in `X-GitLab-Access-Token` header
   - Only sent to GitLab-related endpoints
   - Never sent to other services

## Key Components

### MSAL Integration
- Uses `msal.ConfidentialClientApplication` for Azure AD
- Redis-backed `SerializableTokenCache` for distributed token storage
- Automatic token refresh via `acquire_token_silent`
- Proper Conditional Access and MFA handling

### GitLab OAuth (Authlib)
- Dedicated router and token manager
- OAuth client registered once at startup (not per-request)
- PKCE for enhanced security
- Separate token storage from Azure tokens

### S2S Authentication
- Short-lived JWT tokens (10 minutes)
- Enhanced claims: `oid`, `tid`, `aud`, `iss`, `roles`, `preferred_username`
- In-memory caching (60s TTL) for performance
- Shared secret validation across services

## Development

### Run Locally

```bash
cd services/ui_service
pip install -e .
python -m src.main
```

### Run Tests

```bash
pip install -e .[dev]
pytest
```

### Environment Setup

1. Copy `.env.example` to `.env` (if provided)
2. Set required environment variables (see Configuration section)
3. Ensure Redis is running
4. Ensure downstream services are accessible

## Deployment

### Docker

The service is containerized and configured for production deployment:

```dockerfile
# Multi-stage build for optimization
# See Dockerfile for details
```

### Health Check

- Endpoint: `GET /health`
- Interval: 30s
- Timeout: 30s
- Start period: 5s

### Kubernetes

See `k8s/` directory for deployment manifests.

## Security Considerations

### Development vs Production

| Feature | Development (Mock) | Production (Azure AD) |
|---------|-------------------|----------------------|
| Authority | `http://host.docker.internal:8005` | `https://login.microsoftonline.com` |
| HTTPS Only | `false` (`ALLOW_INSECURE_SESSION=true`) | `true` |
| Session Secret | Can be ephemeral | Must be persistent and strong |
| Token Validation | Mock signing (RSA) | Full Azure AD validation |

### Security Features

1. **Session Security**:
   - Session cookie is `HttpOnly` and `Secure` (in production)
   - SameSite protection enabled
   - Session secret must be strong in production (store in Azure Key Vault)

2. **Token Storage**:
   - Azure tokens stored in Redis with MSAL cache serialization
   - GitLab tokens stored separately in Redis
   - Tokens properly revoked on logout (GitLab uses `/oauth/revoke`)
   - All tokens cleared from Redis on logout

3. **S2S Authentication**:
   - Short-lived tokens (10 minutes)
   - HMAC signing with shared secret
   - Consider certificate-based auth for production

4. **CSRF Protection**:
   - State parameter validated in OAuth flows (both Azure and GitLab)
   - Session-based state verification
   - Authlib and MSAL handle state generation and validation

5. **OAuth Error Handling**:
   - Detects and handles OAuth-specific errors (`access_denied`, `invalid_grant`, etc.)
   - User-friendly error messages (no internal details exposed)
   - Proper logging for audit trail

### Production Checklist

- [ ] Set `AZURE_AD_AUTHORITY=https://login.microsoftonline.com`
- [ ] Set `ALLOW_INSECURE_SESSION=false` (or remove it)
- [ ] Use production Azure AD tenant ID and client ID
- [ ] Store `SESSION_SECRET_KEY` in secure vault (Azure Key Vault)
- [ ] Store `LOCAL_JWT_SECRET` in secure vault
- [ ] Enable HTTPS on UI service
- [ ] Configure proper CORS origins
- [ ] Set up Conditional Access policies
- [ ] Enable MFA for users
- [ ] Verify GitLab OAuth app is properly configured
- [ ] Test token refresh and revocation flows

### MSAL Errors

Enable MSAL debug logging:
```bash
export MSAL_LOG_LEVEL=DEBUG
```

## Architecture Diagrams

### Authentication Flow
```
Browser → UI Service → Azure AD
                    ↓
                  MSAL
                    ↓
            Redis Token Cache
                    ↓
            Session Storage
```

### S2S Flow
```
Browser → UI Service → Mint JWT → Downstream Service
            ↓                           ↓
      Session Claims              Validate JWT
                                        ↓
                                  Extract Claims
```

### GitLab Flow
```
Browser → UI Service → GitLab OAuth → GitLab
            ↓                              ↓
      Token Manager ←←←←←←←←←←←←←←←←←← Access Token
            ↓
     Auto-Refresh Logic
            ↓
        Forward to GitLab Client Service
```

## GitLab API Endpoints Used

The service uses the following GitLab OAuth 2.0 endpoints:

| Endpoint | Purpose | When Used |
|----------|---------|-----------|
| `/oauth/authorize` | Initial authorization | User initiates GitLab connection |
| `/oauth/token` | Token exchange & refresh | After authorization code received |
| `/oauth/revoke` | Token revocation | User disconnects GitLab |
| `/oauth/token/info` | Token validation | Validating token status (efficient) |

## References

### Microsoft Documentation
- [MSAL Python Documentation](https://learn.microsoft.com/en-us/entra/msal/python/)
- [Microsoft Identity Platform](https://learn.microsoft.com/en-us/entra/identity-platform/)

### OAuth & Security
- [OAuth 2.0 PKCE](https://oauth.net/2/pkce/)
- [GitLab OAuth 2.0 API](https://docs.gitlab.com/ee/api/oauth2.html)
- [GitLab Token Revocation](https://docs.gitlab.com/api/oauth2/#revoke-a-token)
- [GitLab Token Information](https://docs.gitlab.com/api/oauth2/#retrieve-the-token-information)