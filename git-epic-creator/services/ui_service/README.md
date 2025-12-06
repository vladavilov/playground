# UI Service

Serves a modern Tailwind HTML/JS UI with integrated Azure AD authentication, GitLab OAuth, and AI-powered task generation capabilities. Provides SSE endpoints bridging Redis pubsub channels to the browser for real-time updates.

## Architecture

The UI Service implements a modern, secure authentication architecture with comprehensive task management capabilities:

```mermaid
graph TB
    Browser[Browser]
    UI[UI Service]
    Azure[Azure AD]
    GitLab[GitLab OAuth]
    Auth[Authentication<br/>Service]
    Redis[Redis Pubsub<br/>Real-time Events]
    Project[Project Service]
    Workflow[Workflow Service]
    Tasks[AI Tasks Service]

    Browser -->|SSO| UI
    UI -->|OAuth| Azure
    Azure -->|Tokens| UI
    UI -->|SSE/Real-time| Browser
    
    UI -->|Mint S2S| Auth
    Auth -->|S2S JWT| UI
    
    UI -->|S2S JWT| Project
    UI -->|S2S JWT| Workflow
    UI -->|S2S JWT| Tasks
    
    UI <-->|OAuth| GitLab
    Redis -->|Events| UI
    
    style UI fill:#e3f2fd,stroke:#1976d2,stroke-width:2px
    style Browser fill:#f5f5f5,stroke:#757575,stroke-width:2px
    style Azure fill:#fff3e0,stroke:#f57c00,stroke-width:2px
    style GitLab fill:#fff3e0,stroke:#f57c00,stroke-width:2px
    style Auth fill:#f3e5f5,stroke:#7b1fa2,stroke-width:2px
    style Redis fill:#fce4ec,stroke:#c2185b,stroke-width:2px
    style Project fill:#e8f5e9,stroke:#388e3c,stroke-width:2px
    style Workflow fill:#e8f5e9,stroke:#388e3c,stroke-width:2px
    style Tasks fill:#e8f5e9,stroke:#388e3c,stroke-width:2px
```

### Core Components

1. **Azure SSO** - MSAL Python-based Microsoft identity platform integration
2. **Centralized S2S Authentication** - Delegates S2S JWT minting to `authentication-service`
3. **GitLab Integration** - Proxies OAuth and API calls to gitlab-client-service
4. **Task Generation UI** - AI-powered backlog creation with real-time agent thought streaming
5. **SSE Bridge** - Real-time event streaming from Redis pubsub to browser

## Features

### Authentication & Authorization
- **MSAL Python Integration**: Official Microsoft library for Azure AD authentication
- **Redis-backed Token Cache**: Distributed, scalable session management (key: `msal_cache:{session_id}`)
- **Centralized S2S Authentication**: Delegates token minting to `authentication-service` for consistency
- **Enhanced S2S Tokens**: Comprehensive claims including oid, tid, aud, iss, roles (minted by auth-service)
- **Automatic Token Refresh**: Proactive refresh for Azure tokens via MSAL
- **Conditional Access Support**: Proper handling of MFA and CA challenges
- **GitLab Integration (Proxied)**: All GitLab OAuth and API interactions proxied to gitlab-client-service

### Task Generation Interface
- **Seamless Navigation Flow**: Requirements gathering → AI task generation → backlog review
- **Real-time Agent Thought Streaming**: Live visualization of AI reasoning and processing
- **Connection Status Dashboard**: Real-time monitoring of Auth, AI Tasks Service, and SSE connection
- **Interactive Backlog Visualization**: Epic cards with expandable tasks, acceptance criteria, dependencies
- **Duplicate Detection**: Yellow warning badges for similar GitLab items with similarity percentages
- **Task Editor Modal**: Full editing capabilities for epics, tasks, and GitLab link associations
- **Quality Score Display**: Visual indicators (0-100%) based on coverage, specificity, feasibility

### UI Components
- **Visual Authentication UI**: Green/red indicator, user profile display, logout button
- **Split-View Layout**: Left panel for agent workspace, right panel for live backlog
- **Collapsible Thinking Boxes**: Show/hide AI reasoning with markdown rendering
- **Responsive Design**: Tailwind CSS with modern gradients, shadows, and animations
- **Accessibility**: Semantic HTML5, ARIA roles, keyboard navigation, WCAG AA compliant

## Configuration

### Session Management
- `SESSION_SECRET_KEY` - Secret key for session cookie HMAC (required in production)
- `SESSION_COOKIE_NAME` - Session cookie name (default: `ui_session`)
- `SESSION_MAX_AGE` - Session max age in seconds (default: 14 days)
- `SESSION_SAME_SITE` - Session SameSite policy (default: `lax`)
- `ALLOW_INSECURE_SESSION` - Allow HTTP cookies for development/mock auth (default: `false`)

### Redis
- `REDIS_URL` - Redis connection URL (required, e.g., `redis://redis:6379`)

### Azure AD (via shared configuration)
- `AZURE_TENANT_ID` - Azure AD tenant ID (required)
- `AZURE_CLIENT_ID` - Azure AD application client ID (required)
- `AZURE_CLIENT_SECRET` - Azure AD application client secret (required)
- `AZURE_SCOPE_DESCRIPTION` - Scope description (default: `user_impersonation`)
- `AZURE_AD_AUTHORITY` - Azure AD authority URL (default: `https://login.microsoftonline.com`)


### S2S Authentication
- `LOCAL_JWT_SECRET` - Shared secret for S2S JWT tokens (required)
- `LOCAL_JWT_ALG` - JWT algorithm (default: `HS256`)

### MSAL Logging
- `MSAL_LOG_LEVEL` - MSAL log level: DEBUG, INFO, WARNING, ERROR (default: `INFO`)

### Downstream Services
- `AUTH_SERVICE_URL` - Centralized authentication service URL (required, default: `http://authentication-service:8020`)
- `PROJECT_MANAGEMENT_SERVICE_URL` - Project management service URL
- `AI_WORKFLOW_SERVICE_URL` - AI workflow service URL
- `AI_TASKS_SERVICE_URL` - AI tasks/backlog generation service URL (default: `http://localhost:8003`)
- `GITLAB_CLIENT_SERVICE_URL` - GitLab client service URL for OAuth and API proxying

## API Endpoints

### Azure SSO
- `GET /auth/login?redirect_uri=<url>` - Initiate Azure AD login flow
  - `redirect_uri` (optional): URL to redirect to after successful authentication (validated for security)
  - If not provided, redirects to `/projects.html` by default
- `GET /auth/callback` - Azure AD callback handler
- `GET /auth/me` - Get current user authentication status
- `POST /auth/logout` - Logout and clear tokens

### GitLab Integration (Proxied)

ui-service acts as a **transparent proxy** for all GitLab OAuth and API interactions. All GitLab logic is handled by gitlab-client-service.

**Proxy Endpoints:**
- `GET /auth/gitlab/authorize?redirect_uri=<url>` - Proxy GitLab OAuth authorization
- `GET /auth/gitlab/callback` - Proxy GitLab OAuth callback
- `GET /auth/gitlab/status` - Proxy GitLab connection status check
- `POST /auth/gitlab/disconnect` - Proxy GitLab disconnect request
- `GET/POST/PUT/DELETE /gitlab/*` - Proxy GitLab API requests


**For detailed GitLab OAuth implementation, see:** [GitLab Client Service README](../gitlab_client_service/README.md#authentication--authorization)

### API Proxying
- `GET/POST/PUT/DELETE /project/*` - Proxy to project management service
- `GET/POST/PUT/DELETE /workflow/*` - Proxy to AI workflow service
- `GET/POST/PUT/DELETE /tasks/*` - Proxy to AI tasks service
- `GET /config` - Get UI configuration

### Server-Sent Events
- `GET /events` - SSE stream for project progress updates
  - Channels: `ui:project_progress`, `ui:ai_workflow_progress`, `ui:ai_tasks_progress`, `ui:retrieval_progress`

### Static UI Pages
- `GET /projects.html` - Main project selection page
- `GET /requirements.html?project_id=<id>` - Requirements gathering interface (requires project_id query parameter)
- `GET /tasks.html?project_id=<id>&from_requirements=<bool>` - AI task generation interface (requires project_id, from_requirements=true triggers auto-load from sessionStorage)

## User Experience Flows

### 1. Authentication Flow

```mermaid
sequenceDiagram
    participant User
    participant Browser
    participant UI as UI Service
    participant MSAL
    participant Redis
    participant Azure as Azure AD

    User->>Browser: Access /auth/login
    Browser->>UI: GET /auth/login
    UI->>MSAL: Generate auth URL with PKCE
    MSAL-->>UI: Authorization URL
    UI-->>Browser: Redirect to Azure AD
    Browser->>Azure: Authenticate user
    
    alt MFA Required
        Azure->>User: Prompt for MFA
        User->>Azure: Provide MFA
    end
    
    Azure-->>Browser: Redirect with auth code
    Browser->>UI: GET /auth/callback?code=xxx
    UI->>MSAL: acquire_token_by_authorization_code
    MSAL->>Azure: Exchange code for tokens
    Azure-->>MSAL: Access & Refresh tokens
    MSAL->>Redis: Store tokens in cache
    UI->>UI: Extract claims & create session
    UI-->>Browser: Set session cookie
    
    Note over UI,Redis: Token Refresh (automatic)
    UI->>MSAL: acquire_token_silent
    MSAL->>Redis: Check cached tokens
    alt Token expired
        MSAL->>Azure: Refresh token
        Azure-->>MSAL: New tokens
        MSAL->>Redis: Update cache
    end
```

**Key Steps**:

1. **User Login**: User accesses `/auth/login?redirect_uri=<url>`, optional redirect URI is validated and encoded in OAuth state parameter (base64-encoded JSON with CSRF token + redirect URL), MSAL generates authorization URL with PKCE, user redirects to Azure AD
2. **Callback Processing**: Azure AD redirects to `/auth/callback` with authorization code and state parameter, state is decoded to extract CSRF token and redirect URI, MSAL exchanges code for tokens, tokens stored in Redis-backed cache, user claims extracted and stored in session, user redirected to original page (from state) or default `/projects.html`
3. **Token Refresh**: MSAL automatically refreshes tokens via `acquire_token_silent`, refresh occurs when cached token is expired or missing
4. **Conditional Access**: Detects `interaction_required` errors, stores claims challenge for re-authentication, prompts user for MFA when required
5. **Security**: Redirect URI validation prevents open redirect attacks (rejects external URLs, javascript: URIs, etc.), state parameter provides both CSRF protection and return URL preservation

### 2. Unified Token Exchange Flow

UI Service uses the **same authentication mechanism** as MCP Server (VS Code Copilot). After Azure AD OAuth, both exchange Azure AD tokens for LOCAL JWTs via the same `/auth/exchange` endpoint:

```mermaid
sequenceDiagram
    participant Browser
    participant UI as UI Service
    participant Auth as Authentication<br/>Service
    participant Azure as Azure AD
    participant DS as Downstream Service

    Note over Browser,Azure: Same flow as MCP Server (VS Code)
    Browser->>UI: Access /auth/login
    UI->>Azure: OAuth redirect
    Azure->>Browser: Login prompt
    Browser->>Azure: Credentials
    Azure->>UI: Authorization code
    UI->>Azure: Exchange code (MSAL)
    Azure-->>UI: Azure AD access token
    
    UI->>Auth: POST /auth/exchange<br/>{access_token: azure_token}
    Auth->>Azure: Validate token (JWKS)
    Auth->>Auth: Mint LOCAL JWT
    Auth-->>UI: {access_token: LOCAL_JWT, user_id, roles}
    
    UI->>UI: Store LOCAL JWT in session
    
    Note over Browser,DS: Subsequent API calls
    Browser->>UI: API Request + Session Cookie
    UI->>UI: Get LOCAL JWT from session<br/>(refresh via MSAL if expired)
    UI->>DS: Request + Bearer {LOCAL_JWT}
    DS->>DS: Validate LOCAL JWT
    DS-->>UI: Response
    UI-->>Browser: Response
```

**Key Points**:

1. **Unified Mechanism**: UI Service and MCP Server use the **same `/auth/exchange` endpoint**
2. **Same Token Format**: Both get the same LOCAL JWT format (oid, tid, roles, iss, aud)
3. **Secure Validation**: ALL tokens validated against Azure AD JWKS before exchange
4. **Token Caching**: LOCAL JWT stored in session, refreshed via MSAL silent acquisition when needed

### 3. GitLab Integration Flow

```mermaid
sequenceDiagram
    participant Browser
    participant UI as ui-service<br/>(Proxy Only)
    participant GCS as gitlab-client-service<br/>(Stateless OAuth)
    participant GitLab
    participant Redis

    Browser->>UI: Click "Connect GitLab"
    UI->>UI: Extract session_id from cookie
    UI->>GCS: Proxy with S2S JWT + session_id
    
    Note over GCS: Stateless OAuth
    GCS->>GCS: Encode state={sid, redirect, csrf}
    GCS-->>UI: 302 → GitLab OAuth
    UI-->>Browser: 302 → GitLab OAuth
    
    Browser->>GitLab: Authorize application
    GitLab-->>Browser: 302 → callback?code=xxx&state=yyy
    
    Browser->>UI: GET /auth/gitlab/callback
    UI->>GCS: Proxy callback
    GCS->>GCS: Decode state, validate CSRF
    GCS->>GitLab: fetch_access_token(code)
    GitLab-->>GCS: Access + refresh tokens
    GCS->>Redis: Store token (key: gitlab:oauth:{sid})
    GCS-->>UI: 302 → application
    UI-->>Browser: 302 → application
    
    Note over Browser,Redis: API Requests
    Browser->>UI: API request
    UI->>GCS: Proxy with S2S JWT
    GCS->>Redis: Load OAuth token by session_id
    GCS->>GitLab: API call with token
    GitLab-->>GCS: Response
    GCS-->>UI: Response
    UI-->>Browser: Response
```

**For complete OAuth implementation details, see:** [GitLab Client Service README](../gitlab_client_service/README.md#authentication--authorization)

### 4. Task Generation Workflow

```mermaid
stateDiagram-v2
    [*] --> ProjectSelection: User opens index.html
    ProjectSelection --> RequirementsGathering: Click "Open Chat"
    RequirementsGathering --> Refinement: User describes needs
    Refinement --> Refinement: AI asks clarifications
    Refinement --> Confirmation: Score > 70%
    Confirmation --> TaskGeneration: Click "Confirm & Create Tasks"
    
    TaskGeneration --> AIProcessing: Navigate to tasks.html
    AIProcessing --> BacklogReview: Backlog generated
    
    state AIProcessing {
        [*] --> AnalyzingRequirements
        AnalyzingRequirements --> RetrievingContext
        RetrievingContext --> FetchingBacklog
        FetchingBacklog --> DraftingBacklog
        DraftingBacklog --> MappingDuplicates
        MappingDuplicates --> Evaluating
        Evaluating --> [*]
    }
    
    BacklogReview --> EditDetails: Click "Edit All"
    EditDetails --> BacklogReview: Save changes
    BacklogReview --> IterateWithAI: Chat with AI
    IterateWithAI --> TaskGeneration: Refine request
    BacklogReview --> GitLabSubmit: Click "Submit to GitLab"
    GitLabSubmit --> [*]: Epics & Issues created
```

**Typical User Journey**:

1. **Start**: User opens `projects.html` → selects project
2. **Requirements**: Clicks "Requirements" → navigates to `requirements.html?project_id=X` to discuss needs
3. **Refinement**: AI generates requirements, user answers clarification questions
4. **Confirmation**: Score reaches >70% → "✓ Confirm & Create Tasks" button appears
5. **Task Generation**: Click stores requirements in sessionStorage and navigates to `tasks.html?project_id=X&from_requirements=true`
6. **Auto-Submit**: Tasks page retrieves requirements from sessionStorage and auto-sends them as first message to AI
7. **AI Processing**: Thinking box appears showing agent reasoning, status updates in real-time via SSE, backlog materializes on the right panel
8. **Review**: User reviews epics, tasks, duplicate alerts
9. **Edit**: Click "✎ Edit All" to modify details
10. **Iterate**: User can chat with AI to refine backlog
11. **Submit**: (Future) "Save & Submit to GitLab" pushes to repository

**Note**: `prompt_id` is service-specific and not shared between requirements and tasks services. Requirements are passed as formatted text via sessionStorage instead.

## Technical Implementation

### Frontend Structure

The frontend follows a modern, modular architecture with clear separation of concerns:

```mermaid
graph TB
    subgraph "View Layer"
        Pages[pages/<br/>HTML files]
    end
    
    subgraph "Core Framework"
        BaseCtrl[base-controller.js<br/>Chat screens]
        BaseEdit[base-editor.js<br/>Multi-mode editing]
        BaseRend[base-renderer.js<br/>Consistent UI]
    end
    
    subgraph "Application Layer"
        Controllers[controllers/<br/>Page logic]
        Editors[editors/<br/>Editing impl]
        Renderers[renderers/<br/>Display impl]
    end
    
    subgraph "Infrastructure"
        Services[services/<br/>API client]
        Components[components/<br/>UI widgets]
        Utils[utils/<br/>Helpers]
    end
    
    Pages --> Controllers
    Controllers --> BaseCtrl
    Controllers --> Editors
    Controllers --> Renderers
    
    Editors --> BaseEdit
    Renderers --> BaseRend
    
    BaseCtrl --> Components
    BaseCtrl --> Utils
    BaseCtrl --> Services
    
    Components --> Utils
    Editors --> Utils
    Renderers --> Utils
    
    style Pages fill:#e3f2fd,stroke:#1976d2,stroke-width:2px
    style BaseCtrl fill:#f3e5f5,stroke:#7b1fa2,stroke-width:3px
    style BaseEdit fill:#f3e5f5,stroke:#7b1fa2,stroke-width:3px
    style BaseRend fill:#f3e5f5,stroke:#7b1fa2,stroke-width:3px
    style Controllers fill:#fce4ec,stroke:#c2185b,stroke-width:2px
    style Components fill:#fff9c4,stroke:#f9a825,stroke-width:2px
    style Utils fill:#e8f5e9,stroke:#388e3c,stroke-width:2px
```

**Modular File Structure**:
```
static/
├── pages/                          # HTML pages (view layer)
│   ├── projects.html               # Project selection and console
│   ├── requirements.html           # Requirements gathering interface
│   └── tasks.html                  # Task generation interface
│
├── js/                             # JavaScript modules
│   ├── core/                       # Framework base classes
│   │   ├── base-controller.js      # Abstract controller for chat screens (~360 lines)
│   │   ├── base-editor.js          # Multi-mode editor (inline/focus/fullscreen) (~600 lines)
│   │   └── base-renderer.js        # Consistent UI rendering (~244 lines)
│   │
│   ├── controllers/                # Page controllers (application layer)
│   │   ├── projects-controller.js  # Project CRUD, uploads, embeddings (~914 lines)
│   │   ├── requirements-controller.js  # Requirements generation (~264 lines)
│   │   └── tasks-controller.js     # Task generation, GitLab submit (~390 lines)
│   │
│   ├── editors/                    # Specialized editor implementations
│   │   ├── requirements-editor.js  # Requirements editing (~237 lines)
│   │   └── tasks-editor.js         # Epic/task editing (~227 lines)
│   │
│   ├── renderers/                  # Specialized renderer implementations
│   │   ├── backlog-renderer.js     # Epic/task rendering (~182 lines)
│   │   └── requirements-renderer.js # Requirements rendering (~149 lines)
│   │
│   ├── services/                   # External services & API communication
│   │   └── api-client.js           # HTTP client with error handling (~232 lines)
│   │
│   ├── components/                 # Reusable UI components
│   │   ├── chat-ui.js              # Chat message rendering (~85 lines)
│   │   ├── modal-manager.js        # Modal lifecycle management (~145 lines)
│   │   ├── thinking-box-manager.js # Thinking box routing (~125 lines)
│   │   └── typewriter-box.js       # AI message typewriter effect (~380 lines)
│   │
│   └── utils/                      # Utility functions
│       ├── connections.js          # Auth, GitLab, SSE management (~285 lines)
│       ├── dom-helpers.js          # DOM utilities, XSS prevention (~60 lines)
│       ├── formatting.js           # Date/number formatting (~20 lines)
│       └── status-badges.js        # Status badge creation (~30 lines)
```

### Task Generation UI Features

#### Split-View Layout
- **Left Panel**: Agent workspace with chat/iteration
- **Right Panel**: Live backlog visualization
- Responsive design with Tailwind CSS gradients and shadows

#### Connection Status Dashboard
Real-time indicators for:
- Authentication status (🟢 Green / 🔵 Blue pulse / ⚪ Gray)
- AI Tasks Service health
- SSE/Real-time events connection

#### Agent Thought Stream Visualization
- Collapsible "thinking boxes" show AI reasoning
- Markdown rendering with syntax highlighting
- Animated pulsing indicator during active processing
- Completion badges with timing information

#### Backlog Rendering

**Epic Cards**:
- Clean, card-based layout for each epic
- Expandable task lists with:
  - Task titles and descriptions
  - Acceptance criteria (Given/When/Then format)
  - Dependencies visualization
  - Similar work item detection

**Duplicate Detection Alerts**:
- Yellow warning badges for similar GitLab items
- Shows similarity percentage and links
- Helps prevent redundant work

**Quality Score Display**:
- Visual score indicator (0-100%)
- Color-coded: Green (≥75%), Amber (50-74%), Red (<50%)

#### Task Editor Modal
- Edit epic titles and descriptions
- Modify task details inline
- Manage GitLab link associations
- Add/remove similar work items
- Clean, form-based UI with validation

#### Real-Time Progress Updates

```mermaid
sequenceDiagram
    participant Service as Backend Service<br/>(Workflow/Tasks)
    participant Redis as Redis Pubsub
    participant SSE as SSE Router<br/>(UI Service)
    participant Browser
    participant UI as UI JavaScript

    Browser->>SSE: GET /events (EventSource)
    SSE->>Redis: SUBSCRIBE ui:*_progress
    SSE-->>Browser: Connection established
    
    loop Real-time Updates
        Service->>Redis: PUBLISH ui:ai_tasks_progress<br/>{status, thought_summary, score}
        Redis->>SSE: Message received
        SSE->>SSE: Filter by project_id/prompt_id
        SSE-->>Browser: data: {...}\n\n
        Browser->>UI: onmessage event
        UI->>UI: Update thinking box
        UI->>UI: Update status badges
        UI->>UI: Update backlog display
        UI->>UI: Auto-scroll to latest
    end
    
    Note over Browser,UI: Connection Management
    Browser-xSSE: Connection lost
    UI->>UI: Retry connection (exponential backoff)
    Browser->>SSE: Reconnect with Last-Event-ID
```

**SSE Event Subscription** - Listens to channels with event types:

Channels:
- `ui:project_progress` - General project updates (document processing, RAG ingestion, etc.)
- `ui:ai_requirements_progress` - Requirements gathering
- `ui:ai_tasks_progress` - Task generation
- `ui:retrieval_progress` - Neo4j GraphRAG context retrieval (used by both requirements and tasks pages)

Event types:
- `analyzing_requirements`
- `retrieving_context`
- `fetching_backlog`
- `drafting_backlog`
- `mapping_duplicates`
- `evaluating`
- `needs_clarification`
- `completed`
- `error`

**Live Status Updates**:
- Status badges update in real-time
- Thought streams append to active thinking box
- Automatic scrolling to latest updates

**Project Progress Matching**:
- The `project_progress` event handler in `projects-page-controller.js` matches `msg.project_id` against `project.id` (both are Project Management Service UUIDs)
- This ensures document processing and RAG ingestion status updates are properly displayed in the project logs panel
- Upload panel shows real-time progress with percentage complete and status messages
- Error messages from failed document processing are displayed in the logs panel with specific failure details

### State Management

The refactored architecture uses a hierarchical state management approach:

**Base Controller State** (managed by `ChatBaseController`):
```javascript
{
  config, projectId, project,
  authenticated, gitlab: { status, configured },
  rtEvents: { status },
  evtSource
}
```

**Screen-Specific State** (managed by individual controllers):
```javascript
// RequirementsController
{
  ...baseState,
  currentBundle: { business_requirements, functional_requirements, ... }
}

// TasksController  
{
  ...baseState,
  backlogBundle: { epics, assumptions, risks, score },
  gitlabProjectId, promptId
}
```

**Component State** (managed by component classes):
- `ThinkingBoxManager`: Manages thinking boxes lifecycle (`boxes`, `boxesByPromptId`, `activeBox`, `pendingBox`)
- `ChatUI`: Manages chat message rendering
- `ModalManager`: Manages modal dialog state

### Markdown Rendering
- Custom renderer for headings, bold, italic, code, links
- Syntax-highlighted code blocks
- Safe HTML escaping

### Connection Management
- Automatic SSE reconnection
- Token refresh handling
- Service health monitoring

## Key Components

### MSAL Integration

```mermaid
graph TB
    subgraph "UI Service"
        App[FastAPI Application]
        MSAL[MSAL ConfidentialClient]
        Cache[SerializableTokenCache]
    end
    
    subgraph "Storage"
        Redis[("Redis<br/>msal_cache:session_id")]
    end
    
    subgraph "Azure AD"
        Authority["Azure Authority<br/>login.microsoftonline.com"]
    end
    
    App -->|Login flow| MSAL
    MSAL -->|Authorization| Authority
    Authority -->|Tokens| MSAL
    
    MSAL <-->|Serialize/Deserialize| Cache
    Cache <-->|Store/Retrieve| Redis
    
    MSAL -->|acquire_token_silent| Cache
    Cache -->|Check expiration| MSAL
    MSAL -->|Refresh if needed| Authority
    
    style MSAL fill:#e3f2fd,stroke:#1976d2,stroke-width:2px
    style Cache fill:#fff3e0,stroke:#f57c00,stroke-width:2px
    style Redis fill:#fce4ec,stroke:#c2185b,stroke-width:2px
    style Authority fill:#f3e5f5,stroke:#7b1fa2,stroke-width:2px
```

**Features**:
- Uses `msal.ConfidentialClientApplication` for Azure AD
- Redis-backed `SerializableTokenCache` for distributed token storage
- Automatic token refresh via `acquire_token_silent`
- Proper Conditional Access and MFA handling
- Session-based cache isolation (one cache per session)

### GitLab Integration Architecture

**Design:** ui-service acts as a **transparent proxy** to gitlab-client-service

```mermaid
graph LR
    Browser[Browser] -->|HTTP| UI[ui-service<br/>Proxy Layer]
    UI -->|S2S JWT| GCS[gitlab-client-service<br/>OAuth + API Logic]
    GCS -->|OAuth 2.0| GitLab[GitLab Server]
    GCS -->|Store Tokens| Redis[(Redis)]
    
    style UI fill:#e3f2fd,stroke:#1976d2,stroke-width:2px
    style GCS fill:#fff3e0,stroke:#f57c00,stroke-width:2px
    style GitLab fill:#f3e5f5,stroke:#7b1fa2,stroke-width:2px
    style Redis fill:#fce4ec,stroke:#c2185b,stroke-width:2px
```


**Proxy Implementation:**
```python
@router.api_route("/gitlab/{path:path}", methods=["GET", "POST", "PUT", "DELETE"])
async def proxy_to_gitlab_client(path: str, request: Request):
    # Add S2S JWT with session_id from user's session cookie
    s2s_token = mint_jwt(session_id=request.session["sid"])
    
    # Forward to gitlab-client-service
    target_url = f"{GITLAB_CLIENT_SERVICE_URL}/gitlab/{path}"
    return await _forward(request, target_url, auth_token=s2s_token)
```

**For complete GitLab OAuth documentation, see:** [GitLab Client Service README](../gitlab_client_service/README.md#authentication--authorization)

### S2S Authentication

```mermaid
graph LR
    subgraph "Token Types"
        AzureToken[Azure AD Token<br/>From MSAL]
        LocalJWT[LOCAL JWT Token<br/>From Auth Service]
    end
    
    subgraph "Services"
        UISession[UI Service<br/>Session Management]
        AuthSvc[Authentication<br/>Service]
        Backend[Backend Services]
    end
    
    AzureToken -->|Used for| UISession
    AzureToken -.->|NOT used| Backend
    UISession -->|Request token| AuthSvc
    AuthSvc -->|Mint JWT| LocalJWT
    LocalJWT -->|Forwarded to| Backend
    
    style AzureToken fill:#fff3e0,stroke:#f57c00,stroke-width:2px
    style LocalJWT fill:#e8f5e9,stroke:#388e3c,stroke-width:2px
    style AuthSvc fill:#f3e5f5,stroke:#7b1fa2,stroke-width:2px
    style Backend fill:#e3f2fd,stroke:#1976d2,stroke-width:2px
```

**Important Distinction**:

⚠️ **Backend services do NOT validate Azure AD tokens**. They only validate LOCAL JWT tokens signed with `LOCAL_JWT_SECRET`.

**Two Token Types**:

1. **Azure AD Tokens** (from MSAL):
   - Used ONLY for UI service session management
   - Stored in Redis via MSAL token cache
   - Never forwarded to backend services
   - Validated by Azure AD

2. **LOCAL JWT Tokens** (from authentication-service):
   - Centralized minting by `authentication-service` at `/auth/mint`
   - Used for service-to-service (S2S) authentication
   - Signed with `LOCAL_JWT_SECRET` (shared secret)
   - Forwarded to all backend services (project, workflow, tasks)
   - Validated by backend services using shared secret
   - Short-lived (10 minutes)
   - Enhanced claims: `oid`, `tid`, `aud`, `iss`, `roles`, `preferred_username`
   - In-memory caching (60s TTL) in UI service for performance

**For Testing/Direct Backend Access**:

When calling backend services directly (e.g., in E2E tests), you can use the `authentication-service` to mint tokens:

```bash
# Using authentication-service mint endpoint
curl -X POST http://localhost:8020/auth/mint \
  -H "Content-Type: application/json" \
  -d '{
    "oid": "test-user-id",
    "preferred_username": "test.user@example.com",
    "roles": ["Admin", "User"],
    "target_service": "backend-services"
  }'
```

Or create LOCAL JWT tokens directly for testing:

```python
import jwt
import time

def create_local_jwt_token(oid: str = None, roles: list = None, username: str = None) -> str:
    """Create LOCAL JWT token for backend service authentication."""
    secret = os.getenv("LOCAL_JWT_SECRET", "test-secret-key")
    now = int(time.time())
    
    claims = {
        "oid": oid or str(uuid.uuid4()),
        "preferred_username": username or "test.user@example.com",
        "roles": roles or ["Admin", "User"],
        "iss": "authentication-service",  # Match auth-service issuer
        "aud": "backend-services",
        "iat": now,
        "nbf": now,
        "exp": now + 3600,  # 1 hour validity
    }
    
    return jwt.encode(claims, secret, algorithm="HS256")

# Usage
headers = {"Authorization": f"Bearer {create_local_jwt_token()}"}
response = requests.get("http://project-service:8000/api/projects", headers=headers)
```

**Backend Services Using LOCAL JWT**:
- `project_management_service`
- `ai_requirements_service`
- `ai_tasks_service`
- `neo4j_retrieval_service`
- `neo4j_maintenance_service`
- `db_init_service`

### SSE Router
- Bridges Redis pubsub to browser-compatible SSE
- Supports multiple channels with message filtering
- Automatic cleanup on connection close

### Proxy Router

```mermaid
flowchart TD
    Browser[Browser Request]
    Proxy[Proxy Router]
    Session[Session Validation]
    AuthSvc[Authentication<br/>Service]
    Cache[Token Cache<br/>60s TTL]
    
    Project[Project Service<br/>:8001]
    Workflow[Workflow Service<br/>:8002]
    Tasks[AI Tasks Service<br/>:8003]
    GitLab[GitLab API]
    
    Browser --> Proxy
    Proxy --> Session
    Session -->|Valid| Proxy
    
    Proxy -->|/project/*| Cache
    Proxy -->|/workflow/*| Cache
    Proxy -->|/tasks/*| Cache
    Proxy -->|/gitlab/*| GitLabToken[Get GitLab Token]
    
    Cache -->|Hit| Forward1[Forward Request]
    Cache -->|Miss| AuthSvc
    AuthSvc -->|POST /auth/mint| Mint[S2S JWT<br/>aud, oid, tid, roles]
    Mint --> Cache
    Cache --> Forward1
    
    GitLabToken --> Forward2[Forward Request]
    
    Forward1 -->|/project/*| Project
    Forward1 -->|/workflow/*| Workflow
    Forward1 -->|/tasks/*| Tasks
    Forward2 -->|/gitlab/*| GitLab
    
    Project --> Response[Response]
    Workflow --> Response
    Tasks --> Response
    GitLab --> Response
    Response --> Proxy
    Proxy --> Browser
    
    style Proxy fill:#e3f2fd,stroke:#1976d2,stroke-width:3px
    style AuthSvc fill:#f3e5f5,stroke:#7b1fa2,stroke-width:2px
    style Cache fill:#fce4ec,stroke:#c2185b,stroke-width:2px
    style Project fill:#e8f5e9,stroke:#388e3c,stroke-width:2px
    style Workflow fill:#e8f5e9,stroke:#388e3c,stroke-width:2px
    style Tasks fill:#e8f5e9,stroke:#388e3c,stroke-width:2px
    style GitLab fill:#fff3e0,stroke:#f57c00,stroke-width:2px
```

**Features**:
- Transparent proxying to downstream services
- Centralized S2S token minting via `authentication-service`
- Token caching (60s TTL) to reduce authentication-service calls
- Header preservation and error handling
- Service-specific routing (project, workflow, tasks, gitlab)
- Graceful handling of authentication-service unavailability (503 response)

## Integration with AI Tasks Service

### Request Payload
```json
POST /tasks/tasks/generate
{
  "project_id": "uuid",
  "prompt_id": "uuid (optional)",
  "message": "User message",
  "options": {
    "top_k": 2,
    "similarity_threshold": 0.7,
    "max_iters": 3
  }
}
```

### Response Bundle
```json
{
  "prompt_id": "uuid",
  "project_id": "uuid",
  "epics": [
    {
      "id": "epic-1",
      "title": "...",
      "description": "...",
      "tasks": [...],
      "similar": [...]
    }
  ],
  "assumptions": [...],
  "risks": [...],
  "score": 0.82,
  "coverage_components": {...}
}
```

### SSE Progress Events
```json
{
  "message_type": "ai_tasks_progress",
  "project_id": "uuid",
  "prompt_id": "uuid",
  "status": "drafting_backlog",
  "thought_summary": "...",
  "details_md": "...",
  "score": 0.75,
  "timestamp": "2025-10-02T..."
}
```

## Development

### Run Locally

```bash
cd services/ui_service
pip install -e .[dev]
python -m src.main
```

### Run Tests

```bash
pip install -e .[dev]
pytest
```

### Environment Setup

1. Set required environment variables (see Configuration section)
2. Ensure Redis is running
3. Ensure downstream services are accessible

### Development Mode
For local development with mock authentication:
```bash
export ALLOW_INSECURE_SESSION=true
export AZURE_AD_AUTHORITY=http://host.docker.internal:8005
```

## Deployment

### Docker

Multi-stage build for optimization:
```bash
docker build -t ui-service .
docker run -p 8000:8000 --env-file .env ui-service
```

### Health Check
- Endpoint: `GET /health`
- Interval: 30s
- Timeout: 30s
- Start period: 5s

### Docker Compose

```mermaid
graph TB
    subgraph "External"
        Browser[Browser :80]
        Azure[Azure AD]
        GitLabExt[GitLab Server]
    end
    
    subgraph "Docker Network"
        UI[ui-service :8000]
        Redis[(redis :6379)]
        Project[project-service :8001]
        Workflow[workflow-service :8002]
        Tasks[ai-tasks-service :8003]
        DB[(PostgreSQL<br/>Vector DB)]
    end
    
    Browser -->|HTTP/SSE| UI
    UI -->|OAuth| Azure
    UI -->|OAuth| GitLabExt
    
    UI -->|Pubsub| Redis
    UI -->|S2S JWT| Project
    UI -->|S2S JWT| Workflow
    UI -->|S2S JWT| Tasks
    
    Project --> Redis
    Workflow --> Redis
    Tasks --> Redis
    
    Project --> DB
    Workflow --> DB
    Tasks --> DB
    
    style UI fill:#e3f2fd,stroke:#1976d2,stroke-width:3px
    style Redis fill:#fce4ec,stroke:#c2185b,stroke-width:2px
    style Project fill:#e8f5e9,stroke:#388e3c,stroke-width:2px
    style Workflow fill:#e8f5e9,stroke:#388e3c,stroke-width:2px
    style Tasks fill:#e8f5e9,stroke:#388e3c,stroke-width:2px
    style DB fill:#fff3e0,stroke:#f57c00,stroke-width:2px
```

**Configuration**:
```yaml
ui-service:
  build: ./services/ui_service
  ports:
    - "8000:8000"
  environment:
    - REDIS_URL=redis://redis:6379
    - AUTH_SERVICE_URL=http://authentication-service:8020
    - PROJECT_MANAGEMENT_SERVICE_URL=http://project-service:8000
    - AI_WORKFLOW_SERVICE_URL=http://workflow-service:8000
    - AI_TASKS_SERVICE_URL=http://ai-tasks-service:8000
  depends_on:
    - redis
    - authentication-service
    - project-service
    - workflow-service
    - ai-tasks-service
```

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
   - Tokens properly revoked on logout
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
   - Detects and handles OAuth-specific errors
   - User-friendly error messages
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

## UI/UX Design Principles

### Visual Hierarchy
- Clear sectioning with borders and backgrounds
- Typography scale (xl → lg → base → sm → xs)
- Color-coded status and categories

### Feedback & Affordance
- Hover effects on interactive elements
- Loading overlays with spinners
- Disabled states with reduced opacity
- Animated connection indicators

### Progressive Disclosure
- Collapsible thinking boxes (hide complexity)
- Expandable epic cards
- Modal-based detailed editing

### Real-Time Transparency
- Live agent thought streams
- Connection status always visible
- Progress indicators for long operations

### Consistency
- Reuses color palette across all pages
- Consistent button styles and interactions
- Unified status badge rendering

### Accessibility
- Semantic HTML5 elements
- ARIA roles (`role="log"`, `aria-live="polite"`)
- Keyboard navigation support
- Color contrast WCAG AA compliant

## Code Quality

### Principles Followed
- **DRY**: Refactored architecture eliminates ~60% code duplication through base classes and shared utilities
- **SOLID**: 
  - **Single Responsibility**: Controllers coordinate, renderers display, utilities handle isolated concerns
  - **Open/Closed**: Screens extend `ChatBaseController` without modifying base code
  - **Liskov Substitution**: All screen controllers follow base class contracts
  - **Interface Segregation**: Focused classes (ThinkingBoxManager, ChatUI, ModalManager)
  - **Dependency Inversion**: Screens depend on abstractions (`ChatBaseController`)
- **Separation of Concerns**: 
  - Controllers handle coordination and business logic
  - Renderers handle UI presentation only
  - Base classes provide common infrastructure
  - Utilities provide reusable components
- **Error Handling**: Centralized error handling in base classes with user-friendly messages
- **Documentation**: Comprehensive JSDoc comments, clear function names, architecture documentation

### Refactored Architecture Benefits
- **Code Reduction**: Requirements.js (686→420 lines, -39%), Tasks.js (828→530 lines, -36%)
- **Maintainability**: Bug fixes in base classes automatically propagate to all screens
- **Extensibility**: New chat screens can be created in <2 hours by extending `ChatBaseController`
- **Testability**: Base classes and utilities can be unit tested independently
- **Consistency**: Shared UI components ensure consistent behavior across screens

### Performance Optimizations
- Debounced scrolling
- Efficient DOM updates (innerHTML for batch rendering)
- Event delegation for dynamic content
- Cached selectors where appropriate
- S2S token caching (60s TTL)

## GitLab Integration Details

**Note:** ui-service does **NOT** handle GitLab integration directly. All GitLab OAuth and API operations are implemented in **gitlab-client-service**.

**Proxy Pattern:**
- ui-service transparently proxies all `/auth/gitlab/*` and `/gitlab/*` requests
- Adds S2S JWT authentication with session_id from user's session
- gitlab-client-service handles all OAuth logic and API calls

**For complete GitLab integration documentation, including:**
- Stateless OAuth flow implementation
- Token management and refresh
- API access patterns
- Security considerations

**See:** [GitLab Client Service README - Authentication & Authorization](../gitlab_client_service/README.md#authentication--authorization)

## Troubleshooting

### Connection Status Shows Red
- Check if services are running: `docker-compose ps`
- Verify environment variables
- Check service logs: `docker-compose logs <service-name>`

### "Not authenticated" Error
- Session expired - refresh page to re-login
- Check auth service logs

### Tasks Not Generating
- Open browser console (F12) for detailed errors
- Verify project_id in URL: `requirements.html?project_id=xxx` or `tasks.html?project_id=xxx`
- Check Redis is running

### Thinking Box Stuck on "Thinking..."
- SSE connection may have dropped
- Refresh page to reconnect
- Check network tab for `/events` connection

### MSAL Errors
Enable MSAL debug logging:
```bash
export MSAL_LOG_LEVEL=DEBUG
```

## References

### Microsoft Documentation
- [MSAL Python Documentation](https://learn.microsoft.com/en-us/entra/msal/python/)
- [Microsoft Identity Platform](https://learn.microsoft.com/en-us/entra/identity-platform/)

### OAuth & Security
- [OAuth 2.0 PKCE](https://oauth.net/2/pkce/)
- [GitLab Client Service - OAuth Documentation](../gitlab_client_service/README.md#authentication--authorization) - Complete GitLab OAuth implementation (stateless design)

### UI/UX
- [Tailwind CSS Documentation](https://tailwindcss.com/docs)
- [WCAG 2.1 Guidelines](https://www.w3.org/WAI/WCAG21/quickref/)

---

**Production-ready UI service with comprehensive authentication, real-time updates, and AI-powered task generation capabilities.** 🚀