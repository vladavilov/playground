## Goal

Refactor the **existing UI implementation** from the legacy UI service into a new **`frontend_service`** that uses the required stack:

- **React Router v7 (Framework Mode)**: SSR, loaders/actions, type-safe route modules
- **React 19**
- **Zustand 5**: *client-only* ephemeral state (drafts, UI toggles, SSE transport flags)
- **React Router loaders/actions**: *server state* (projects, project details, generated bundles, GitLab status)
- **Tailwind CSS 4**
- **shadcn/ui + Radix UI**
- **Framer Motion 11**

This document is an **implementation plan** designed to be **directly ingestible by an AI coding agent**: each task includes file placement, what to implement, and acceptance criteria, plus â€œhard partâ€ code snippets.

---

## Current UI inventory (what exists today)

### Screens

- **Projects page**: legacy static pages (removed)
- **Requirements page**: legacy static pages (removed)
- **Tasks page**: legacy static pages (removed)

### â€œFramework-likeâ€ custom client architecture to replace

The current UI uses a custom MVC-ish architecture:

- **Controllers**: `js/controllers/*`
- **Renderers**: `js/renderers/*`
- **Editors (inline + focus/fullscreen modal)**: `js/editors/*` built on `js/core/base-editor.js`
- **State + init + SSE wiring**: `js/core/base-page-controller.js` and `js/core/base-controller.js`
- **SSE transport**: `js/utils/connections.js::connectSSE` uses `EventSource('/events')`
- **Thinking stream UI**: `js/components/thinking-box-manager.js`, `js/components/typewriter-box.js`
- **Markdown + Mermaid runtime rendering**: `js/utils/markdown-renderer.js` (plus the inline â€œAI fix Mermaidâ€ placeholder)

### Backend gateway endpoints the UI depends on (must remain reachable)

The browser talks to the **gateway entrypoint (Envoy)**, which routes:

- **Auth / session (control plane)**
  - `GET /auth/me`
  - `GET /auth/login?redirect_uri=<...>`
  - `POST /auth/logout`
- **SSE (control plane)**
  - `GET /events` event-stream emitting event names:
    - `hello`, `project_progress`, `retrieval_progress`, `ai_requirements_progress`, `ai_tasks_progress`
- **GitLab OAuth + API (gitlab-client-service, routed by Envoy)**
  - `/auth/gitlab/*`
  - `/gitlab/*`
- **Protected APIs (microservices, routed by Envoy + enforced by ext_authz)**
  - `/project/*` → project-management-service
  - `/workflow/*` → ai_requirements_service
  - `/tasks/*` → ai_tasks_service

**Note:** there is **no `/config` endpoint**. UI configuration is local to `frontend_service` (`UI_CONFIG`).

### API calls made by the JS UI (must be replicated in the new app)

- **Projects**
  - `GET /project/projects`
  - `POST /project/projects`
  - `PUT /project/projects/:id`
  - `DELETE /project/projects/:id`
  - `POST /project/projects/:id/documents/upload` (multipart/form-data)
  - `POST /project/projects/multi/cache-embeddings?project_id=<uuid>&gitlab_project_ids=1,2,...`
- **Requirements**
  - `POST /workflow/requirements` (generate bundle; returns `RequirementsBundle`)
  - `POST /workflow/enhance` (single requirement enhancement)
- **Tasks**
  - `POST /tasks/generate` (generate backlog bundle; returns `BacklogBundle`)
  - `POST /tasks/enhance` (single epic/task enhancement)
  - `POST /gitlab/projects/apply-backlog` (submit to GitLab; supports multi-project payload)

---

## Target UI architecture (React Router v7 Framework Mode)

### High-level approach

- Create a new Node/Vite-based **`frontend_service`** hosting a **React Router v7 Framework Mode** app (SSR capable).
- **Keep** `sse_bridge_service` as the **SSE bridge** only (`/events`), with auth/policy handled by `authentication_service`.
- In the new frontend, all HTTP calls should be same-origin **to the gateway** (either via reverse proxy or by co-locating behind the same host in production). This preserves cookie auth and avoids CORS complexity.

### Route tree

Replace the static `pages/*.html` with typed route modules:

- `/` â†’ redirect to `/projects`
- `/projects` (projects list + details + upload panel)
- `/projects/:projectId/requirements` (chat + requirements bundle + editing + enhance)
- `/projects/:projectId/tasks` (chat + backlog + editing + enhance + submit)

### State boundaries (enforced)

- **Client state (Zustand)** â€” small, ephemeral:
  - Draft chat input text
  - UI toggles (panel open/closed, focus modal open, selected compare items)
  - SSE transport lifecycle flags (connected/connecting, lastEventAt, activePromptId, activeMessageId)
  - Local â€œoptimisticâ€ link decisions while user clicks accept/reject
- **Server state (loaders/actions)** â€” API-backed:
  - Auth status + username
  - GitLab status (connectedured)
  - Projects list + selected project details
  - Requirements bundle / Backlog bundle returned from actions (kept in router state via fetchers/actionData; optionally persisted later)
- **Transport state**:
  - Keep `EventSource` instance and close/retry logic inside a hook (`useSseBridge`)
  - Keep correlation to `project_id`, `prompt_id`, `item_id` in a tiny Zustand â€œtransport sliceâ€

---

## React Router v7 Framework Mode: required conventions (Context7)

### Routes config in Framework Mode

Per React Router Framework Mode, define routes in `routes.ts` using helpers from `@react-router/dev/routes`:

```ts
import { index, layout, route } from "@react-router/dev/routes";

export default [
  layout("./app/root.tsx", [
    index("./app/routes/_index.tsx"),
    route("projects", "./app/routes/projects.tsx"),
    route("projects/:projectId/requirements", "./app/routes/projects.$projectId.requirements.tsx"),
    route("projects/:projectId/tasks", "./app/routes/projects.$projectId.tasks.tsx"),
  ]),
];
```

This matches the documented Framework Mode configuration pattern. (Context7: `/remix-run/react-router` and `/websites/reactrouter`)

### Route module API (typed loaders/actions)

Framework Mode route modules use `Route.LoaderArgs` / `Route.ActionArgs` and typed props:

```tsx
import { Route } from "./+types/projects.$projectId.tasks";

export async function loader({ params }: Route.LoaderArgs) {
  return { projectId: params.projectId };
}

export default function Component({ loaderData }: Route.ComponentProps) {
  return <div>{loaderData.projectId}</div>;
}
```

(Context7: `/remix-run/react-router`)

### Mutations + optimistic UI: `useFetcher`

Use `useFetcher` and `<fetcher.Form>` for non-navigation form submissions:

```tsx
import { useFetcher } from "react-router";

export default function Component {
  const fetcher = useFetcher;
  const isSubmitting = fetcher.state === "submitting";
  return (
    <fetcher.Form method="post">
      <input name="title" />
      <button disabled={isSubmitting}>Save</button>
    </fetcher.Form>
  );
}
```

(Context7: `/remix-run/react-router` fetchers)

---

## UI feature mapping (old â†’ new)

### Projects screen (`/projects`)

**Must replicate:**

- Projects list (search filter)
- Create/Edit Project modal (includes GitLab repo URL and backlog project URLs list)
- Delete confirmation modal
- Project details panel (status, dates, repo link, backlog projects list)
- Upload panel (drag/drop + browse, progress bar, processing log)
- â€œCache embeddingsâ€ button for backlog projects (and show in-progress state)
- Header: user profile, logout, connections panel (Auth/GitLab/RT Events)

**Replace custom code with:**

- Page loader: loads auth, config, gitlab status, projects list
- Fetchers/actions for create/update/delete/upload/cacheEmbeddings
- shadcn/ui: `Dialog`, `AlertDialog`, `Button`, `Input`, `Textarea`, `Badge`, `Select`, `Toast`

### Requirements screen (`/projects/:projectId/requirements`)

**Must replicate:**

- Split view: chat (left) and requirements bundle (right)
- â€œThinking streamâ€ box(es) that update from SSE events keyed by `prompt_id`
- Requirements renderer (markdown + mermaid)
- Inline editing (title/description/AC list + priority)
- Focus mode modal editing (and fullscreen mode)
- AI enhance per requirement with card-level progress updates from SSE (`enhancement_mode=true`, `item_id`)
- â€œConfirm & Create Tasksâ€ flow

**Replace custom code with:**

- `useFetcher` action for `POST /workflow/requirements`
- `useFetcher` action for `POST /workflow/enhance`
- Zustand: draft input + modal open state
- React components instead of BaseEditor/DOM patching

### Tasks screen (`/projects/:projectId/tasks`)

**Must replicate:**

- Split view: chat (left) and generated backlog (right)
- Render epics/tasks with markdown + mermaid
- Inline editing + focus/fullscreen modal editing
- AI enhance per epic/task (card-level progress updates from SSE `item_id`)
- Similar item matches: accept/reject with â€œmulti linkâ€ semantics (many-to-many)
- Project routing dropdown per epic/task (`target_project_id`) across multiple GitLab project IDs
- Submit to GitLab:
  - Group items by `target_project_id`
  - Use `related_to_iids` from accepted matches
  - Use `parent_epic_index` for issues
  - Render result summary + sample links

**Replace custom code with:**

- Route action for `POST /tasks/generate`
- Route action for `POST /tasks/enhance`
- Route action for `POST /gitlab/projects/apply-backlog`
- Local UI state for click decisions (Zustand) + submit status

---

## Implementation plan (agent-ingestible tasks)

### Strict rules (apply to every task below)

- **Tasks must be checkable**: each task starts with a markdown checkbox `- [ ]`.
- **Testing pyramid is mandatory per task**:
  - **Unit**: required for core logic introduced/changed by the task.
  - **Integration**: required for at least one â€œhappy pathâ€ across the main boundary the task touches (route loader/action â†” BFF â†” mocked gateway, or component â†” mocked network).
  - **E2E**: can be implemented as the **last task**; but each task must add/extend the E2E spec coverage list for that feature.
- **UI must run against backend mocks**:
  - The UI must support a **mock mode** that provides deterministic responses for `/auth/*`, ``, `/project/*`, `/workflow/*`, `/tasks/*`, `/gitlab/*`, and `/events` to enable reliable E2E runs.

### 1) Create `frontend_service` skeleton (React Router v7 Framework Mode)

- [x] **Task**: Create the new `frontend_service` app skeleton and verify Framework Mode basics.
- **Implementation**
  - **Create**: `playground/git-epic-creator/services/frontend_service/` (Node project)
  - **Add**: Vite + React Router v7 Framework Mode scaffolding (`react-router.config.ts`, `routes.ts`, `src/entry.client.tsx`, `src/entry.server.tsx`, `src/app/**`)
  - **Add**: Tailwind CSS 4 using the Vite plugin (Context7: `/tailwindlabs/tailwindcss.com`)
- **Acceptance criteria**
  - `frontend_service` builds and starts (dev/prod)
  - SSR entry files exist and Framework Mode route module typegen wiring is in place (tsconfig includes `.react-router/types/**/*` when enabled)
- **Tests (required)**
  - **Unit**: minimal sanity tests for any new pure helpers created in this task (if any)
  - **Integration**: a smoke test that can render the root route module in a test harness (no network)
  - **E2E**: add a â€œbootâ€ spec placeholder for `/projects` (implemented in the final E2E task)

### 2) Theme system (developer-controlled tokens: colors, spacing, radii, typography)

- [x] **Task**: Implement a robust, community-trusted shadcn/ui theme system: **CSS variable design tokens** + **class-based dark mode**, with **SSR-safe theme selection via cookie**.
- **Implementation**
  - **Design tokens (single source of truth) â€” shadcn theming model**
    - **Create**: `frontend_service/src/app/styles/tokens.css`
    - Define shadcnâ€™s canonical token set in `:root` and override in `.dark`, following the official shadcn theming docs (`/docs/theming`).
      - Use shadcn variable names (examples): `--background`, `--foreground`, `--card`, `--primary`, `--secondary`, `--muted`, `--accent`, `--destructive`, `--border`, `--input`, `--ring`, `--radius`, plus optional `--sidebar-*`.
      - Use `oklch(...)` values as shown in the shadcn theming docs (stable, perceptual color space).
    - Extend with a small set of *layout tokens* (since you want sizes/paddings controlled centrally):
      - `--app-padding`, `--panel-gap`, `--card-padding`, `--radius-sm/md/lg`
  - **Tailwind integration**
    - **Create**: `frontend_service/src/app/styles/app.css`
      - Import Tailwind v4 (`@import "tailwindcss";`)
      - Import tokens (`@import "./tokens.css";`)
    - Use shadcnâ€™s semantic Tailwind class conventions in the UI (`bg-background`, `text-foreground`, etc.) so components automatically track token changes.
  - **SSR-safe theme selection (React Router Framework Mode SSR)**
    - Persist the selected theme in a **cookie** (e.g. `ui_theme=light|dark`).
    - Root loader reads the cookie and applies `class="dark"` server-side to the `<html>` (avoids flash + hydration mismatch).
    - Theme toggle uses a React Router **action** (via `fetcher.Form`) to set the cookie and revalidate.
  - **Theme toggle UI**
    - Add `ThemeToggle` using shadcn/ui `DropdownMenu` + `Button` following shadcn dark-mode UX patterns.
- **Acceptance criteria**
  - A developer can change core colors/radii/spacing/paddings by editing a single file (`tokens.css`)
  - Light/dark themes work and donâ€™t require per-component edits
  - SSR renders correct theme class with no flash/mismatch
- **Tests (required)**
  - **Unit**: cookie parse/serialize helpers for theme (if extracted)
  - **Integration**: root loader applies correct theme class given a cookie; theme action sets cookie and triggers revalidation
  - **E2E**: add a spec item: â€œtoggle theme persists (cookie) and affects UIâ€ (implemented in final E2E task)

### 3) Decide and implement gateway wiring (same-origin to keep auth cookies)

- [x] **Task**: Ensure browser requests from the new UI can reach gateway endpoints with cookies.
- **Implementation**
  - **Goal**: browser requests from `frontend_service` must reach gateway endpoints (`/auth/*`, `/project/*`, `/workflow/*`, `/tasks/*`, `/gitlab/*`, `/events`) with cookies included.
  - **Implement** one of:
    - **Reverse-proxy**: route `/` to frontend-service and `/auth|/project|/workflow|/tasks|/gitlab` to envoy-gateway; `/events` to sse-bridge-service (behind Envoy)
    - or **Vite dev proxy** + production reverse proxy
- **Acceptance criteria**
  - Visiting `/projects` triggers auth redirect through `/auth/login` when unauthenticated
  - Authenticated requests to `/project/projects` succeed with cookie forwarding
- **Tests (required)**
  - **Unit**: none unless helper logic is added
  - **Integration**: run route loader against a mocked fetch target and assert cookies are forwarded server-side
  - **E2E**: add spec item: â€œauth redirect works and returns backâ€ (implemented in final E2E task)

### 4) Create a single â€œserver fetchâ€ helper for loaders/actions

- [x] **Task**: Centralize gateway access for loaders/actions.
- **Implementation**
  - **Create**: `frontend_service/src/app/lib/bff.server.ts`
    - Read cookies from `request.headers.get("cookie")`
    - Forward `cookie` header to the gateway
    - Uniform error handling: throw `Response` with status for React Router error boundaries
    - On 401: redirect to `/auth/login?redirect_uri=...`
- **Acceptance criteria**
  - All loaders/actions use a single helper (no ad-hoc fetch scattered around)
  - 401 from gateway redirects to login with correct return URL
- **Tests (required)**
  - **Unit**: error mapping + redirect behavior on 401
  - **Integration**: loader/action uses `bff.server.ts` against mock gateway (success + 401)
  - **E2E**: add spec item: â€œauth expiry mid-session redirects cleanlyâ€ (final E2E task)

### 5) Root layout route: auth + config + gitlab status in loader

- [x] **Task**: Build the app shell and global server state loader.
- **Implementation**
  - **Create**: `frontend_service/src/app/root.tsx`
    - Loader fetches: `/auth/me`, ``, `/auth/gitlab/status`
    - Provides app-shell layout (top bar + outlet)
    - Provides connection indicator UI (Auth/GitLab/RT Events)
- **Acceptance criteria**
  - All child routes render within the shell
  - On unauthenticated, route redirects to `/auth/login` with return URL preserved
- **Tests (required)**
  - **Unit**: loader utilities if extracted
  - **Integration**: root loader returns expected shape given mocked endpoints
  - **E2E**: add spec item: â€œheader indicators show connected/connecting statesâ€ (final E2E task)

### 6) Zustand store: strict slices (draft UI + transport SSE)

- [x] **Task**: Add client-only ephemeral UI/transport store (strict boundaries).
- **Implementation**
  - **Create**: `frontend_service/src/app/state/store.ts`
    - Use slices pattern (Context7: `/pmndrs/zustand/v5.0.8`)
    - Slices:
      - `draftSlice`: `requirementsDraft`, `tasksDraft`, `chatDraftByRouteKey`
      - `uiSlice`: panel open, modals open, focus mode state
      - `sseSlice`: status, lastEventAt, activePromptId, activeMessageId, enhancementItemProgress map
- **Acceptance criteria**
  - No server data (projects/bundles) stored in Zustand
  - Zustand store stays ~KB scale (no giant bundles in it)
- **Tests (required)**
  - **Unit**: slice reducers/actions behavior
  - **Integration**: component reads/writes draft state and persists across navigation without involving loaders
  - **E2E**: add spec item: â€œdraft input persists during navigation back/forwardâ€ (final E2E task)

### 7) SSE bridge hook (transport state + correlation)

- [x] **Task**: Implement SSE transport lifecycle and correlation to prompt/item ids.
- **Implementation**
  - **Create**: `frontend_service/src/app/sse/useSseBridge.ts`
    - Connect `EventSource("/events")`
    - On `open|error|hello`: update `sseSlice.status`
    - Listen: `project_progress`, `retrieval_progress`, `ai_requirements_progress`, `ai_tasks_progress`
    - Parse `evt.data` JSON
    - Filter by `project_id`
    - If `enhancement_mode && item_id`: update per-item progress state
    - Else: append to â€œthinking streamâ€ keyed by `prompt_id`
- **Acceptance criteria**
  - Switching projects closes/reopens SSE filters correctly
  - No memory leaks: `EventSource.close` on unmount
- **Tests (required)**
  - **Unit**: event routing logic (pure function) for enhancement vs thinking stream updates
  - **Integration**: simulated EventSource events update Zustand slices correctly
  - **E2E**: add spec item: â€œSSE shows progress while generatingâ€ (final E2E task; uses mock SSE)

### 8) Thinking stream component (replace TypewriterBoxManager/TypewriterBox)

- [x] **Task**: Replace bespoke thinking stream UI with React components + motion.
- **Implementation**
  - **Create**: `frontend_service/src/app/components/thinking/ThinkingStream.tsx`
    - Streams keyed by `prompt_id`
    - Collapsible UI (shadcn) + transitions (Framer Motion)
    - Render markdown safely; include Mermaid support
- **Acceptance criteria**
  - Receives SSE events and renders incremental updates without blocking UI
  - Collapsing/expanding works and animates smoothly
- **Tests (required)**
  - **Unit**: markdown rendering helpers if extracted
  - **Integration**: feed a stream of events and assert UI updates/scroll behavior
  - **E2E**: add spec item: â€œthinking stream groups by prompt_idâ€ (final E2E task)

### 9) Projects route (`/projects`): loader + fetcher actions

- [x] **Task**: Implement Projects screen feature parity via loaders/actions/components.
- **Implementation**
  - **Create**: `frontend_service/src/app/routes/projects.tsx`
    - Loader: `GET /project/projects`
    - Actions/fetchers:
      - create project (`POST /project/projects`)
      - update project (`PUT /project/projects/:id`)
      - delete project (`DELETE /project/projects/:id`)
      - upload docs (`POST /project/projects/:id/documents/upload`)
      - cache embeddings (`POST /project/projects/multi/cache-embeddings?...`)
    - Replace modals with shadcn:
      - Create/Edit: `Dialog`
      - Delete: `AlertDialog`
- **Acceptance criteria**
  - Feature parity with existing projects page
  - Upload uses multipart form submission (server action converts FormData passthrough)
- **Tests (required)**
  - **Unit**: payload builders/validators (project create/update, cache-embeddings query builder)
  - **Integration**: route loader + each action path using mocked gateway responses
  - **E2E**: add spec items for create/edit/delete/upload/cache (final E2E task; using mocks)

### 10) Requirements route: generate bundle + enhance item + confirm flow

- [x] **Task**: Implement requirements generation + enhancement using actions/fetchers.
- **Implementation**
  - **Create**: `frontend_service/src/app/routes/projects.$projectId.requirements.tsx`
    - Loader: `GET /project/projects/:projectId`
    - Action â€œgenerate requirementsâ€: `POST /workflow/requirements` with `{ project_id, prompt, prompt_id? }`
    - Action â€œenhance requirementâ€: `POST /workflow/enhance`
    - Confirm flow: navigate to tasks without sessionStorage hacks (prefer navigation state or a handoff action)
- **Acceptance criteria**
  - Requirements generation works and updates UI
  - Enhance button triggers per-card progress and updates the card on completion
- **Tests (required)**
  - **Unit**: formatters (requirements-to-text handoff), payload builders
  - **Integration**: generate+enhance actions with mocked gateway + simulated SSE progress
  - **E2E**: add spec items for generate/enhance/confirm (final E2E task; using mocks)

### 11) Requirements editor UI: inline + focus modal

- [x] **Task**: Replace BaseEditor DOM-mutation editing with React-first editing.
- **Implementation**
  - **Create**: `frontend_service/src/app/components/requirements/RequirementsEditor.tsx`
    - Inline editing with controlled inputs
    - Focus modal with shadcn `Dialog` and live preview
    - Priority select, acceptance criteria split/join
- **Acceptance criteria**
  - Inline edits update immediately and remain stable across re-renders
  - Focus modal supports fullscreen-like mode (Dialog sizing + responsive)
- **Tests (required)**
  - **Unit**: AC split/join, priority normalization
  - **Integration**: render editor, edit fields, assert bundle updates and preview updates
  - **E2E**: add spec item: â€œedit requirement inline + in focus modalâ€ (final E2E task)

### 12) Tasks route: generate backlog + enhance item + submit to GitLab

- [x] **Task**: Implement tasks generation, enhancement, and submission via actions.
- **Implementation**
  - **Create**: `frontend_service/src/app/routes/projects.$projectId.tasks.tsx`
    - Loader: `GET /project/projects/:projectId` (needs `gitlab_backlog_project_ids`)
    - Action â€œgenerate backlogâ€: `POST /tasks/generate` with `{ project_id, message, prompt_id? }`
    - Action â€œenhance epic/taskâ€: `POST /tasks/enhance`
    - Action â€œsubmit to GitLabâ€: `POST /gitlab/projects/apply-backlog` with grouped payload
- **Acceptance criteria**
  - Multi-project dropdown routing is respected in payload
  - Similar match decisions produce `related_to_iids` arrays for each epic/issue payload
- **Tests (required)**
  - **Unit**: backlog-to-apply payload shaping (`related_to_iids`, `parent_epic_index`, grouping by project)
  - **Integration**: generate+enhance+submit actions (mock gateway), plus SSE progress routing
  - **E2E**: add spec items for generate/enhance/accept match/routing dropdown/submit (final E2E task)

### 13) Tasks editor UI: epics/tasks, similar matches, routing dropdowns

- [x] **Task**: Implement backlog viewing/editing UX (React-first).
- **Implementation**
  - **Create**: `frontend_service/src/app/components/tasks/BacklogEditor.tsx`
    - Render epics/tasks with shadcn Cards + markdown + mermaid
    - Similar matches accept/reject (multi-accept) and persistence across re-renders
    - Project routing dropdown per epic/task
- **Acceptance criteria**
  - User can accept multiple matches for a single item (â€œmany-to-many linkingâ€)
  - Re-render does not lose decisions or selections
- **Tests (required)**
  - **Unit**: link decision reducers/selectors
  - **Integration**: simulate accept/reject, change routing dropdown, ensure payload builder sees correct state
  - **E2E**: add spec item: â€œaccept multiple similar matches and submitâ€ (final E2E task)

### 14) Error boundaries and toasts

- [x] **Task**: Replace alerts with predictable error boundaries + toasts.
- **Implementation**
  - Add route-level `ErrorBoundary` exports for auth failures/upstream 5xx/validation errors
  - Add toast system (shadcn `Toast`) for success/error feedback instead of `alert`
- **Acceptance criteria**
  - No browser `alert` calls remain in the new UI
  - Errors render predictably and donâ€™t blank the app
- **Tests (required)**
  - **Unit**: error mappers (if any)
  - **Integration**: simulate 401/500 from loader/action and assert error UI/toast
  - **E2E**: add spec item: â€œerror boundary renders and user can recoverâ€ (final E2E task)

### 15) Backend mocks mode (mandatory for E2E reliability)

- [x] **Task**: Make the UI fully runnable without real backend services.
- **Implementation**
  - Add **mock mode** flag (e.g. `MOCK_BACKEND=1`) to:
    - Mock server-side loader/action fetches (Node runtime) with deterministic fixtures
    - Mock client-side fetches (Service Worker) with the same fixtures
    - Mock SSE `/events` stream (scripted timeline for progress + enhancement events)
  - Recommended approach:
    - Use MSW for browser (`msw/browser`) and Node (`msw/node`) so loader/action + client share handlers
    - Keep fixtures in `src/app/mocks/fixtures/*`
- **Acceptance criteria**
  - Running frontend in mock mode supports Projects/Requirements/Tasks flows end-to-end without `sse_bridge_service`
  - SSE-driven UI (thinking stream + enhancement progress) works with the mock event stream
- **Tests (required)**
  - **Unit**: fixture validators (types/shape)
  - **Integration**: route loaders/actions work against MSW-node
  - **E2E**: final E2E suite runs entirely in mock mode

### 16) E2E test suite (last task): Playwright + mock backend

- [x] **Task**: Implement the E2E layer as the last step, using the mock backend to keep it deterministic.
- **Implementation**
  - Add Playwright tests that cover:
    - Projects: list/create/edit/delete/upload/cache embeddings
    - Requirements: generate/enhance/edit/confirm
    - Tasks: generate/enhance/edit/accept matches/routing dropdown/submit
    - Theme: toggle and persistence
    - Error recovery: loader/action failures
- **Acceptance criteria**
  - E2E tests run green locally and in CI
  - E2E does not depend on live microservices (uses mocks)

### 17) Remove legacy static UI from the old gateway UI service (optional phase, after new UI is ready)

- [x] **Task**: Turn the old gateway UI service into gateway-only.
- **Implementation**
  - Remove or stop referencing:
    - legacy static pages and JS (removed)
- **Acceptance criteria**
  - No users reach old HTML pages in production
  - Gateway endpoints remain intact

---

## Code snippets (complex and main places)

### 1) Tailwind v4 Vite plugin (frontend_service)

```ts
import { defineConfig } from "vite";
import tailwindcss from "@tailwindcss/vite";

export default defineConfig({
  plugins: [tailwindcss],
});
```

(Context7: `/tailwindlabs/tailwindcss.com`)

### 2) Zustand â€œslices patternâ€ store (client-only state boundary)

```ts
import { create, StateCreator } from "zustand";

type SseStatus = "disconnected" | "connecting" | "connected";

type SseSlice = {
  sseStatus: SseStatus;
  setSseStatus: (s: SseStatus) => void;
  lastEventAt: number | null;
  setLastEventAt: (t: number) => void;
  enhancementProgressByItemId: Record<string, { status: string; detailsMd?: string }>;
  upsertEnhancementProgress: (itemId: string, p: { status: string; detailsMd?: string }) => void;
};

type DraftSlice = {
  chatDraft: string;
  setChatDraft: (v: string) => void;
};

type Store = SseSlice & DraftSlice;

const createSseSlice: StateCreator<Store, [], [], SseSlice> = (set) => ({
  sseStatus: "disconnected",
  setSseStatus: (s) => set({ sseStatus: s }),
  lastEventAt: null,
  setLastEventAt: (t) => set({ lastEventAt: t }),
  enhancementProgressByItemId: {},
  upsertEnhancementProgress: (itemId, p) =>
    set((st) => ({
      enhancementProgressByItemId: {
        ...st.enhancementProgressByItemId,
        [itemId]: p,
      },
    })),
});

const createDraftSlice: StateCreator<Store, [], [], DraftSlice> = (set) => ({
  chatDraft: "",
  setChatDraft: (v) => set({ chatDraft: v }),
});

export const useUiStore = create<Store>((...a) => ({
  ...createSseSlice(...a),
  ...createDraftSlice(...a),
}));
```

(Context7: `/pmndrs/zustand/v5.0.8`)

### 3) SSE bridge hook (EventSource + project_id filtering)

```ts
import { useEffect, useRef } from "react";
import { useUiStore } from "~/state/store";

type SseMessage = {
  project_id?: string;
  prompt_id?: string;
  enhancement_mode?: boolean;
  item_id?: string;
  status?: string;
  details_md?: string;
  thought_summary?: string;
};

export function useSseBridge(projectId: string | null) {
  const setSseStatus = useUiStore((s) => s.setSseStatus);
  const setLastEventAt = useUiStore((s) => s.setLastEventAt);
  const upsertEnhancementProgress = useUiStore((s) => s.upsertEnhancementProgress);
  const evtSourceRef = useRef<EventSource | null>(null);

  useEffect( => {
    if (!projectId) return;

    setSseStatus("connecting");
    const es = new EventSource("/events");
    evtSourceRef.current = es;

    const onOpen =  => setSseStatus("connected");
    const onError =  => setSseStatus("connecting");
    const onHello =  => setSseStatus("connected");

    const onMsg = (evt: MessageEvent) => {
      setLastEventAt(Date.now);
      const msg = JSON.parse(evt.data) as SseMessage;
      if (String(msg.project_id ?? "") !== String(projectId)) return;

      if (msg.enhancement_mode && msg.item_id) {
        upsertEnhancementProgress(msg.item_id, {
          status: msg.status ?? "unknown",
          detailsMd: msg.details_md ?? msg.thought_summary,
        });
      } else {
        // route to ThinkingStream store keyed by prompt_id (not shown)
      }
    };

    es.addEventListener("open", onOpen);
    es.addEventListener("error", onError);
    es.addEventListener("hello", onHello);
    es.addEventListener("retrieval_progress", onMsg);
    es.addEventListener("ai_requirements_progress", onMsg);
    es.addEventListener("ai_tasks_progress", onMsg);
    es.addEventListener("project_progress", onMsg);

    return  => {
      es.close;
      evtSourceRef.current = null;
      setSseStatus("disconnected");
    };
  }, [projectId, setSseStatus, setLastEventAt, upsertEnhancementProgress]);
}
```

### 4) Framework Mode route config + typed loader (routes.ts + route module)

```ts
// routes.ts
import { layout, route } from "@react-router/dev/routes";
import type { RouteConfig } from "@react-router/dev/routes";

export default [
  layout("./app/root.tsx", [
    route("projects", "./app/routes/projects.tsx"),
    route("projects/:projectId/requirements", "./app/routes/projects.$projectId.requirements.tsx"),
    route("projects/:projectId/tasks", "./app/routes/projects.$projectId.tasks.tsx"),
  ]),
] satisfies RouteConfig;
```

```tsx
// app/routes/projects.tsx
import type { Route } from "./+types/projects";

export async function loader({ request }: Route.LoaderArgs) {
  // fetch /project/projects via shared bff helper (not shown)
  return { projects: [] };
}

export default function ProjectsRoute({ loaderData }: Route.ComponentProps) {
  return <div>{loaderData.projects.length} projects</div>;
}
```

(Context7: `/remix-run/react-router` and `/websites/reactrouter`)

### 5) Tasks â€œsubmit to GitLabâ€ payload shaping (the hardest business logic)

```ts
type SimilarMatch = { iid?: number | string; id: number | string; link_decision?: "accepted" | "rejected" | "pending" };
type BacklogTask = { title: string; description?: string; target_project_id?: string; similar?: SimilarMatch[] };
type BacklogEpic = { title: string; description?: string; target_project_id?: string; similar?: SimilarMatch[]; tasks?: BacklogTask[] };

function acceptedIids(similar: SimilarMatch[] | undefined) {
  return (similar ?? []).filter((s) => s.link_decision === "accepted").map((s) => s.iid ?? s.id);
}

export function buildApplyBacklogRequest(args: {
  internalProjectId: string;
  promptId: string;
  defaultGitlabProjectId: string;
  epics: BacklogEpic[];
}) {
  const byProject = new Map<string, { project_id: string; epics: any[]; issues: any[] }>;

  const getBucket = (projectId: string) => {
    const key = projectId || args.defaultGitlabProjectId;
    if (!byProject.has(key)) byProject.set(key, { project_id: key, epics: [], issues: [] });
    return byProject.get(key)!;
  };

  args.epics.forEach((epic, epicIdx) => {
    const epicTarget = epic.target_project_id || args.defaultGitlabProjectId;
    getBucket(epicTarget).epics.push({
      title: epic.title,
      description: epic.description ?? "",
      labels: [],
      target_project_id: epicTarget,
      related_to_iids: acceptedIids(epic.similar),
    });

    (epic.tasks ?? []).forEach((task) => {
      const taskTarget = task.target_project_id || epicTarget;
      getBucket(taskTarget).issues.push({
        title: task.title,
        description: task.description ?? "",
        labels: [],
        target_project_id: taskTarget,
        related_to_iids: acceptedIids(task.similar),
        parent_epic_index: epicIdx,
      });
    });
  });

  return {
    prompt_id: args.promptId,
    internal_project_id: args.internalProjectId,
    projects: Array.from(byProject.values),
  };
}
```

This logic replaces the bespoke JS currently embedded in the legacy UI (removed).

### 6) shadcn/ui Dialog pattern for focus editing (replace ModalManager/BaseEditor)

```tsx
import { Button } from "@/components/ui/button";
import { Dialog, DialogContent, DialogHeader, DialogTitle, DialogTrigger } from "@/components/ui/dialog";

export function FocusEditorDialog(props: { title: string; children: React.ReactNode }) {
  return (
    <Dialog>
      <DialogTrigger asChild>
        <Button variant="outline">Open focus editor</Button>
      </DialogTrigger>
      <DialogContent className="max-w-5xl">
        <DialogHeader>
          <DialogTitle>{props.title}</DialogTitle>
        </DialogHeader>
        {props.children}
      </DialogContent>
    </Dialog>
  );
}
```

(Context7: `/websites/ui_shadcn`)

### 7) Framer Motion for smooth list/layout transitions (thinking stream + cards)

```tsx
import { AnimatePresence, LayoutGroup, motion } from "motion/react";

export function AnimatedList(props: { items: { id: string }[] }) {
  return (
    <LayoutGroup>
      <motion.ul layout className="space-y-2">
        <AnimatePresence>
          {props.items.map((it) => (
            <motion.li
              layout
              key={it.id}
              initial={{ opacity: 0, y: 6 }}
              animate={{ opacity: 1, y: 0 }}
              exit={{ opacity: 0, y: -6 }}
            />
          ))}
        </AnimatePresence>
      </motion.ul>
    </LayoutGroup>
  );
}
```

(Context7: `/websites/motion-dev-docs`)

---

## Self-evaluation checklist (coverage + â€œreplace custom code with frameworkâ€)

### Screens covered

- **Projects**
  - [ ] List projects (search/filter)
  - [ ] Create project (Dialog)
  - [ ] Edit project (Dialog)
  - [ ] Delete project (AlertDialog)
  - [ ] View project details (status/date/repo/backlog list)
  - [ ] Upload documents (multipart action + progress UI)
  - [ ] Cache embeddings for backlog projects
  - [ ] Header indicators (Auth/GitLab/RT Events) + logout

- **Requirements**
  - [ ] Split view layout
  - [ ] Generate requirements (action + fetcher)
  - [ ] Render markdown + mermaid
  - [ ] Inline editing for all fields (title/description/priority/AC list)
  - [ ] Focus editing modal (Dialog) + preview
  - [ ] Enhance single requirement (action) + per-card SSE progress updates (`item_id`)
  - [ ] Confirm â†’ tasks handoff without sessionStorage hacks

- **Tasks**
  - [ ] Split view layout
  - [ ] Generate backlog (action + fetcher)
  - [ ] Render epics/tasks markdown + mermaid
  - [ ] Inline editing for epic/task fields (title/desc/AC/deps where present)
  - [ ] Focus editing modal (Dialog) + preview
  - [ ] Enhance epic/task (action) + per-card SSE progress updates (`item_id`)
  - [ ] Similar matches accept/reject (multi-accept) and state persistence across re-renders
  - [ ] Project routing dropdown per epic/task
  - [ ] Submit backlog grouped by target project (payload uses `related_to_iids`, issues include `parent_epic_index`)
  - [ ] Show submission result summary + links and errors

### State boundary compliance

- [ ] Zustand contains only ephemeral UI + transport state (drafts, toggles, SSE status, per-item progress)
- [ ] Projects/project details are fetched in loaders (server state)
- [ ] Requirements/backlog bundles originate from actions/fetchers (server state), not shoved into Zustand
- [ ] SSE lifecycle and EventSource instance lives in a hook; Zustand stores only derived flags/progress

### Theme system coverage

- [ ] Theme tokens live in a single place (e.g. `src/app/styles/theme.css`) and control colors/spacing/radii/typography
- [ ] Light/dark switching works without per-component style edits
- [ ] shadcn/ui components respect the token system (no hard-coded colors/radii outside the token map)

### Testing pyramid enforcement (strict)

- [ ] Every implementation task adds/updates **unit tests** for new logic
- [ ] Every implementation task adds/updates **integration tests** for the boundary it touches (loader/action â†” mocked gateway, or component â†” mocked network)
- [ ] E2E suite exists and covers all features listed in the plan (implemented last)
- [ ] UI can run in **mock backend mode** (deterministic `/auth`, ``, `/project`, `/workflow`, `/tasks`, `/gitlab`, `/events`) so E2E does not depend on live microservices

### â€œReplace custom code with frameworks as much as possibleâ€

- [ ] No BaseController/BasePageController clones exist in the new app
- [ ] No DOM mutation editors (BaseEditor) â€” replaced with React components + shadcn Dialogs
- [ ] No `alert` usage â€” replaced with toast system + error boundaries
- [ ] Navigation uses React Router routes (no `window.location.href` assembly; use links/navigate)
- [ ] SessionStorage bridging removed or minimized (prefer navigation state or server-side persistence)

### Remaining intentional gaps / follow-ups

- [ ] If SSR for generated bundles is required, add a backend endpoint to persist/retrieve last bundle per project/session so loaders can SSR it (today the backend does not expose that state).


