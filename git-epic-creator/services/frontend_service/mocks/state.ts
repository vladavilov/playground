import { projectsFixture } from "./fixtures/projects";

export type MockProject = (typeof projectsFixture)[number];

type MockGitlabState = {
  connected: boolean;
  configured: boolean;
};

type MockAuthState = {
  authenticated: boolean;
  username: string;
};

type MockState = {
  auth: MockAuthState;
  gitlab: MockGitlabState;
  projects: MockProject[];
};

function clone<T>(v: T): T {
  return JSON.parse(JSON.stringify(v)) as T;
}

const initialState: MockState = {
  auth: { authenticated: true, username: "mock-user" },
  gitlab: { connected: false, configured: true },
  projects: clone(projectsFixture),
};

let state: MockState = clone(initialState);

type CreateProjectPayload = Pick<
  MockProject,
  | "name"
  | "description"
  | "gitlab_repository_urls"
  | "gitlab_backlog_project_ids"
  | "gitlab_backlog_project_urls"
> & { status?: MockProject["status"] };

export const mockState = {
  get: () => state,
  reset: () => {
    state = clone(initialState);
  },
  setGitlabConnected: (connected: boolean) => {
    state = { ...state, gitlab: { ...state.gitlab, connected } };
  },
  setAuthenticated: (authenticated: boolean) => {
    state = { ...state, auth: { ...state.auth, authenticated } };
  },
  createProject: (p: CreateProjectPayload) => {
    const now = new Date().toISOString();
    const id = crypto.randomUUID();
    const next: MockProject = {
      id,
      name: p.name,
      description: p.description ?? null,
      gitlab_repository_urls: p.gitlab_repository_urls ?? [],
      gitlab_backlog_project_ids: p.gitlab_backlog_project_ids ?? [],
      gitlab_backlog_project_urls: p.gitlab_backlog_project_urls ?? [],
      status: p.status ?? "active",
      created_at: now,
      updated_at: now,
    };
    state = { ...state, projects: [next, ...state.projects] };
    return next;
  },
  updateProject: (projectId: string, patch: Partial<MockProject>) => {
    const now = new Date().toISOString();
    let updated: MockProject | null = null;
    state = {
      ...state,
      projects: state.projects.map((p) => {
        if (p.id !== projectId) return p;
        updated = { ...p, ...patch, id: p.id, updated_at: now };
        return updated!;
      }),
    };
    return updated;
  },
  deleteProject: (projectId: string) => {
    const before = state.projects.length;
    state = { ...state, projects: state.projects.filter((p) => p.id !== projectId) };
    return state.projects.length !== before;
  },
};


