// @vitest-environment node
import { afterAll, beforeAll, beforeEach, describe, expect, it, vi } from "vitest";

import { mockState } from "./state";
import { subscribeServerSse } from "./serverSseBus";
import { ensureMockSseBridgeRunning } from "./mockSseBridgeService";

import { loader as projectsLoader } from "../src/routes/projects";
import { action as projectsAction } from "../src/routes/projects";
import { action as reqAction } from "../src/routes/projects.$projectId.requirements";
import { action as tasksAction } from "../src/routes/projects.$projectId.tasks";
import { startMocks } from "./start";

describe("mock backend mode (msw/node)", () => {
  beforeAll(async () => {
    // Ensure the mock sse-bridge-service binds to an ephemeral port in tests.
    process.env.SSE_BRIDGE_ORIGIN = "http://127.0.0.1:0";
    await ensureMockSseBridgeRunning();
    await startMocks();
  });
  afterAll(() => {
    // no-op: startMocks owns its server instance; keep tests process-scoped
  });
  beforeEach(() => mockState.reset());

  it("serves projects + requirements + tasks flows with fixtures", async () => {
    const request = new Request("http://localhost/projects", { headers: { cookie: "a=b" } });
    const projects = await projectsLoader({ request, params: {}, context: {} } as any);
    expect(projects.projects[0].name).toBe("Mock Project");

    const fdReq = new FormData();
    fdReq.set("intent", "generate");
    fdReq.set("prompt", "hello");
    const req = new Request(
      "http://localhost/projects/11111111-1111-1111-1111-111111111111/requirements",
      {
        method: "POST",
        body: fdReq,
        headers: { cookie: "a=b" },
      },
    );
    const reqRes = await reqAction({
      request: req,
      params: { projectId: "11111111-1111-1111-1111-111111111111" },
      context: {},
    } as any);
    expect((reqRes as any).bundle.business_requirements[0].id).toBe("br-1");

    const fdTasks = new FormData();
    fdTasks.set("intent", "generate");
    fdTasks.set("message", "hello");
    const tReq = new Request("http://localhost/projects/11111111-1111-1111-1111-111111111111/tasks", {
      method: "POST",
      body: fdTasks,
      headers: { cookie: "a=b" },
    });
    const tRes = await tasksAction({
      request: tReq,
      params: { projectId: "11111111-1111-1111-1111-111111111111" },
      context: {},
    } as any);
    expect((tRes as any).backlog.epics[0].id).toBe("epic-1");
  });

  it("supports create/update/delete project via mock backend", async () => {
    const createFd = new FormData();
    createFd.set("intent", "create");
    createFd.set("name", "Created Project");
    createFd.set("description", "desc");
    createFd.set("gitlab_repository_urls", "https://gitlab.example.com/group/project\n");
    createFd.set("gitlab_backlog_project_urls", "https://gitlab.example.com/group/backlog\n");

    const createReq = new Request("http://localhost/projects", {
      method: "POST",
      body: createFd,
      headers: { cookie: "a=b" },
    });

    const createRes = await projectsAction({ request: createReq, params: {}, context: {} } as any);
    expect((createRes as any).ok).toBe(true);

    const afterCreate = await projectsLoader({
      request: new Request("http://localhost/projects", { headers: { cookie: "a=b" } }),
      params: {},
      context: {},
    } as any);
    const created = afterCreate.projects.find((p: any) => p.name === "Created Project");
    expect(created).toBeTruthy();
    if (!created) throw new Error("created project not found");

    const updateFd = new FormData();
    updateFd.set("intent", "update");
    updateFd.set("projectId", String(created.id));
    updateFd.set("name", "Updated Project");
    updateFd.set("description", "updated desc");
    updateFd.set("gitlab_repository_urls", "");
    updateFd.set("gitlab_backlog_project_urls", "");

    const updateReq = new Request("http://localhost/projects", {
      method: "POST",
      body: updateFd,
      headers: { cookie: "a=b" },
    });

    const updateRes = await projectsAction({ request: updateReq, params: {}, context: {} } as any);
    expect((updateRes as any).ok).toBe(true);

    const afterUpdate = await projectsLoader({
      request: new Request("http://localhost/projects", { headers: { cookie: "a=b" } }),
      params: {},
      context: {},
    } as any);
    expect(afterUpdate.projects.some((p: any) => p.name === "Updated Project")).toBe(true);

    const delFd = new FormData();
    delFd.set("intent", "delete");
    delFd.set("projectId", String(created.id));
    const delReq = new Request("http://localhost/projects", {
      method: "POST",
      body: delFd,
      headers: { cookie: "a=b" },
    });

    const delRes = await projectsAction({ request: delReq, params: {}, context: {} } as any);
    expect((delRes as any).ok).toBe(true);

    const afterDelete = await projectsLoader({
      request: new Request("http://localhost/projects", { headers: { cookie: "a=b" } }),
      params: {},
      context: {},
    } as any);
    expect(afterDelete.projects.some((p: any) => p.id === created.id)).toBe(false);
  });

  it("supports upload + cache embeddings endpoints in mock backend", async () => {
    const projectId = "11111111-1111-1111-1111-111111111111";

    const uploadFd = new FormData();
    uploadFd.set("intent", "upload");
    uploadFd.set("projectId", projectId);
    uploadFd.append("files", new File(["hello"], "hello.txt", { type: "text/plain" }));

    const uploadReq = new Request("http://localhost/projects", {
      method: "POST",
      body: uploadFd,
      headers: { cookie: "a=b" },
    });
    const uploadRes = await projectsAction({ request: uploadReq, params: {}, context: {} } as any);
    expect((uploadRes as any).ok).toBe(true);

    const cacheFd = new FormData();
    cacheFd.set("intent", "cacheEmbeddings");
    cacheFd.set("projectId", projectId);
    cacheFd.set("gitlab_project_ids", "100,200");
    const cacheReq = new Request("http://localhost/projects", {
      method: "POST",
      body: cacheFd,
      headers: { cookie: "a=b" },
    });
    const cacheRes = await projectsAction({ request: cacheReq, params: {}, context: {} } as any);
    expect((cacheRes as any).ok).toBe(true);
  });

  it("publishes SSE progress events from server-side mocks (SSR) into the server SSE bus", async () => {
    vi.useFakeTimers();
    const events: { event: string; data: unknown }[] = [];
    const unsubscribe = subscribeServerSse((e) => events.push(e));

    const projectId = "11111111-1111-1111-1111-111111111111";
    const uploadFd = new FormData();
    uploadFd.set("intent", "upload");
    uploadFd.set("projectId", projectId);
    uploadFd.append("files", new File(["hello"], "hello.txt", { type: "text/plain" }));

    const uploadReq = new Request("http://localhost/projects", {
      method: "POST",
      body: uploadFd,
      headers: { cookie: "a=b" },
    });

    await projectsAction({ request: uploadReq, params: {}, context: {} } as any);
    await vi.runAllTimersAsync();
    unsubscribe();
    vi.useRealTimers();

    expect(events.some((e) => e.event === "project_document_upload_progress")).toBe(true);
  });

  it("publishes AI thinking progress events (ai_generation_progress) with prompt_id for chat thinking box", async () => {
    vi.useFakeTimers();
    const events: { event: string; data: unknown }[] = [];
    const unsubscribe = subscribeServerSse((e) => events.push(e));

    const projectId = "11111111-1111-1111-1111-111111111111";

    const fdReq = new FormData();
    fdReq.set("intent", "generate");
    fdReq.set("prompt", "hello");
    const req = new Request(`http://localhost/projects/${projectId}/requirements`, {
      method: "POST",
      body: fdReq,
      headers: { cookie: "a=b" },
    });
    await reqAction({ request: req, params: { projectId }, context: {} } as any);

    const fdTasks = new FormData();
    fdTasks.set("intent", "generate");
    fdTasks.set("message", "hello");
    const tReq = new Request(`http://localhost/projects/${projectId}/tasks`, {
      method: "POST",
      body: fdTasks,
      headers: { cookie: "a=b" },
    });
    await tasksAction({ request: tReq, params: { projectId }, context: {} } as any);

    await vi.runAllTimersAsync();
    unsubscribe();
    vi.useRealTimers();

    const aiEvents = events.filter((e) => e.event === "ai_generation_progress");
    expect(aiEvents.length).toBeGreaterThan(0);
    expect(
      aiEvents.some((e) => {
        const d = e.data as any;
        return d && typeof d === "object" && typeof d.prompt_id === "string" && d.prompt_id.length > 0;
      }),
    ).toBe(true);
  });

  it("serves SSE stream via the mock sse-bridge-service at /events", async () => {
    // Ensure mock gateway is started (idempotent).
    const { origin } = await ensureMockSseBridgeRunning();

    const res = await fetch(new URL("/events", origin));
    expect(res.status).toBe(200);
    expect(res.headers.get("content-type")).toContain("text/event-stream");
  });
});


