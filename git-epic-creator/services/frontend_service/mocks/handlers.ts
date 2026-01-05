import { http, HttpResponse } from "msw";

import { applyBacklogResponseFixture } from "./fixtures/gitlab";
import { requirementsBundleFixture } from "./fixtures/requirements";
import { backlogBundleFixture, enhancedTaskFixture } from "./fixtures/tasks";
import { emitServerSseEvent } from "./serverSseBus";
import { mockState } from "./state";

type ProjectCreatePayload = {
  name: string;
  description: string | null;
  gitlab_repository_urls: string[];
  gitlab_backlog_project_urls: string[];
};

function readStringRecord(v: unknown): Record<string, unknown> {
  return v && typeof v === "object" ? (v as Record<string, unknown>) : {};
}

function asNonEmptyString(v: unknown): string | null {
  if (typeof v !== "string") return null;
  const t = v.trim();
  return t ? t : null;
}

function emitSse(event: string, data: unknown) {
  // In this app, the backend calls that trigger SSE are executed server-side (SSR actions/loaders),
  // so the only events that matter are those emitted in Node. In browser/MSW(SW) this is a no-op.
  if (!import.meta.env.SSR) return;
  // Include an event timestamp for de-duping in the UI (or across reconnects).
  if (data && typeof data === "object" && !Array.isArray(data)) {
    const rec = data as Record<string, unknown>;
    emitServerSseEvent(event, { ...rec, event_at: rec.event_at ?? Date.now() });
    return;
  }
  emitServerSseEvent(event, { value: data, event_at: Date.now() });
}

function emitAiThinkingSequence(args: {
  projectId: string;
  promptId: string;
  messageType: "ai_requirements_progress" | "ai_tasks_progress";
}) {
  const steps =
    args.messageType === "ai_requirements_progress"
      ? [
          "Understanding context",
          "Extracting key goals",
          "Drafting requirements",
          "Scoring and formatting output",
        ]
      : ["Parsing request", "Generating epics", "Generating tasks", "Scoring and formatting output"];

  steps.forEach((text, idx) => {
    setTimeout(() => {
      emitSse("ai_generation_progress", {
        project_id: args.projectId,
        message_type: args.messageType,
        prompt_id: args.promptId,
        thought_summary: text,
      });
    }, 80 + idx * 140);
  });
}

export const handlers = [
  // Auth (root loader depends on these)
  http.get("*/auth/me", () => {
    const st = mockState.get();
    return HttpResponse.json({
      authenticated: st.auth.authenticated,
      username: st.auth.username,
    });
  }),
  http.get("*/auth/gitlab/status", () => {
    const st = mockState.get();
    return HttpResponse.json({
      connected: st.gitlab.connected,
      configured: st.gitlab.configured,
    });
  }),
  http.post("*/auth/logout", () => {
    mockState.setGitlabConnected(false);
    return HttpResponse.json({ ok: true });
  }),

  http.get("*/project/projects", () => HttpResponse.json(mockState.get().projects)),
  http.get("*/project/projects/:projectId", ({ params }) => {
    const projectId = String(params.projectId ?? "");
    const p = mockState.get().projects.find((x) => x.id === projectId) ?? null;
    if (!p) return new HttpResponse("not found", { status: 404 });
    return HttpResponse.json(p);
  }),
  http.post("*/project/projects", async ({ request }) => {
    const body = (await request.json()) as ProjectCreatePayload;
    if (!body?.name?.trim()) return new HttpResponse("name is required", { status: 400 });
    const created = mockState.createProject({
      name: body.name.trim(),
      description: body.description ?? "",
      gitlab_repository_urls: Array.isArray(body.gitlab_repository_urls) ? body.gitlab_repository_urls : [],
      gitlab_backlog_project_urls: Array.isArray(body.gitlab_backlog_project_urls)
        ? body.gitlab_backlog_project_urls
        : [],
      gitlab_backlog_project_ids: [],
    });
    return HttpResponse.json(created, { status: 201 });
  }),
  http.put("*/project/projects/:projectId", async ({ params, request }) => {
    const projectId = String(params.projectId ?? "");
    const body = (await request.json()) as Partial<ProjectCreatePayload>;
    const updated = mockState.updateProject(projectId, {
      name: typeof body.name === "string" ? body.name.trim() : undefined,
      description:
        typeof body.description === "string"
          ? body.description.trim() : "",
      gitlab_repository_urls: Array.isArray(body.gitlab_repository_urls) ? body.gitlab_repository_urls : [],
      gitlab_backlog_project_urls: Array.isArray(body.gitlab_backlog_project_urls)
        ? body.gitlab_backlog_project_urls
        : [],
    });
    if (!updated) return new HttpResponse("not found", { status: 404 });
    return HttpResponse.json(updated);
  }),
  http.delete("*/project/projects/:projectId", ({ params }) => {
    const projectId = String(params.projectId ?? "");
    const ok = mockState.deleteProject(projectId);
    if (!ok) return new HttpResponse("not found", { status: 404 });
    return HttpResponse.json({ ok: true });
  }),

  http.post("*/project/projects/:projectId/documents/upload", async ({ params, request }) => {
    const projectId = String(params.projectId ?? "");
    const exists = mockState.get().projects.some((p) => p.id === projectId);
    if (!exists) return new HttpResponse("not found", { status: 404 });

    try {
      await request.formData();
    } catch {
      // ignore
    }

    // Emulate ingestion progress via SSE (neo4j ingestion service style).
    const steps = [
      "Upload received",
      "Extracting text",
      "Chunking documents",
      "Writing to Neo4j",
      "Building embeddings",
      "Ingestion complete",
    ];
    steps.forEach((text, idx) => {
      setTimeout(() => {
        emitSse("project_document_upload_progress", {
          project_id: projectId,
          thought_summary: text,
        });
      }, 150 + idx * 220);
    });

    return HttpResponse.json({ ok: true }, { status: 200 });
  }),

  http.post("*/project/projects/multi/cache-embeddings", ({ request }) => {
    const url = new URL(request.url);
    const projectId = url.searchParams.get("project_id") ?? "unknown";
    const ids = (url.searchParams.get("gitlab_project_ids") ?? "")
      .split(",")
      .map((x) => x.trim())
      .filter(Boolean);

    const steps = [
      `Caching embeddings for ${ids.length} project(s)…`,
      "Fetching issues/epics",
      "Computing vectors",
      "Upserting cache",
      "Embeddings cached",
    ];
    steps.forEach((text, idx) => {
      setTimeout(() => {
        emitSse("project_cache_backlog_progress", {
          project_id: projectId,
          thought_summary: text,
        });
      }, 100 + idx * 180);
    });

    return HttpResponse.json({ ok: true }, { status: 200 });
  }),

  http.post("*/code-graph/ingest/git", async ({ request }) => {
    let projectId = "unknown";
    try {
      const body = (await request.json()) as { project_id?: unknown };
      projectId = typeof body?.project_id === "string" ? body.project_id : projectId;
    } catch {
      // ignore
    }

    const steps = [
      "Repo ingest queued",
      "Cloning repository",
      "Parsing source files",
      "Writing graph",
      "Repo index complete",
    ];
    steps.forEach((text, idx) => {
      setTimeout(() => {
        emitSse("project_repo_index_progress", {
          project_id: projectId,
          thought_summary: text,
        });
      }, 120 + idx * 220);
    });
    return HttpResponse.json({ ok: true }, { status: 200 });
  }),

  http.post("*/code-graph/ingest/zip", async ({ request }) => {
    let projectId = "unknown";
    try {
      const fd = await request.formData();
      projectId = String(fd.get("project_id") ?? "unknown");
    } catch {
      // ignore
    }

    const steps = [
      "ZIP received",
      "Extracting archive",
      "Parsing source files",
      "Writing graph",
      "Repo index complete",
    ];
    steps.forEach((text, idx) => {
      setTimeout(() => {
        emitSse("project_repo_index_progress", {
          project_id: projectId,
          thought_summary: text,
        });
      }, 120 + idx * 220);
    });
    return HttpResponse.json({ ok: true }, { status: 200 });
  }),

  http.post("*/workflow/requirements", async ({ request }) => {
    const body = readStringRecord(await request.json().catch(() => ({})));
    const projectId = asNonEmptyString(body.project_id) ?? requirementsBundleFixture.project_id;
    const promptId = asNonEmptyString(body.prompt_id) ?? requirementsBundleFixture.prompt_id;

    emitAiThinkingSequence({ projectId, promptId, messageType: "ai_requirements_progress" });

    return HttpResponse.json({
      ...requirementsBundleFixture,
      project_id: projectId,
      prompt_id: promptId,
    });
  }),
  http.post("*/workflow/enhance", async ({ request }) => {
    const body = readStringRecord(await request.json().catch(() => ({})));
    const projectId = asNonEmptyString(body.project_id) ?? requirementsBundleFixture.project_id;
    const itemId = asNonEmptyString(body.requirement_id) ?? "unknown";

    // Emit a tiny enhancement progress burst; the UI listens under `ai_generation_progress`.
    setTimeout(() => {
      emitSse("ai_generation_progress", {
        project_id: projectId,
        message_type: "ai_requirements_progress",
        enhancement_mode: true,
        item_id: itemId,
        status: "running",
        details_md: "Enhancing…",
      });
    }, 40);
    setTimeout(() => {
      emitSse("ai_generation_progress", {
        project_id: projectId,
        message_type: "ai_requirements_progress",
        enhancement_mode: true,
        item_id: itemId,
        status: "done",
        details_md: "Enhanced",
      });
    }, 140);

    return HttpResponse.json(requirementsBundleFixture.business_requirements[0]);
  }),

  http.post("*/tasks/generate", async ({ request }) => {
    const body = readStringRecord(await request.json().catch(() => ({})));
    const projectId = asNonEmptyString(body.project_id) ?? backlogBundleFixture.project_id;
    const promptId = asNonEmptyString(body.prompt_id) ?? backlogBundleFixture.prompt_id;

    emitAiThinkingSequence({ projectId, promptId, messageType: "ai_tasks_progress" });

    return HttpResponse.json({
      ...backlogBundleFixture,
      project_id: projectId,
      prompt_id: promptId,
    });
  }),
  http.post("*/tasks/enhance", async ({ request }) => {
    const body = readStringRecord(await request.json().catch(() => ({})));
    const projectId = asNonEmptyString(body.project_id) ?? backlogBundleFixture.project_id;
    const itemId = asNonEmptyString(body.item_id) ?? enhancedTaskFixture.item_id;

    setTimeout(() => {
      emitSse("ai_generation_progress", {
        project_id: projectId,
        message_type: "ai_tasks_progress",
        enhancement_mode: true,
        item_id: itemId,
        status: "running",
        details_md: "Enhancing…",
      });
    }, 40);
    setTimeout(() => {
      emitSse("ai_generation_progress", {
        project_id: projectId,
        message_type: "ai_tasks_progress",
        enhancement_mode: true,
        item_id: itemId,
        status: "done",
        details_md: "Enhanced",
      });
    }, 140);

    return HttpResponse.json({ ...enhancedTaskFixture, item_id: itemId });
  }),

  http.post("*/gitlab/projects/apply-backlog", async () =>
    HttpResponse.json(applyBacklogResponseFixture),
  ),
];


