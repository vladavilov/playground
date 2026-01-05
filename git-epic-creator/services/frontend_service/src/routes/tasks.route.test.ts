// @vitest-environment node
import { describe, expect, it, vi } from "vitest";

import { action, loader } from "./projects.$projectId.tasks";

describe("tasks route loader/action", () => {
  it("loader fetches project details", async () => {
    vi.stubGlobal(
      "fetch",
      vi.fn(async (input: unknown) => {
        const url = new URL(String(input));
        expect(url.pathname).toBe("/project/projects/p1");
        return Response.json({ id: "p1", name: "P1", gitlab_backlog_project_ids: ["100"] });
      }) as any,
    );

    const request = new Request("http://localhost/projects/p1/tasks", {
      headers: { cookie: "a=b" },
    });
    const data = await loader({ request, params: { projectId: "p1" }, context: {} } as any);
    expect(data.project.name).toBe("P1");
  });

  it("action generate posts to /tasks/generate", async () => {
    vi.stubGlobal(
      "fetch",
      vi.fn(async (input: unknown, init?: unknown) => {
        const url = new URL(String(input));
        expect(url.pathname).toBe("/tasks/generate");
        expect((init as { method?: unknown } | undefined)?.method).toBe("POST");
        return Response.json({
          prompt_id: "pr",
          project_id: "p1",
          epics: [],
          assumptions: [],
          risks: [],
          score: 1,
        });
      }) as any,
    );

    const fd = new FormData();
    fd.set("intent", "generate");
    fd.set("message", "hello");

    const request = new Request("http://localhost/projects/p1/tasks", {
      method: "POST",
      body: fd,
      headers: { cookie: "a=b" },
    });

    const data = await action({ request, params: { projectId: "p1" }, context: {} } as any);
    expect(data && typeof data === "object" && "backlog" in data).toBe(true);
    const backlog = (data as { backlog: { prompt_id: string } }).backlog;
    expect(backlog.prompt_id).toBe("pr");
  });

  it("action generate throws 502 when backend response shape is invalid", async () => {
    vi.stubGlobal("fetch", vi.fn(async () => Response.json({ nope: true })) as any);

    const fd = new FormData();
    fd.set("intent", "generate");
    fd.set("message", "hello");

    const request = new Request("http://localhost/projects/p1/tasks", {
      method: "POST",
      body: fd,
      headers: { cookie: "a=b" },
    });

    await expect(action({ request, params: { projectId: "p1" }, context: {} } as any)).rejects.toMatchObject({
      status: 502,
    });
  });

  it("action submit posts to /gitlab/projects/apply-backlog and returns http response", async () => {
    vi.stubGlobal(
      "fetch",
      vi.fn(async (input: unknown, init?: unknown) => {
        const url = new URL(String(input));
        expect(url.pathname).toBe("/gitlab/projects/apply-backlog");
        expect((init as { method?: unknown } | undefined)?.method).toBe("POST");
        return Response.json({ created: 2 }, { status: 201 });
      }) as any,
    );

    const fd = new FormData();
    fd.set("intent", "submit");
    fd.set("prompt_id", "pr");
    fd.set("default_gitlab_project_id", "100");
    fd.set(
      "epics_json",
      JSON.stringify([
        {
          id: "e1",
          title: "Epic 1",
          description: "Desc",
          tasks: [
            {
              id: "t1",
              title: "Task 1",
              description: "TDesc",
              acceptance_criteria: [],
              dependencies: [],
            },
          ],
        },
      ]),
    );

    const request = new Request("http://localhost/projects/p1/tasks", {
      method: "POST",
      body: fd,
      headers: { cookie: "a=b" },
    });

    const data = await action({ request, params: { projectId: "p1" }, context: {} } as any);
    expect(data && typeof data === "object" && "submitStatus" in data).toBe(true);
    const submit = data as { submitStatus: number; submitOk: boolean; submitBody: unknown };
    expect(submit.submitStatus).toBe(201);
    expect(submit.submitOk).toBe(true);
    expect(submit.submitBody).toEqual({ created: 2 });
  });

  it("action submit returns ok:false for malformed epics_json", async () => {
    vi.stubGlobal("fetch", vi.fn(async () => new Response("unexpected", { status: 500 })) as any);

    const fd = new FormData();
    fd.set("intent", "submit");
    fd.set("prompt_id", "pr");
    fd.set("default_gitlab_project_id", "100");
    fd.set(
      "epics_json",
      JSON.stringify([
        {
          id: "e1",
          title: "Epic 1",
          description: "Desc",
          tasks: [{ id: "t1", title: "Task 1" }], // missing required fields
        },
      ]),
    );

    const request = new Request("http://localhost/projects/p1/tasks", {
      method: "POST",
      body: fd,
      headers: { cookie: "a=b" },
    });

    const data = await action({ request, params: { projectId: "p1" }, context: {} } as any);
    expect(data).toEqual({ ok: false });
  });
});



