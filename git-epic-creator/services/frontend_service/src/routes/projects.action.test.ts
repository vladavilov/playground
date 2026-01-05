// @vitest-environment node
import { describe, expect, it, vi } from "vitest";

import { action, loader } from "./projects";

describe("projects route loader/action", () => {
  it("loader fetches /project/projects", async () => {
    vi.stubGlobal(
      "fetch",
      vi.fn(async (input: unknown) => {
        const url = new URL(String(input));
        expect(url.pathname).toBe("/project/projects");
        return Response.json([
          {
            id: "p",
            name: "n",
            status: "active",
            created_at: "t",
            updated_at: "t",
          },
        ]);
      }) as any,
    );

    const request = new Request("http://localhost/projects", { headers: { cookie: "x=y" } });
    const data = await loader({ request, params: {}, context: {} } as any);
    expect(data.projects).toHaveLength(1);
  });

  it("action create posts JSON to /project/projects", async () => {
    const fetchSpy = vi.fn(async (input: unknown, init?: unknown) => {
      const url = new URL(String(input));
      expect(url.pathname).toBe("/project/projects");
      expect((init as { method?: unknown } | undefined)?.method).toBe("POST");
      const headers = (init as { headers?: any } | undefined)?.headers;
      expect(headers?.get?.("cookie") ?? headers?.cookie).toContain("x=y");
      return Response.json({ id: "p" }, { status: 201 });
    });
    vi.stubGlobal("fetch", fetchSpy as any);

    const fd = new FormData();
    fd.set("intent", "create");
    fd.set("name", "My Project");

    const request = new Request("http://localhost/projects", { method: "POST", body: fd, headers: { cookie: "x=y" } });
    const data = await action({ request, params: {}, context: {} } as any);
    expect(data).toEqual({ ok: true });
  });

  it("action indexRepo posts JSON to /code-graph/ingest/git", async () => {
    const fetchSpy = vi.fn(async (input: unknown, init?: unknown) => {
      const url = new URL(String(input));
      expect(url.pathname).toBe("/code-graph/ingest/git");
      const initObj = init as { method?: unknown; headers?: any; body?: unknown } | undefined;
      expect(initObj?.method).toBe("POST");
      expect(initObj?.headers?.get?.("content-type") ?? initObj?.headers?.["Content-Type"]).toContain(
        "application/json",
      );
      const body = JSON.parse(String(initObj?.body ?? ""));
      expect(body).toEqual({
        project_id: "p1",
        source_language: "java",
        git_url: "https://gitlab.example.com/group/repo",
      });
      return Response.json({ ok: true }, { status: 200 });
    });
    vi.stubGlobal("fetch", fetchSpy as any);

    const fd = new FormData();
    fd.set("intent", "indexRepo");
    fd.set("projectId", "p1");
    fd.set("repoUrl", "https://gitlab.example.com/group/repo");
    fd.set("source_language", "java");
    const request = new Request("http://localhost/projects", {
      method: "POST",
      body: fd,
      headers: { cookie: "x=y" },
    });

    const data = await action({ request, params: {}, context: {} } as any);
    expect(data).toEqual({ ok: true });
  });

  it("action uploadRepoZip forwards multipart to /code-graph/ingest/zip with mapped fields", async () => {
    const fetchSpy = vi.fn(async (input: unknown, init?: unknown) => {
      const url = new URL(String(input));
      expect(url.pathname).toBe("/code-graph/ingest/zip");
      const initObj = init as { method?: unknown; body?: unknown } | undefined;
      expect(initObj?.method).toBe("POST");
      expect(initObj?.body).toBeInstanceOf(FormData);

      const outgoing = initObj?.body as FormData;
      expect(outgoing.get("project_id")).toBe("p1");
      expect(outgoing.get("source_language")).toBe("cobol");
      expect(outgoing.get("file")).toBeTruthy();

      return Response.json({ ok: true }, { status: 200 });
    });
    vi.stubGlobal("fetch", fetchSpy as any);

    const fd = new FormData();
    fd.set("intent", "uploadRepoZip");
    fd.set("projectId", "p1");
    fd.set("source_language", "cobol");
    fd.set("file", new Blob([Uint8Array.from([0x50, 0x4b, 0x03, 0x04])], { type: "application/zip" }), "repo.zip");

    const request = new Request("http://localhost/projects", {
      method: "POST",
      body: fd,
      headers: { cookie: "x=y" },
    });

    const data = await action({ request, params: {}, context: {} } as any);
    expect(data).toEqual({ ok: true });
  });
});



