// @vitest-environment node
import { describe, expect, it, vi } from "vitest";

import { action, loader } from "./projects.$projectId.requirements";

describe("requirements route loader/action", () => {
  it("loader fetches project details", async () => {
    vi.stubGlobal(
      "fetch",
      vi.fn(async (input: unknown) => {
        const url = new URL(String(input));
        expect(url.pathname).toBe("/project/projects/p1");
        return Response.json({ id: "p1", name: "P1" });
      }) as any,
    );

    const request = new Request("http://localhost/projects/p1/requirements", {
      headers: { cookie: "a=b" },
    });
    const data = await loader({ request, params: { projectId: "p1" }, context: {} } as any);
    expect(data.project.name).toBe("P1");
  });

  it("action generate posts to /workflow/requirements", async () => {
    vi.stubGlobal(
      "fetch",
      vi.fn(async (input: unknown, init?: unknown) => {
        const url = new URL(String(input));
        expect(url.pathname).toBe("/workflow/requirements");
        expect((init as { method?: unknown } | undefined)?.method).toBe("POST");
        return Response.json({
          prompt_id: "pr",
          project_id: "p1",
          business_requirements: [],
          functional_requirements: [],
          assumptions: [],
          risks: [],
          score: 1,
        });
      }) as any,
    );

    const fd = new FormData();
    fd.set("intent", "generate");
    fd.set("prompt", "hello");

    const request = new Request("http://localhost/projects/p1/requirements", {
      method: "POST",
      body: fd,
      headers: { cookie: "a=b" },
    });

    const data = await action({ request, params: { projectId: "p1" }, context: {} } as any);
    expect(data && typeof data === "object" && "bundle" in data).toBe(true);
    const bundle = (data as { bundle: { project_id: string } }).bundle;
    expect(bundle.project_id).toBe("p1");
  });

  it("action generate throws 502 when backend response shape is invalid", async () => {
    vi.stubGlobal(
      "fetch",
      vi.fn(async () => Response.json({ nope: true })) as any,
    );

    const fd = new FormData();
    fd.set("intent", "generate");
    fd.set("prompt", "hello");

    const request = new Request("http://localhost/projects/p1/requirements", {
      method: "POST",
      body: fd,
      headers: { cookie: "a=b" },
    });

    await expect(action({ request, params: { projectId: "p1" }, context: {} } as any)).rejects.toMatchObject({
      status: 502,
    });
  });
});



