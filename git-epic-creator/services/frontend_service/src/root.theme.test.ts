// @vitest-environment node
import { describe, expect, it, vi } from "vitest";

import { action, loader } from "./root";

describe("root theme SSR behavior", () => {
  it("loader redirects to login when unauthenticated", async () => {
    vi.stubGlobal(
      "fetch",
      vi.fn(async (input: unknown) => {
        const url = new URL(String(input));
        if (url.pathname === "/auth/me") {
          return Response.json({ authenticated: false, username: null });
        }
        return new Response("unexpected", { status: 500 });
      }) as any,
    );

    const request = new Request("http://localhost/projects?x=1", {
      headers: { cookie: "ui_theme=dark" },
    });

    try {
      await loader({ request, params: {}, context: {} } as any);
      expect.unreachable("expected redirect throw");
    } catch (e: unknown) {
      expect(e).toBeInstanceOf(Response);
      const res = e as Response;
      expect(res.headers.get("Location")).toContain("/auth/login?redirect_uri=");
      expect(res.headers.get("Location")).toContain(
        encodeURIComponent("/projects?x=1"),
      );
    }
  });

  it("loader returns theme + auth + config + gitlab status when authenticated", async () => {
    vi.stubGlobal(
      "fetch",
      vi.fn(async (input: unknown, init?: unknown) => {
        const url = new URL(String(input));
        const headers = (init as { headers?: any } | undefined)?.headers;
        const cookie = headers?.get?.("cookie") ?? headers?.cookie;
        expect(cookie).toContain("ui_theme=dark");

        if (url.pathname === "/auth/me") {
          return Response.json({ authenticated: true, username: "alice" });
        }
        if (url.pathname === "/auth/gitlab/status") {
          return Response.json({ connected: false, configured: true });
        }
        return new Response("not found", { status: 404 });
      }) as any,
    );

    const request = new Request("http://localhost/projects", {
      headers: { cookie: "ui_theme=dark" },
    });

    const data = await loader({ request, params: {}, context: {} } as any);
    expect(data).toMatchObject({
      theme: "dark",
      auth: { authenticated: true, username: "alice" },
      gitlabStatus: { connected: false, configured: true },
    });
  });

  it("action sets cookie", async () => {
    const fd = new FormData();
    fd.set("theme", "dark");

    const request = new Request("http://localhost/", { method: "POST", body: fd });
    const res = (await action({ request, params: {}, context: {} } as any)) as Response;

    expect(res.status).toBe(200);
    const cookie = res.headers.get("Set-Cookie");
    expect(cookie).toContain("ui_theme=dark");
    expect(cookie).toContain("Path=/");
  });
});


