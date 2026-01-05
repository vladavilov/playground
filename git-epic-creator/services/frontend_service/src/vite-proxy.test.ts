// @vitest-environment node
import { describe, expect, it } from "vitest";

import viteConfig from "../vite.config";

describe("vite proxy wiring", () => {
  it("proxies gateway routes to sse-bridge-service origin", () => {
    const cfg = viteConfig as { server?: { proxy?: Record<string, unknown> } };

    const proxy = cfg.server?.proxy;
    expect(proxy).toBeTruthy();

    const keys = Object.keys(proxy ?? {});
    expect(keys).toEqual(
      expect.arrayContaining([
        "/auth",
        "/project/",
        "/workflow",
        "/tasks",
        "/gitlab",
        "/events",
      ]),
    );
  });
});


