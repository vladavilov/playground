import React from "react";
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import { render, waitFor } from "@testing-library/react";

import { Mermaid } from "./Mermaid";

vi.mock("mermaid", () => {
  return {
    default: {
      initialize: vi.fn(),
      run: vi.fn(async ({ nodes }: { nodes: HTMLElement[] }) => {
        const node = nodes[0];
        if (!node) return;
        // Simulate Mermaid writing SVG into the node.
        node.innerHTML = "<svg id=\"mermaid-svg\"></svg>";
      }),
    },
  };
});

describe("Mermaid", () => {
  function cleanupLeakedNodes() {
    document
      .querySelectorAll('[id^="dmermaid-"], [id^="mermaid-"]')
      .forEach((el) => el.remove());
  }

  // Mermaid is async and our mock counts calls across tests; ensure isolation.
  beforeEach(() => {
    vi.clearAllMocks();
    cleanupLeakedNodes();
  });

  afterEach(() => {
    cleanupLeakedNodes();
  });

  it("does not leave Mermaid render artifacts in document.body", async () => {
    render(<Mermaid code={"graph TD;\nA-->B"} />);

    await waitFor(() => {
      // Mermaid component should always clean up any dmermaid-* nodes that Mermaid created.
      const leaked = document.body.querySelectorAll('[id^="dmermaid-"]');
      expect(leaked.length).toBe(0);
      expect(document.querySelector("#mermaid-svg")).toBeTruthy();
    });
  });

  it("cleans up artifacts when render() throws (prevents Mermaid error SVG injection)", async () => {
    const mermaidMod = await import("mermaid");
    (mermaidMod.default.run as unknown as ReturnType<typeof vi.fn>).mockImplementationOnce(async () => {
      // Simulate mermaid leaving a temp node and then throwing.
      const tmp = document.createElement("div");
      tmp.id = "dmermaid-test";
      document.body.appendChild(tmp);
      throw new Error("render error");
    });

    render(<Mermaid code={"this is not mermaid"} />);

    await waitFor(() => {
      const leaked = document.body.querySelectorAll('[id^="dmermaid-"]');
      expect(leaked.length).toBe(0);
    });
  });
});


