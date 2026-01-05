import { describe, expect, it } from "vitest";

import { routeSseMessage } from "./sse";
import { SSE_EVENTS } from "./channels";

describe("routeSseMessage", () => {
  it("ignores messages for other projects", () => {
    expect(
      routeSseMessage({
        projectId: "a",
        eventName: SSE_EVENTS.aiGenerationProgress,
        raw: { project_id: "b", message_type: "ai_requirements_progress" },
      }),
    ).toEqual({ kind: "ignore" });
  });

  it("routes enhancement_mode item progress into a generic enhancement update", () => {
    expect(
      routeSseMessage({
        projectId: "p",
        eventName: SSE_EVENTS.aiGenerationProgress,
        raw: {
          project_id: "p",
          message_type: "ai_requirements_progress",
          enhancement_mode: true,
          item_id: "i",
          status: "s",
        },
      }),
    ).toEqual({
      kind: "ai_enhancement",
      itemId: "i",
      status: "s",
      detailsMd: undefined,
      promptId: undefined,
    });

    expect(
      routeSseMessage({
        projectId: "p",
        eventName: SSE_EVENTS.aiGenerationProgress,
        raw: {
          project_id: "p",
          message_type: "ai_tasks_progress",
          enhancement_mode: true,
          item_id: "t",
          status: "running",
        },
      }),
    ).toEqual({
      kind: "ai_enhancement",
      itemId: "t",
      status: "running",
      detailsMd: undefined,
      promptId: undefined,
    });
  });

  it("routes ai generation messages into a generic thinking update by prompt_id", () => {
    expect(
      routeSseMessage({
        projectId: "p",
        eventName: SSE_EVENTS.aiGenerationProgress,
        raw: { project_id: "p", message_type: "ai_requirements_progress", prompt_id: "pr", thought_summary: "hello" },
      }),
    ).toEqual({ kind: "ai_thinking", promptId: "pr", text: "hello" });

    expect(
      routeSseMessage({
        projectId: "p",
        eventName: SSE_EVENTS.aiGenerationProgress,
        raw: { project_id: "p", message_type: "ai_tasks_progress", prompt_id: "pr2", thought_summary: "hi" },
      }),
    ).toEqual({ kind: "ai_thinking", promptId: "pr2", text: "hi" });
  });

  it("routes retrieval progress (prompt-scoped) into chat thinking by prompt_id", () => {
    expect(
      routeSseMessage({
        projectId: "p",
        eventName: SSE_EVENTS.aiGenerationProgress,
        raw: { project_id: "p", message_type: "retrieval_progress", prompt_id: "pr", thought_summary: "retrieving" },
      }),
    ).toEqual({ kind: "ai_thinking", promptId: "pr", text: "retrieving" });
  });

  it("routes repo index progress into the repo_index progress kind", () => {
    expect(
      routeSseMessage({
        projectId: "p",
        eventName: SSE_EVENTS.project.repoIndexProgress,
        raw: {
          project_id: "p",
          message_type: "project_progress",
          status: "rag_processing",
          process_step: "Indexing",
          processed_pct: 10,
        },
      }),
    ).toEqual({ kind: "repo_index_progress", text: "rag_processing · Indexing · 10%" });
  });
});



