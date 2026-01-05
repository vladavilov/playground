import { afterEach, describe, expect, it } from "vitest";

import { useUiStore } from "./store";

afterEach(() => {
  useUiStore.getState().reset();
});

describe("ui store slices", () => {
  it("stores chat drafts by route key", () => {
    useUiStore.getState().setChatDraft("projects", "hello");
    useUiStore.getState().setChatDraft("tasks", "world");
    expect(useUiStore.getState().chatDraftByRouteKey).toEqual({
      projects: "hello",
      tasks: "world",
    });
  });

  it("tracks SSE status and per-item enhancement progress", () => {
    useUiStore.getState().setSseStatus("connecting");
    useUiStore.getState().appendThinking("prompt-1", "hello");
    useUiStore.getState().upsertEnhancementProgress("item-1", {
      status: "running",
      detailsMd: "step 1",
    });
    expect(useUiStore.getState().sseStatus).toBe("connecting");
    expect(useUiStore.getState().thinkingByPromptId["prompt-1"]).toEqual(["hello"]);
    expect(useUiStore.getState().enhancementProgressByItemId["item-1"]).toEqual({
      status: "running",
      detailsMd: "step 1",
    });
  });
});


