import React from "react";
import { afterEach, describe, expect, it, vi } from "vitest";
import { render } from "@testing-library/react";

import { useUiStore } from "../state/store";
import { useSseBridge } from "./useSseBridge";
import { SSE_EVENTS } from "./channels";

type Handler = (evt: MessageEvent) => void;

class FakeEventSource {
  static instances: FakeEventSource[] = [];
  handlers = new Map<string, Handler[]>();
  closed = false;
  url: string;

  constructor(url: string) {
    this.url = url;
    FakeEventSource.instances.push(this);
  }

  addEventListener(type: string, listener: Handler) {
    const arr = this.handlers.get(type) ?? [];
    arr.push(listener);
    this.handlers.set(type, arr);
  }

  emit(type: string, data: unknown, opts?: { lastEventId?: string }) {
    const arr = this.handlers.get(type) ?? [];
    for (const fn of arr) fn({ data: JSON.stringify(data), lastEventId: opts?.lastEventId ?? "" } as any);
  }

  close() {
    this.closed = true;
  }
}

function Harness(props: { projectId: string | null }) {
  useSseBridge(props.projectId);
  return null;
}

afterEach(() => {
  useUiStore.getState().reset();
  FakeEventSource.instances = [];
  vi.unstubAllGlobals();
});

describe("useSseBridge", () => {
  it("routes enhancement_mode item progress into the store", async () => {
    vi.stubGlobal("EventSource", FakeEventSource as any);
    render(<Harness projectId="p" />);

    const es = FakeEventSource.instances[0]!;
    es.emit(SSE_EVENTS.aiGenerationProgress, {
      project_id: "p",
      message_type: "ai_tasks_progress",
      enhancement_mode: true,
      item_id: "item-1",
      status: "running",
      details_md: "hello",
    });

    expect(useUiStore.getState().enhancementProgressByItemId["item-1"]).toEqual({
      status: "running",
      detailsMd: "hello",
    });
  });

  it("routes non-enhancement messages into thinking stream by prompt_id", async () => {
    vi.stubGlobal("EventSource", FakeEventSource as any);
    render(<Harness projectId="p" />);

    const es = FakeEventSource.instances[0]!;
    es.emit(SSE_EVENTS.aiGenerationProgress, {
      project_id: "p",
      message_type: "ai_requirements_progress",
      prompt_id: "prompt-1",
      thought_summary: "step 1",
    });

    expect(useUiStore.getState().thinkingByPromptId["prompt-1"]).toEqual(["step 1"]);
  });

  it("de-dupes duplicate SSE deliveries by lastEventId", async () => {
    vi.stubGlobal("EventSource", FakeEventSource as any);
    render(<Harness projectId="p" />);

    const es = FakeEventSource.instances[0]!;
    const payload = {
      project_id: "p",
      message_type: "ai_requirements_progress",
      prompt_id: "prompt-1",
      thought_summary: "step 1",
      event_at: 123,
    };

    es.emit(SSE_EVENTS.aiGenerationProgress, payload, { lastEventId: "42" });
    es.emit(SSE_EVENTS.aiGenerationProgress, payload, { lastEventId: "42" });

    expect(useUiStore.getState().thinkingByPromptId["prompt-1"]).toEqual(["step 1"]);
  });

  it("de-dupes duplicate SSE deliveries by event_at when lastEventId is missing", async () => {
    vi.stubGlobal("EventSource", FakeEventSource as any);
    render(<Harness projectId="p" />);

    const es = FakeEventSource.instances[0]!;
    const payload = {
      project_id: "p",
      message_type: "ai_requirements_progress",
      prompt_id: "prompt-1",
      thought_summary: "step 1",
      event_at: 123,
    };

    es.emit(SSE_EVENTS.aiGenerationProgress, payload);
    es.emit(SSE_EVENTS.aiGenerationProgress, payload);

    expect(useUiStore.getState().thinkingByPromptId["prompt-1"]).toEqual(["step 1"]);
  });

  it("resets de-dupe state when switching projects", async () => {
    vi.stubGlobal("EventSource", FakeEventSource as any);
    const r = render(<Harness projectId="p1" />);

    const es1 = FakeEventSource.instances[0]!;
    es1.emit(
      SSE_EVENTS.aiGenerationProgress,
      {
        project_id: "p1",
        message_type: "ai_requirements_progress",
        prompt_id: "prompt-a",
        thought_summary: "a1",
      },
      { lastEventId: "1" },
    );
    expect(useUiStore.getState().thinkingByPromptId["prompt-a"]).toEqual(["a1"]);

    r.rerender(<Harness projectId="p2" />);
    const es2 = FakeEventSource.instances[1]!;
    // Reuse token "1" to mimic a server restart or a new stream with overlapping ids.
    es2.emit(
      SSE_EVENTS.aiGenerationProgress,
      {
        project_id: "p2",
        message_type: "ai_requirements_progress",
        prompt_id: "prompt-b",
        thought_summary: "b1",
      },
      { lastEventId: "1" },
    );
    expect(useUiStore.getState().thinkingByPromptId["prompt-b"]).toEqual(["b1"]);
  });
});



