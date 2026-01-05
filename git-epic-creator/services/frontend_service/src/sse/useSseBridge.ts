import { useEffect, useRef } from "react";

import { useUiStore } from "../state/store";
import { routeSseMessage } from "./sse";
import { SSE_EVENTS, SSE_PROGRESS_EVENTS, type SseEventName } from "./channels";
import { isRecord } from "../lib/guards";

type EventSourceLike = {
  addEventListener: (type: string, listener: (ev: MessageEvent) => void) => void;
  close: () => void;
};

function createEventSource(url: string): EventSourceLike {
  return new EventSource(url);
}

function safeParseJson(raw: string): unknown | null {
  try {
    return JSON.parse(raw) as unknown;
  } catch {
    return null;
  }
}

function readLastEventId(evt: MessageEvent): string {
  // EventSource messages carry `lastEventId`, but TypeScript doesn't model it on `MessageEvent`.
  const rec = evt as unknown as Record<string, unknown>;
  const v = rec["lastEventId"];
  return typeof v === "string" ? v : "";
}

function readEventAtToken(raw: unknown): string {
  if (!isRecord(raw)) return "";
  if (!("event_at" in raw)) return "";
  const v = raw.event_at;
  if (typeof v === "number" && Number.isFinite(v)) return String(v);
  if (typeof v === "string" && v.trim() && v !== "undefined") return v.trim();
  return "";
}

export function useSseBridge(projectId: string | null) {
  const setSseStatus = useUiStore((s) => s.setSseStatus);
  const setLastEventAt = useUiStore((s) => s.setLastEventAt);
  const setActivePromptId = useUiStore((s) => s.setActivePromptId);
  const upsertEnhancementProgress = useUiStore((s) => s.upsertEnhancementProgress);
  const appendThinking = useUiStore((s) => s.appendThinking);
  const appendProgressLog = useUiStore((s) => s.appendProgressLog);

  const lastSeenByEventRef = useRef<Record<string, string>>({});

  useEffect(() => {
    if (!projectId) return;

    setSseStatus("connecting");
    // De-dupe tokens must be per-project; otherwise switching projects can drop legitimate events.
    lastSeenByEventRef.current = {};

    const es = createEventSource("/events");

    es.addEventListener("open", () => setSseStatus("connected"));
    es.addEventListener("error", () => setSseStatus("connecting"));
    es.addEventListener(SSE_EVENTS.hello, () => setSseStatus("connected"));

    const handlerFor = (eventName: SseEventName) => (evt: MessageEvent) => {
      const raw = safeParseJson(evt.data);
      if (raw === null) return;

      // De-dupe at the transport boundary:
      // - Prefer SSE `id:` (available as `lastEventId`)
      // - Fallback to backend-provided `event_at`
      const lastEventId = readLastEventId(evt);
      const eventAt = readEventAtToken(raw);
      const token = lastEventId || eventAt;
      if (token) {
        const prev = lastSeenByEventRef.current[eventName];
        if (prev === token) return;
        lastSeenByEventRef.current[eventName] = token;
      }

      setLastEventAt(Date.now());
      const update = routeSseMessage({
        projectId,
        eventName,
        raw,
      });
      if (update.kind === "ignore") return;

      // Progress logs (Projects page panels) — only touch the progress log store.
      if (
        update.kind === "upload_documents_progress" ||
        update.kind === "cache_embeddings_progress" ||
        update.kind === "repo_index_progress"
      ) {
        // Feature is derived from kind; fall back to upload_documents.
        const feature =
          update.kind === "cache_embeddings_progress"
            ? "cache_embeddings"
            : update.kind === "repo_index_progress"
              ? "repo_index"
              : "upload_documents";
        appendProgressLog(feature, update.text);
        return;
      }

      // Enhancements — only touch enhancement progress store.
      if (update.kind === "ai_enhancement") {
        if (update.promptId) setActivePromptId(update.promptId);
        upsertEnhancementProgress(update.itemId, {
          status: update.status,
          detailsMd: update.detailsMd,
        });
        return;
      }

      // Thinking — only touch the thinking stream store.
      if (update.kind === "ai_thinking") {
        // Used by multiple routes to select the most recent prompt stream.
        setActivePromptId(update.promptId);
        appendThinking(update.promptId, update.text);
        return;
      }
    };

    for (const eventName of SSE_PROGRESS_EVENTS) {
      es.addEventListener(eventName, handlerFor(eventName));
    }

    return () => {
      es.close();
      setSseStatus("disconnected");
    };
  }, [
    projectId,
    setSseStatus,
    setLastEventAt,
    setActivePromptId,
    upsertEnhancementProgress,
    appendThinking,
    appendProgressLog,
  ]);
}


