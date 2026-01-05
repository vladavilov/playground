import { SSE_EVENTS, type SseEventName } from "./channels";

export type SseMessage = {
  project_id?: string;
  prompt_id?: string;
  enhancement_mode?: boolean;
  item_id?: string;
  status?: string;
  details_md?: string;
  thought_summary?: string;
  // project_progress (unified redis message)
  message_type?: string;
  process_step?: string | null;
  processed_pct?: number | null;
  processed_count?: number | null;
  total_count?: number | null;
};

export type SseUpdate =
  | {
      kind: "ignore";
    }
  | {
      kind: "ai_enhancement";
      itemId: string;
      status: string;
      detailsMd?: string;
      promptId?: string;
    }
  | {
      kind: "ai_thinking";
      promptId: string;
      text: string;
    }
  | {
      kind: "upload_documents_progress";
      text: string;
    }
  | {
      kind: "cache_embeddings_progress";
      text: string;
    }
  | {
      kind: "repo_index_progress";
      text: string;
    };

function buildProjectProgressText(msg: SseMessage): string {
  const pct =
    typeof msg.processed_pct === "number" && Number.isFinite(msg.processed_pct)
      ? `${Math.round(msg.processed_pct)}%`
      : null;
  const counts =
    typeof msg.processed_count === "number" &&
    typeof msg.total_count === "number" &&
    Number.isFinite(msg.processed_count) &&
    Number.isFinite(msg.total_count)
      ? `${msg.processed_count}/${msg.total_count}`
      : null;
  const step = typeof msg.process_step === "string" ? msg.process_step.trim() : "";
  const status = typeof msg.status === "string" ? msg.status.trim() : "project_progress";
  const parts = [status, step, pct, counts].filter((x) => Boolean(x && String(x).trim()));
  return parts.join(" · ");
}

export function routeSseMessage(args: {
  projectId: string;
  eventName: SseEventName;
  raw: unknown;
}): SseUpdate {
  if (!args.raw || typeof args.raw !== "object") return { kind: "ignore" };
  const msg = args.raw as SseMessage;

  if (String(msg.project_id ?? "") !== String(args.projectId)) {
    return { kind: "ignore" };
  }

  const details = msg.details_md ?? msg.thought_summary;

  // Projects page: progress events are split by operation.
  if (args.eventName === SSE_EVENTS.project.documentUploadProgress) {
    const text = details ? String(details) : buildProjectProgressText(msg);
    return { kind: "upload_documents_progress", text: text || "upload" };
  }
  if (args.eventName === SSE_EVENTS.project.cacheBacklogProgress) {
    const text = details ? String(details) : buildProjectProgressText(msg);
    return { kind: "cache_embeddings_progress", text: text || "cache_embeddings" };
  }
  if (args.eventName === SSE_EVENTS.project.repoIndexProgress) {
    const text = details ? String(details) : buildProjectProgressText(msg);
    return { kind: "repo_index_progress", text: text || "repo_index" };
  }

  // AI generation: single SSE event, routed by message_type + prompt_id.
  if (args.eventName === SSE_EVENTS.aiGenerationProgress) {
    const msgType = typeof msg.message_type === "string" ? msg.message_type : null;
    const promptId = msg.prompt_id ? String(msg.prompt_id) : null;

    if (msgType === "ai_requirements_progress") {
      if (msg.enhancement_mode && msg.item_id) {
        return {
          kind: "ai_enhancement",
          itemId: String(msg.item_id),
          status: msg.status ?? "unknown",
          detailsMd: details,
          promptId: promptId ?? undefined,
        };
      }
      // Prompt-id is the routing key for chat thinking. If it's missing, ignore
      // to avoid polluting unrelated chat streams.
      if (!promptId) return { kind: "ignore" };
      return {
        kind: "ai_thinking",
        promptId,
        text: details ? String(details) : "",
      };
    }

    if (msgType === "ai_tasks_progress") {
      if (msg.enhancement_mode && msg.item_id) {
        return {
          kind: "ai_enhancement",
          itemId: String(msg.item_id),
          status: msg.status ?? "unknown",
          detailsMd: details,
          promptId: promptId ?? undefined,
        };
      }
      if (!promptId) return { kind: "ignore" };
      return {
        kind: "ai_thinking",
        promptId,
        text: details ? String(details) : "",
      };
    }

    // Retrieval progress during AI runs should be shown in the thinking stream (prompt-scoped).
    if (msgType === "retrieval_progress") {
      if (!promptId) return { kind: "ignore" };
      return {
        kind: "ai_thinking",
        promptId,
        text: details ? String(details) : buildProjectProgressText(msg),
      };
    }
  }

  return { kind: "ignore" };
}



