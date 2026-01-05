export const SSE_EVENTS = {
  hello: "hello",
  project: {
    documentUploadProgress: "project_document_upload_progress",
    cacheBacklogProgress: "project_cache_backlog_progress",
    repoIndexProgress: "project_repo_index_progress",
  },
  aiGenerationProgress: "ai_generation_progress",
} as const;

export type SseEventName =
  | typeof SSE_EVENTS.hello
  | (typeof SSE_EVENTS.project)[keyof typeof SSE_EVENTS.project]
  | typeof SSE_EVENTS.aiGenerationProgress;

export const SSE_PROGRESS_EVENTS: readonly SseEventName[] = [
  SSE_EVENTS.project.documentUploadProgress,
  SSE_EVENTS.project.cacheBacklogProgress,
  SSE_EVENTS.project.repoIndexProgress,
  SSE_EVENTS.aiGenerationProgress,
] as const;


