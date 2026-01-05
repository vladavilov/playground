import { create } from "zustand";
import { devtools } from "zustand/middleware";

export type SseStatus = "disconnected" | "connecting" | "connected";

export type EnhancementProgress = {
  status: string;
  detailsMd?: string;
};

export type SseLogLine = {
  at: number;
  text: string;
};

export type ProgressFeature = "upload_documents" | "cache_embeddings" | "repo_index";

type DraftSlice = {
  requirementsDraft: string;
  setRequirementsDraft: (v: string) => void;
  tasksDraft: string;
  setTasksDraft: (v: string) => void;
  chatDraftByRouteKey: Record<string, string>;
  setChatDraft: (routeKey: string, v: string) => void;
};

type UiSlice = {
  isLeftPanelOpen: boolean;
  setLeftPanelOpen: (v: boolean) => void;
  focusItemId: string | null;
  setFocusItemId: (id: string | null) => void;
};

type SseSlice = {
  sseStatus: SseStatus;
  setSseStatus: (s: SseStatus) => void;
  lastEventAt: number | null;
  setLastEventAt: (t: number | null) => void;
  activePromptId: string | null;
  setActivePromptId: (id: string | null) => void;
  activeMessageId: string | null;
  setActiveMessageId: (id: string | null) => void;
  thinkingByPromptId: Record<string, string[]>;
  appendThinking: (promptId: string, text: string) => void;
  /**
   * Per-feature progress logs (Projects page panels).
   *
   * SSE events are already separated by operation, so we keep only the higher-level
   * feature buckets required by the UI.
   */
  progressLogByFeature: Record<ProgressFeature, SseLogLine[]>;
  appendProgressLog: (feature: ProgressFeature, text: string) => void;
  clearProgressLog: (feature: ProgressFeature) => void;
  enhancementProgressByItemId: Record<string, EnhancementProgress>;
  upsertEnhancementProgress: (itemId: string, p: EnhancementProgress) => void;
};

export type UiStore = DraftSlice & UiSlice & SseSlice & { reset: () => void };

const initialState = {
  requirementsDraft: "",
  tasksDraft: "",
  chatDraftByRouteKey: {},
  isLeftPanelOpen: true,
  focusItemId: null,
  sseStatus: "disconnected" as SseStatus,
  lastEventAt: null as number | null,
  activePromptId: null as string | null,
  activeMessageId: null as string | null,
  thinkingByPromptId: {} as Record<string, string[]>,
  progressLogByFeature: {
    upload_documents: [],
    cache_embeddings: [],
    repo_index: [],
  } satisfies Record<ProgressFeature, SseLogLine[]>,
  enhancementProgressByItemId: {} as Record<string, EnhancementProgress>,
};

export const useUiStore = create<UiStore>()(
  devtools((set) => ({
    ...initialState,

    setRequirementsDraft: (v) => set({ requirementsDraft: v }),
    setTasksDraft: (v) => set({ tasksDraft: v }),
    setChatDraft: (routeKey, v) =>
      set((st) => ({
        chatDraftByRouteKey: { ...st.chatDraftByRouteKey, [routeKey]: v },
      })),

    setLeftPanelOpen: (v) => set({ isLeftPanelOpen: v }),
    setFocusItemId: (id) => set({ focusItemId: id }),

    setSseStatus: (s) => set({ sseStatus: s }),
    setLastEventAt: (t) => set({ lastEventAt: t }),
    setActivePromptId: (id) => set({ activePromptId: id }),
    setActiveMessageId: (id) => set({ activeMessageId: id }),
    appendThinking: (promptId, text) =>
      set((st) => ({
        thinkingByPromptId: {
          ...st.thinkingByPromptId,
          [promptId]: [...(st.thinkingByPromptId[promptId] ?? []), text],
        },
      })),
    appendProgressLog: (feature, text) =>
      set((st) => {
        const prev = st.progressLogByFeature[feature] ?? [];
        const next = [...prev, { at: Date.now(), text }];
        const trimmed = next.length > 200 ? next.slice(next.length - 200) : next;
        return {
          progressLogByFeature: {
            ...st.progressLogByFeature,
            [feature]: trimmed,
          },
        };
      }),
    clearProgressLog: (feature) =>
      set((st) => ({
        progressLogByFeature: { ...st.progressLogByFeature, [feature]: [] },
      })),
    upsertEnhancementProgress: (itemId, p) =>
      set((st) => ({
        enhancementProgressByItemId: {
          ...st.enhancementProgressByItemId,
          [itemId]: p,
        },
      })),

    reset: () => set({ ...initialState }),
  })),
);


