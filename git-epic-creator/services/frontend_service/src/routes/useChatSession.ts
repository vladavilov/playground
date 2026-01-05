import { useCallback, useEffect, useMemo, useRef, useState } from "react";

import type { ChatMessage, ChatTextMessage, ChatThinkingMessage } from "../components/chat/chat.types";

type FetcherLike<TData> = {
  state: string;
  data: TData | undefined;
  // `useFetcher().submit()` options are a superset of this, but we only need the HTTP verb.
  submit: (
    formData: FormData,
    opts?: { method?: "get" | "post" | "put" | "patch" | "delete" },
  ) => void;
};

type AppendText = (m: Omit<ChatTextMessage, "id" | "createdAt">) => void;

export function useChatSession<TServerData, TResult>(args: {
  initialSystemMessage: string;
  draft: string;
  setDraft: (v: string) => void;
  generateFetcher: FetcherLike<TServerData>;
  buildSubmitFormData: (text: string, promptId: string | null) => FormData;
  extractResult: (data: TServerData | undefined) => TResult | null;
  getResultPromptId: (result: TResult) => string | null;
  onResetForNewSend?: () => void;
  onResult: (ctx: { result: TResult; appendText: AppendText }) => void;
  activePromptId: string | null;
  thinkingByPromptId: Record<string, string[]>;
}) {
  const [messages, setMessages] = useState<ChatMessage[]>([]);
  const [promptId, setPromptId] = useState<string | null>(null);
  const [thinkingMessageId, setThinkingMessageId] = useState<string | null>(null);
  const msgSeq = useRef(0);

  const appendText = useCallback<AppendText>((m) => {
    const id = `m-${msgSeq.current++}`;
    setMessages((prev) => [...prev, { ...m, id, createdAt: Date.now() }]);
  }, []);

  // Init system message exactly once per mount.
  useEffect(() => {
    if (messages.length) return;
    setMessages([
      {
        id: "sys-0",
        kind: "text",
        role: "system",
        content: args.initialSystemMessage,
        createdAt: 0,
      },
    ]);
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, []);

  const thinkingMarkdown = useMemo(() => {
    const pid = args.activePromptId ?? promptId ?? "";
    const items = pid ? (args.thinkingByPromptId[pid] ?? []) : [];
    return items.join("\n\n");
  }, [args.activePromptId, args.thinkingByPromptId, promptId]);

  const send = useCallback(
    (text: string) => {
      const trimmed = text.trim();
      if (!trimmed) return;

      appendText({ kind: "text", role: "user", content: trimmed });
      args.setDraft("");
      args.onResetForNewSend?.();

      const tid = `thinking-${msgSeq.current++}`;
      setThinkingMessageId(tid);
      const thinkingMsg: ChatThinkingMessage = {
        kind: "thinking",
        id: tid,
        title: "Thought process",
        markdown: "",
        collapsed: false,
        createdAt: Date.now(),
      };
      setMessages((prev) => [...prev, thinkingMsg]);

      const fd = args.buildSubmitFormData(trimmed, promptId);
      args.generateFetcher.submit(fd, { method: "post" });
    },
    [appendText, args, promptId],
  );

  // Keep thinking message in sync with SSE stream text.
  useEffect(() => {
    if (!thinkingMessageId) return;
    setMessages((prev) =>
      prev.map((m) =>
        m.kind === "thinking" && m.id === thinkingMessageId ? { ...m, markdown: thinkingMarkdown } : m,
      ),
    );
  }, [thinkingMarkdown, thinkingMessageId]);

  // Apply server result once per prompt id.
  const lastHandledPromptIdRef = useRef<string | null>(null);
  useEffect(() => {
    const result = args.extractResult(args.generateFetcher.data);
    if (!result) return;

    const nextPromptId = args.getResultPromptId(result);
    if (nextPromptId && lastHandledPromptIdRef.current === nextPromptId) return;
    if (nextPromptId) lastHandledPromptIdRef.current = nextPromptId;
    if (nextPromptId && nextPromptId !== promptId) setPromptId(nextPromptId);

    // Collapse thought process right before emitting assistant response.
    if (thinkingMessageId) {
      setMessages((prev) =>
        prev.map((m) => (m.kind === "thinking" && m.id === thinkingMessageId ? { ...m, collapsed: true } : m)),
      );
    }

    args.onResult({ result, appendText });
  }, [appendText, args, promptId, thinkingMessageId]);

  return {
    messages,
    setMessages,
    send,
    promptId,
    setPromptId,
    thinkingMessageId,
    setThinkingMessageId,
    appendText,
  };
}


