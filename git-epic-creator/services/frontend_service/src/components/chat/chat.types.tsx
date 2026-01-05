export type ChatRole = "user" | "assistant" | "system";

export type ChatTextMessage = {
  kind: "text";
  id: string;
  role: ChatRole;
  // For assistant/system, this may be markdown. For user, plain text.
  content: string;
  meta?: string;
  createdAt: number;
};

export type ChatThinkingMessage = {
  kind: "thinking";
  id: string;
  title?: string;
  markdown: string;
  collapsed: boolean;
  createdAt: number;
};

export type ChatMessage = ChatTextMessage | ChatThinkingMessage;



