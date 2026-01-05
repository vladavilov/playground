import type { ReactNode } from "react";
import { useMemo, useState } from "react";
import { AnimatePresence, motion } from "motion/react";
import { ChevronLeft, ChevronRight, MessagesSquare } from "lucide-react";

import { motionDuration } from "../../lib/motion";
import { Button } from "../ui/button";
import { ChatComposer } from "./ChatComposer";
import { ChatThread } from "./ChatThread";
import type { ChatMessage, ChatTextMessage } from "./chat.types";

function shortenPrompt(text: string, maxLen: number) {
  const flat = text.replace(/\s+/g, " ").trim();
  if (flat.length <= maxLen) return flat;
  return `${flat.slice(0, Math.max(0, maxLen - 1))}…`;
}

function getFirstUserPrompt(messages: ChatMessage[]): string | null {
  for (const m of messages) {
    if (m.kind !== "text") continue;
    const t = m as ChatTextMessage;
    if (t.role !== "user") continue;
    const content = t.content.trim();
    return content ? content : null;
  }
  return null;
}

export function ChatShell(props: {
  messages: ChatMessage[];
  onToggleThinking?: (id: string) => void;
  input: string;
  setInput: (v: string) => void;
  onSend: () => void;
  disabled?: boolean;
  quickReplies?: ReactNode;
  rightPanel: ReactNode;
}) {
  const [collapsed, setCollapsed] = useState(false);
  const firstPrompt = useMemo(() => getFirstUserPrompt(props.messages), [props.messages]);
  const collapsedLabel = firstPrompt ? shortenPrompt(firstPrompt, 90) : "Chat";

  return (
    <div className="grid min-h-0 flex-1 gap-4 overflow-hidden lg:grid-cols-[auto_minmax(0,1fr)]">
      <AnimatePresence initial={false}>
        {collapsed ? (
          <motion.section
            key="chat-collapsed"
            initial={{ opacity: 0, x: -8 }}
            animate={{ opacity: 1, x: 0 }}
            exit={{ opacity: 0, x: -8 }}
            transition={{ duration: motionDuration(0.18), ease: "easeOut" }}
            className="min-w-0"
          >
            <div className="flex h-12 items-center justify-between rounded-2xl border border-border bg-muted/30 px-2 text-xs text-muted-foreground lg:h-full lg:w-14 lg:flex-col lg:justify-start lg:py-3">
              <Button
                variant="ghost"
                size="icon"
                className="h-9 w-9"
                onClick={() => setCollapsed(false)}
                title="Restore chat"
                aria-label="Restore chat"
              >
                <ChevronRight className="h-4 w-4" />
              </Button>
              <div className="flex min-w-0 items-center gap-2 px-2 lg:mt-2 lg:flex-1 lg:px-0">
                <MessagesSquare className="h-4 w-4 shrink-0 lg:hidden" />
                <span className="min-w-0 truncate lg:hidden">{collapsedLabel}</span>
                <span className="writing-vertical-rl hidden max-h-full overflow-hidden px-1 text-[11px] leading-tight lg:block">
                  {collapsedLabel}
                </span>
              </div>
            </div>
          </motion.section>
        ) : (
          <motion.section
            key="chat-expanded"
            initial={{ opacity: 0, x: -8 }}
            animate={{ opacity: 1, x: 0 }}
            exit={{ opacity: 0, x: -8 }}
            transition={{ duration: motionDuration(0.18), ease: "easeOut" }}
            className="flex min-h-0 min-w-0 flex-col lg:w-[32rem] lg:max-w-[32rem]"
          >
            {/* chat area */}
            <div className="flex min-h-0 flex-1 flex-col rounded-2xl border border-border bg-card">
              <div className="flex items-center justify-between border-b border-border px-3 py-2 sm:px-4">
                <div className="text-xs font-medium text-muted-foreground">Chat</div>
                <Button
                  variant="ghost"
                  size="icon"
                  className="h-9 w-9"
                  onClick={() => setCollapsed(true)}
                  title="Collapse chat"
                  aria-label="Collapse chat"
                >
                  <ChevronLeft className="h-4 w-4" />
                </Button>
              </div>
              <div className="flex min-h-0 flex-1 flex-col p-3 sm:p-4">
                <ChatThread
                  messages={props.messages}
                  onToggleThinking={props.onToggleThinking}
                  className="flex-1"
                />
              </div>
              {props.quickReplies ? (
                <div className="shrink-0 border-t border-border px-3 py-3 sm:px-4">{props.quickReplies}</div>
              ) : null}
              <div className="shrink-0 border-t border-border px-3 py-3 sm:px-4">
                <ChatComposer
                  value={props.input}
                  onChange={props.setInput}
                  onSend={props.onSend}
                  disabled={props.disabled}
                  placeholder="Write a message…"
                />
              </div>
            </div>
          </motion.section>
        )}
      </AnimatePresence>

      {/* right panel */}
      <aside className="flex min-h-0 min-w-0 flex-col overflow-hidden lg:border-l lg:border-border lg:pl-4">
        {props.rightPanel}
      </aside>
    </div>
  );
}



