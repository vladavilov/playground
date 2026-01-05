import { AnimatePresence, motion } from "motion/react";
import { ChevronDown } from "lucide-react";
import { useEffect, useRef } from "react";

import { cn } from "../../lib/cn";
import { motionDuration } from "../../lib/motion";
import { Markdown } from "../markdown/Markdown";
import type { ChatMessage } from "./chat.types";

export function ChatThread(props: {
  messages: ChatMessage[];
  onToggleThinking?: (id: string) => void;
  className?: string;
}) {
  const endRef = useRef<HTMLDivElement | null>(null);
  const prose =
    "prose prose-sm max-w-none dark:prose-invert prose-p:text-foreground prose-li:text-foreground prose-strong:text-foreground prose-headings:text-foreground prose-code:text-foreground prose-pre:text-foreground prose-a:text-foreground";

  useEffect(() => {
    const el = endRef.current;
    const scrollIntoView = el?.scrollIntoView;
    if (typeof scrollIntoView !== "function") return;
    scrollIntoView.call(el, { block: "end" });
  }, [props.messages.length]);

  return (
    <div
      className={cn("min-h-0 space-y-3 overflow-auto pr-1", props.className)}
      data-testid="chat-thread"
    >
      <AnimatePresence initial={false}>
        {props.messages.map((m) => (
          <motion.div
            key={m.id}
            initial={{ opacity: 0, y: 6 }}
            animate={{ opacity: 1, y: 0 }}
            exit={{ opacity: 0, y: -6 }}
            transition={{ duration: motionDuration(0.16) }}
          >
            {m.kind === "thinking" ? (
              <div className="flex justify-start">
                <div className="w-full max-w-[min(42rem,100%)] rounded-2xl border border-border bg-muted/20">
                  <button
                    type="button"
                    className="flex w-full items-center justify-between gap-2 px-3 py-2 text-xs font-semibold text-muted-foreground"
                    onClick={() => props.onToggleThinking?.(m.id)}
                    aria-label={m.collapsed ? "Expand thought process" : "Collapse thought process"}
                  >
                    <span>{m.title ?? "Thought process"}</span>
                    <ChevronDown className={cn("h-4 w-4 transition-transform", m.collapsed ? "rotate-0" : "rotate-180")} />
                  </button>
                  <AnimatePresence initial={false}>
                    {m.collapsed ? null : (
                      <motion.div
                        key="thinking-body"
                        initial={{ height: 0, opacity: 0 }}
                        animate={{ height: "auto", opacity: 1 }}
                        exit={{ height: 0, opacity: 0 }}
                        transition={{ duration: motionDuration(0.18), ease: "easeOut" }}
                        className="overflow-hidden"
                      >
                        <div className="border-t border-border px-3 py-2">
                          <div className="prose prose-sm max-w-none dark:prose-invert">
                            <Markdown markdown={m.markdown || "_…_"} />
                          </div>
                        </div>
                      </motion.div>
                    )}
                  </AnimatePresence>
                </div>
              </div>
            ) : m.role === "system" ? (
              <div className="flex justify-center">
                <div className="w-full max-w-[min(42rem,100%)] rounded-2xl border border-border bg-muted/30 px-4 py-3">
                  <div className={cn(prose, "text-xs leading-relaxed")}>
                    <Markdown markdown={m.content} />
                  </div>
                </div>
              </div>
            ) : m.role === "user" ? (
              <div className="flex justify-end">
                <div className="max-w-[min(42rem,100%)] rounded-2xl border border-border bg-background px-4 py-3 shadow-sm">
                  <div className={cn(prose, "text-sm leading-relaxed")}>
                    <Markdown markdown={m.content} />
                  </div>
                </div>
              </div>
            ) : (
              <div className="flex justify-start">
                <div className="max-w-[min(42rem,100%)] rounded-2xl border border-primary/20 bg-[linear-gradient(180deg,color-mix(in_oklab,var(--primary)_8%,transparent),transparent)] px-4 py-3 shadow-sm">
                  <div className={cn(prose, "text-sm leading-relaxed")}>
                    <Markdown markdown={m.content} />
                  </div>
                </div>
              </div>
            )}
          </motion.div>
        ))}
      </AnimatePresence>
      <div ref={endRef} />
    </div>
  );
}



