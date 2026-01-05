import { AnimatePresence, motion } from "motion/react";
import { useEffect } from "react";
import { ChevronDown, Sparkles } from "lucide-react";

import { cn } from "../../lib/cn";
import { motionDuration } from "../../lib/motion";
import { Button } from "../ui/button";
import { Collapsible, CollapsibleContent, CollapsibleTrigger } from "../ui/collapsible";
import { Markdown } from "../markdown/Markdown";

export function ThinkingBox(props: {
  open: boolean;
  onOpenChange: (v: boolean) => void;
  markdown: string;
  isThinking: boolean;
  forceVisible?: boolean;
  className?: string;
}) {
  // Auto-open while thinking; collapse when response arrives (caller controls `open`).
  useEffect(() => {
    if (props.isThinking) props.onOpenChange(true);
  }, [props.isThinking, props.onOpenChange]);

  const hasText = props.markdown.trim().length > 0;
  if (!hasText && !props.isThinking && !props.forceVisible) return null;

  return (
    <Collapsible open={props.open} onOpenChange={props.onOpenChange}>
      <div className={cn("rounded-2xl border border-border bg-muted/20", props.className)}>
        <div className="flex items-center justify-between gap-2 px-4 py-2.5">
          <div className="flex items-center gap-2 text-xs font-semibold text-muted-foreground">
            <Sparkles className={cn("h-4 w-4", props.isThinking ? "animate-pulse" : "")} />
            {props.isThinking ? "Thinking…" : "Thought process"}
          </div>
          <CollapsibleTrigger asChild>
            <Button variant="ghost" size="icon" aria-label={props.open ? "Collapse thinking" : "Expand thinking"}>
              <ChevronDown className={cn("h-4 w-4 transition-transform", props.open ? "rotate-180" : "rotate-0")} />
            </Button>
          </CollapsibleTrigger>
        </div>

        <CollapsibleContent asChild forceMount>
          <AnimatePresence initial={false}>
            {props.open ? (
              <motion.div
                key="content"
                initial={{ opacity: 0, height: 0 }}
                animate={{ opacity: 1, height: "auto" }}
                exit={{ opacity: 0, height: 0 }}
                transition={{ duration: motionDuration(0.18), ease: "easeOut" }}
                className="overflow-hidden"
              >
                <div className="border-t border-border px-4 py-3">
                  {hasText ? (
                    <div className="prose prose-sm max-w-none dark:prose-invert">
                      <Markdown markdown={props.markdown} />
                    </div>
                  ) : props.isThinking ? (
                    <div className="space-y-2">
                      <div className="h-3 w-56 rounded bg-muted" />
                      <div className="h-3 w-72 rounded bg-muted" />
                      <div className="h-3 w-64 rounded bg-muted" />
                    </div>
                  ) : (
                    <div className="text-xs text-muted-foreground">Completed.</div>
                  )}
                </div>
              </motion.div>
            ) : null}
          </AnimatePresence>
        </CollapsibleContent>
      </div>
    </Collapsible>
  );
}


