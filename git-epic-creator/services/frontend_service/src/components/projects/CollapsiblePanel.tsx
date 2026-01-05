import type { ReactNode } from "react";
import { useMemo, useState } from "react";
import { AnimatePresence, motion } from "motion/react";
import { ChevronDown, ChevronRight, Trash2 } from "lucide-react";

import { cn } from "../../lib/cn";
import { motionDuration } from "../../lib/motion";
import { Button } from "../ui/button";
import { Collapsible, CollapsibleContent, CollapsibleTrigger } from "../ui/collapsible";

export type LogLine = { at: number; text: string };

export function CollapsiblePanel(props: {
  title: string;
  description?: string;
  defaultOpen?: boolean;
  actions?: ReactNode;
  children: ReactNode;
  logLines?: string[];
  onClearLog?: () => void;
  logDefaultOpen?: boolean;
  showLog?: boolean;
  className?: string;
}) {
  const [open, setOpen] = useState(props.defaultOpen ?? true);
  const [logOpen, setLogOpen] = useState(props.logDefaultOpen ?? true);

  const lines = props.logLines ?? [];
  const hasLines = lines.length > 0;
  const showLog = Boolean(props.showLog) || hasLines;
  const rendered = useMemo(() => lines.slice(-200), [lines]);

  return (
    <Collapsible open={open} onOpenChange={setOpen}>
      <div className={cn("rounded-lg border border-border bg-card shadow-sm", props.className)}>
        <div className="flex items-start justify-between gap-3 p-4">
          <div className="min-w-0">
            <div className="flex items-center gap-2">
              <div className="truncate text-sm font-semibold">{props.title}</div>
            </div>
            {props.description ? (
              <div className="mt-1 text-sm text-muted-foreground">{props.description}</div>
            ) : null}
          </div>

          <div className="flex items-center gap-2">
            {props.actions}
            <CollapsibleTrigger asChild>
              <Button variant="ghost" size="icon" aria-label={open ? "Collapse panel" : "Expand panel"}>
                <ChevronDown className={cn("h-4 w-4 transition-transform", open ? "rotate-180" : "rotate-0")} />
              </Button>
            </CollapsibleTrigger>
          </div>
        </div>

        <CollapsibleContent asChild forceMount>
          <AnimatePresence initial={false}>
            {open ? (
              <motion.div
                initial={{ height: 0, opacity: 0 }}
                animate={{ height: "auto", opacity: 1 }}
                exit={{ height: 0, opacity: 0 }}
                transition={{ duration: motionDuration(0.18), ease: "easeOut" }}
                className="overflow-hidden"
              >
                <div className="border-t border-border p-4 pt-3">
                  {props.children}

                  {/* Visible while an operation is running, even before first event arrives. */}
                  {showLog ? (
                    <Collapsible open={logOpen} onOpenChange={setLogOpen}>
                      <div className="mt-4 rounded-md border border-border bg-muted/15">
                        <div className="flex items-center justify-between gap-2 px-3 py-2">
                          <CollapsibleTrigger asChild>
                            <button
                              type="button"
                              className="inline-flex items-center gap-2 text-xs font-semibold text-muted-foreground hover:text-foreground"
                              aria-label={logOpen ? "Collapse log" : "Expand log"}
                            >
                              {logOpen ? (
                                <ChevronDown className="h-4 w-4" />
                              ) : (
                                <ChevronRight className="h-4 w-4" />
                              )}
                              Log <span className="text-muted-foreground/70">({lines.length})</span>
                            </button>
                          </CollapsibleTrigger>

                          {props.onClearLog ? (
                            <Button
                              variant="ghost"
                              size="sm"
                              onClick={props.onClearLog}
                              title="Clear log"
                            >
                              <Trash2 className="h-4 w-4" />
                              Clear
                            </Button>
                          ) : null}
                        </div>

                        <CollapsibleContent>
                          <div className="max-h-40 overflow-auto border-t border-border bg-muted/10 p-2 font-mono text-[11px] leading-relaxed text-foreground">
                            {rendered.length ? (
                              rendered.map((line, idx) => <div key={idx}>{line}</div>)
                            ) : (
                              <div className="text-muted-foreground">Waiting for progress updates…</div>
                            )}
                          </div>
                        </CollapsibleContent>
                      </div>
                    </Collapsible>
                  ) : null}
                </div>
              </motion.div>
            ) : null}
          </AnimatePresence>
        </CollapsibleContent>
      </div>
    </Collapsible>
  );
}


