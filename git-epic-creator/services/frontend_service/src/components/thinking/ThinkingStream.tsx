import { useMemo, useState } from "react";
import { AnimatePresence, motion } from "motion/react";

import { useUiStore } from "../../state/store";
import { motionDuration } from "../../lib/motion";
import { Button } from "../ui/button";
import { Collapsible, CollapsibleContent, CollapsibleTrigger } from "../ui/collapsible";
import { Markdown } from "../markdown/Markdown";

type Stream = { promptId: string; items: string[] };

export function ThinkingStream(props: { className?: string; showTitle?: boolean }) {
  const thinkingByPromptId = useUiStore((s) => s.thinkingByPromptId);
  const showTitle = props.showTitle ?? true;

  const streams = useMemo<Stream[]>(
    () =>
      Object.entries(thinkingByPromptId)
        .map(([promptId, items]) => ({ promptId, items }))
        .sort((a, b) => a.promptId.localeCompare(b.promptId)),
    [thinkingByPromptId],
  );

  if (streams.length === 0) return null;

  return (
    <section className={props.className}>
      {showTitle ? <h2 className="text-sm font-semibold">Thinking stream</h2> : null}
      <div className={showTitle ? "mt-2 space-y-2" : "space-y-2"}>
        {streams.map((s) => (
          <ThinkingPrompt key={s.promptId} promptId={s.promptId} items={s.items} />
        ))}
      </div>
    </section>
  );
}

function ThinkingPrompt(props: { promptId: string; items: string[] }) {
  const [open, setOpen] = useState(true);

  const markdown = useMemo(() => props.items.join("\n\n"), [props.items]);

  return (
    <Collapsible open={open} onOpenChange={setOpen}>
      <div className="rounded-md border border-border bg-card">
        <div className="flex items-center justify-between gap-2 px-3 py-2">
          <div className="text-xs font-medium">prompt_id: {props.promptId}</div>
          <CollapsibleTrigger asChild>
            <Button variant="ghost" size="sm">
              {open ? "Collapse" : "Expand"}
            </Button>
          </CollapsibleTrigger>
        </div>

        <CollapsibleContent asChild forceMount>
          <AnimatePresence initial={false}>
            {open ? (
              <motion.div
                key="content"
                initial={{ opacity: 0, height: 0 }}
                animate={{ opacity: 1, height: "auto" }}
                exit={{ opacity: 0, height: 0 }}
                transition={{ duration: motionDuration(0.2), ease: "easeOut" }}
                className="overflow-hidden"
              >
                <div className="border-t border-border px-3 py-3">
                  <div className="prose prose-sm max-w-none dark:prose-invert">
                    <Markdown markdown={markdown} />
                  </div>
                </div>
              </motion.div>
            ) : null}
          </AnimatePresence>
        </CollapsibleContent>
      </div>
    </Collapsible>
  );
}


