import { SendHorizonal } from "lucide-react";
import { useEffect, useRef } from "react";

import { cn } from "../../lib/cn";
import { Button } from "../ui/button";

export function ChatComposer(props: {
  value: string;
  onChange: (v: string) => void;
  onSend: () => void;
  disabled?: boolean;
  placeholder?: string;
  className?: string;
}) {
  const ref = useRef<HTMLTextAreaElement | null>(null);

  useEffect(() => {
    // Keep the caret visible on input growth
    ref.current?.setSelectionRange(ref.current.value.length, ref.current.value.length);
  }, [props.value]);

  const canSend = !props.disabled && props.value.trim().length > 0;

  return (
    <div className={cn("rounded-2xl border border-border bg-background/80 backdrop-blur", props.className)}>
      <div className="flex items-end gap-2 p-3">
        <textarea
          ref={ref}
          className="min-h-[44px] max-h-40 w-full resize-none bg-transparent px-2 py-2 text-sm outline-none placeholder:text-muted-foreground"
          value={props.value}
          onChange={(e) => props.onChange(e.target.value)}
          placeholder={props.placeholder ?? "Write a message…"}
          disabled={props.disabled}
          onKeyDown={(e) => {
            if (e.key === "Enter" && !e.shiftKey) {
              e.preventDefault();
              if (canSend) props.onSend();
            }
          }}
        />
        <Button size="icon" disabled={!canSend} onClick={props.onSend} aria-label="Send">
          <SendHorizonal className="h-4 w-4" />
        </Button>
      </div>
      <div className="px-4 pb-3 text-[11px] text-muted-foreground">
        Enter to send, Shift+Enter for a new line
      </div>
    </div>
  );
}


