import { cn } from "../../lib/cn";
import { Button } from "../ui/button";

export type QuickReply = { id: string; label: string; value: string };

export function QuickReplies(props: {
  title?: string;
  items: QuickReply[];
  onPick: (value: string) => void;
  className?: string;
}) {
  if (!props.items.length) return null;

  return (
    <div className={cn("space-y-2", props.className)}>
      {props.title ? (
        <div className="text-xs font-semibold text-muted-foreground">{props.title}</div>
      ) : null}
      <div className="flex flex-wrap gap-2">
        {props.items.map((it) => (
          <Button
            key={it.id}
            type="button"
            variant="outline"
            size="sm"
            className="h-7 rounded-full px-3 text-xs"
            onClick={() => props.onPick(it.value)}
            title={it.label}
          >
            {it.label}
          </Button>
        ))}
      </div>
    </div>
  );
}



