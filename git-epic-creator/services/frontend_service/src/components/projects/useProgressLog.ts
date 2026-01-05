import { useCallback, useState } from "react";

import type { LogLine } from "./CollapsiblePanel";

export function useProgressLog(limit = 200) {
  const [log, setLog] = useState<LogLine[]>([]);

  const append = useCallback(
    (text: string) => {
      setLog((prev) => {
        const next = [...prev, { at: Date.now(), text }];
        return next.length > limit ? next.slice(next.length - limit) : next;
      });
    },
    [limit],
  );

  const clear = useCallback(() => setLog([]), []);

  return { log, append, clear };
}



