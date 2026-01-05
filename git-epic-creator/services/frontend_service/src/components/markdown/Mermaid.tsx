import { useEffect, useId, useMemo, useRef, useState } from "react";
import mermaid from "mermaid";
import { Sparkles } from "lucide-react";

import { useToastStore } from "../../state/toast";
import { Button } from "../ui/button";

let didInit = false;

function normalizeMermaidId(raw: string): string {
  // React's useId() can contain ":" which Mermaid re-writes. We pre-normalize to keep IDs stable.
  return raw.replace(/[^a-zA-Z0-9_-]/g, "_");
}

function formatMermaidError(e: unknown): string {
  if (e instanceof Error) return e.message || "Unknown error";
  if (typeof e === "string") return e;
  try {
    return JSON.stringify(e);
  } catch {
    return String(e);
  }
}

function cleanupBodyMermaidArtifacts() {
  // Defensive cleanup for Mermaid *leaks* only.
  // Important: never remove elements inside the React app tree (Mermaid SVG IDs often start with `mermaid-`).
  for (const el of Array.from(document.body.children)) {
    if (!(el instanceof HTMLElement)) continue;
    if (el.id.startsWith("dmermaid-")) el.remove();
  }

  // In case Mermaid ever appends an SVG directly under <body>, remove it too.
  for (const el of Array.from(document.body.children)) {
    if (!(el instanceof SVGElement)) continue;
    if (el.id.startsWith("mermaid-")) el.remove();
  }
}

export function Mermaid(props: { code: string }) {
  const id = useId();
  const [errorText, setErrorText] = useState<string | null>(null);
  const toast = useToastStore((s) => s.show);
  const hostRef = useRef<HTMLDivElement | null>(null);
  const normalizedCode = useMemo(() => props.code.replace(/\r\n?/g, "\n"), [props.code]);
  const hostId = useMemo(() => `mermaid-host-${normalizeMermaidId(id)}`, [id]);

  useEffect(() => {
    let cancelled = false;

    if (!didInit) {
      mermaid.initialize({ startOnLoad: false, securityLevel: "strict" });
      didInit = true;
    }

    const host = hostRef.current;
    if (!host) return;

    // The legacy UI uses `mermaid.run({ nodes: [node] })` on a `.mermaid` element.
    // This avoids the problematic `render()` path that can append errors to <body>.
    host.textContent = normalizedCode;
    host.removeAttribute("data-processed");

    (async () => {
      try {
        await mermaid.run({ nodes: [host] });
        cleanupBodyMermaidArtifacts();

        const svg = host.querySelector("svg");
        if (!cancelled) {
          setErrorText(svg ? null : "Diagram rendered but SVG not found");
        }
      } catch (e) {
        cleanupBodyMermaidArtifacts();
        const svg = host.querySelector("svg");
        // Mermaid can sometimes throw a non-Error object even though it already produced SVG.
        // If SVG exists, treat as success.
        if (!cancelled && svg) {
          setErrorText(null);
          return;
        }
        if (!cancelled) setErrorText(formatMermaidError(e));
        if (process.env.VITEST !== "true") {
          // eslint-disable-next-line no-console
          console.warn("[Mermaid] run failed", e);
        }
      }
    })();

    return () => {
      cancelled = true;
      cleanupBodyMermaidArtifacts();
    };
  }, [id, normalizedCode]);

  // Mermaid writes SVG into `hostRef`. We keep a fallback source view on errors.
  return (
    <div className="relative overflow-auto rounded-md border border-border bg-card p-3 text-xs">
      <div className="absolute right-2 top-2">
        <Button
          variant="outline"
          size="icon"
          title="Fix with AI"
          aria-label="Fix with AI"
          onClick={() =>
            toast({
              title: "Fix with AI",
              description: "AI Mermaid fix is not implemented yet (legacy parity).",
            })
          }
        >
          <Sparkles className="h-4 w-4" />
        </Button>
      </div>
      {errorText ? (
        <div className="mb-2 rounded-md border border-border bg-muted/30 px-2 py-1 text-[11px] text-muted-foreground">
          Mermaid failed to render: <span className="font-mono">{errorText}</span>
        </div>
      ) : null}
      <div
        ref={hostRef}
        id={hostId}
        className={errorText ? "rounded-md border border-border bg-background/50 p-2 font-mono" : "mermaid"}
      />
      {errorText ? <pre className="mt-2 overflow-auto">{props.code}</pre> : null}
    </div>
  );
}



