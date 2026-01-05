import { useEffect, useRef } from "react";
import { useFetcher } from "react-router";
import { Tag, Zap } from "lucide-react";

import { useUiStore } from "../../state/store";
import { useToastStore } from "../../state/toast";
import { CollapsiblePanel } from "./CollapsiblePanel";
import { extractGitlabPath, isOkResponse } from "./projectUtils";

export function GitlabBacklogPanel(props: {
  project: { id: string; gitlab_backlog_project_ids?: string[]; gitlab_backlog_project_urls?: string[] };
  logLines: string[];
  clearLog: () => void;
}) {
  const fetcher = useFetcher();
  const show = useToastStore((s) => s.show);
  const ids = props.project.gitlab_backlog_project_ids ?? [];
  const urls = props.project.gitlab_backlog_project_urls ?? [];
  const appendProgressLog = useUiStore((s) => s.appendProgressLog);

  const toastArmedRef = useRef(false);
  useEffect(() => {
    if (fetcher.state !== "idle") toastArmedRef.current = true;
    if (fetcher.state !== "idle") return;
    if (!toastArmedRef.current) return;
    if (!isOkResponse(fetcher.data)) return;
    toastArmedRef.current = false;
    show({ title: "Cache embeddings started" });
  }, [fetcher.data, fetcher.state, show]);

  return (
    <CollapsiblePanel
      title="Backlog projects"
      description="Linked GitLab issues/epics projects."
      defaultOpen={false}
      logLines={props.logLines}
      showLog={fetcher.state !== "idle"}
      onClearLog={props.clearLog}
      logDefaultOpen
    >
      <div className="flex flex-wrap items-center gap-2">
        <button
          type="button"
          disabled={ids.length === 0 || fetcher.state !== "idle"}
          title="Cache embeddings for all linked backlog projects (improves AI retrieval)."
          onClick={() => {
            appendProgressLog("cache_embeddings", "Caching embeddings requested. Waiting for progress updates…");
            fetcher.submit(
              {
                intent: "cacheEmbeddings",
                projectId: props.project.id,
                gitlab_project_ids: ids.join(","),
              },
              { method: "post" },
            );
          }}
          className="inline-flex items-center gap-2 rounded-full border border-border bg-background px-3 py-1 text-xs font-medium transition-colors hover:bg-accent disabled:opacity-50"
        >
          <Zap className="h-3.5 w-3.5 text-muted-foreground" />
          Cache embeddings
        </button>
      </div>

      {ids.length === 0 ? (
        <div className="mt-3 text-sm text-muted-foreground">No backlog projects linked.</div>
      ) : (
        <div className="mt-3 flex flex-wrap gap-2">
          {ids.map((id, idx) => {
            const url = urls[idx] ?? null;
            const label = extractGitlabPath(url) ?? id;
            return (
              <a
                key={`${id}-${idx}`}
                href={url ?? undefined}
                target={url ? "_blank" : undefined}
                rel={url ? "noreferrer" : undefined}
                className="inline-flex items-center gap-2 rounded-full border border-border bg-background px-3 py-1 text-xs font-medium hover:bg-accent"
                title={url ? url : `GitLab project id: ${id}`}
              >
                <Tag className="h-3.5 w-3.5 text-muted-foreground" />
                <span className="max-w-[14rem] truncate">{label}</span>
                <span className="text-muted-foreground">#{id}</span>
              </a>
            );
          })}
        </div>
      )}
    </CollapsiblePanel>
  );
}


