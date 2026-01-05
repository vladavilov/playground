import { useEffect, useRef, useState } from "react";
import { useFetcher } from "react-router";
import { RefreshCw, Upload } from "lucide-react";

import { useUiStore } from "../../state/store";
import { useToastStore } from "../../state/toast";
import { CollapsiblePanel } from "./CollapsiblePanel";
import { FileDropzone } from "./FileDropzone";
import { Button } from "../ui/button";
import { Select, SelectContent, SelectItem, SelectTrigger, SelectValue } from "../ui/select";
import { extractGitlabPath, isOkResponse, isSourceLanguage, type SourceLanguage } from "./projectUtils";

export function GitlabRepoPanel(props: {
  projectId: string;
  repoUrls: string[];
  logLines: string[];
  clearLog: () => void;
}) {
  const repoUrls = props.repoUrls ?? [];
  const [activeRepoUrl, setActiveRepoUrl] = useState<string>(repoUrls[0] ?? "");
  useEffect(() => {
    setActiveRepoUrl(repoUrls[0] ?? "");
  }, [repoUrls.join("\n")]);

  const indexFetcher = useFetcher();
  const zipFetcher = useFetcher();
  const show = useToastStore((s) => s.show);
  const appendProgressLog = useUiStore((s) => s.appendProgressLog);

  const [sourceLanguage, setSourceLanguage] = useState<SourceLanguage>("javascript");
  const [zipFiles, setZipFiles] = useState<File[]>([]);

  const indexToastArmedRef = useRef(false);
  useEffect(() => {
    if (indexFetcher.state !== "idle") indexToastArmedRef.current = true;
    if (indexFetcher.state !== "idle") return;
    if (!indexToastArmedRef.current) return;
    if (!isOkResponse(indexFetcher.data)) return;
    indexToastArmedRef.current = false;
    show({ title: "Index started", description: "Code graph ingestion kicked off." });
  }, [indexFetcher.data, indexFetcher.state, show]);

  const zipToastArmedRef = useRef(false);
  useEffect(() => {
    if (zipFetcher.state !== "idle") zipToastArmedRef.current = true;
    if (zipFetcher.state !== "idle") return;
    if (!zipToastArmedRef.current) return;
    if (!isOkResponse(zipFetcher.data)) return;
    zipToastArmedRef.current = false;
    show({ title: "Upload started", description: "ZIP is being ingested." });
  }, [zipFetcher.data, zipFetcher.state, show]);

  const canUploadZip = zipFiles.length > 0 && zipFetcher.state === "idle";

  return (
    <CollapsiblePanel
      title="Repository"
      description="Source repositories used for context."
      defaultOpen={false}
      logLines={props.logLines}
      showLog={indexFetcher.state !== "idle" || zipFetcher.state !== "idle"}
      onClearLog={props.clearLog}
      logDefaultOpen
      actions={null}
    >
      <div className="space-y-3">
        <div className="flex flex-wrap items-center gap-2">
          {repoUrls.length ? (
            <div className="flex flex-wrap gap-2">
              {repoUrls.map((u) => {
                const display = extractGitlabPath(u) ?? u;
                const isHttp = u.startsWith("http://") || u.startsWith("https://");
                const selected = u === activeRepoUrl;
                return (
                  <button
                    key={u}
                    type="button"
                    onClick={() => setActiveRepoUrl(u)}
                    className={[
                      "inline-flex items-center gap-2 rounded-full border px-3 py-1 text-xs font-medium transition-colors",
                      selected ? "border-primary/40 bg-primary/10" : "border-border bg-background hover:bg-accent",
                    ].join(" ")}
                    title={u}
                  >
                    {isHttp ? (
                      <a
                        href={u}
                        target="_blank"
                        rel="noreferrer"
                        className="max-w-[18rem] truncate hover:underline"
                        onClick={(e) => e.stopPropagation()}
                      >
                        {display}
                      </a>
                    ) : (
                      <span className="max-w-[18rem] truncate">{display}</span>
                    )}
                    {selected ? <span className="text-muted-foreground">(active)</span> : null}
                  </button>
                );
              })}
            </div>
          ) : (
            <div className="text-sm text-muted-foreground">No repository URLs set.</div>
          )}

          <button
            type="button"
            disabled={indexFetcher.state !== "idle" || !activeRepoUrl}
            title="Index repository into the code graph (ingest from Git URL)."
            onClick={() => {
              appendProgressLog("repo_index", "Index requested. Waiting for progress updates…");
              indexFetcher.submit(
                {
                  intent: "indexRepo",
                  projectId: props.projectId,
                  repoUrl: activeRepoUrl,
                  source_language: sourceLanguage,
                },
                { method: "post" },
              );
            }}
            className="inline-flex items-center gap-2 rounded-full border border-border bg-background px-3 py-1 text-xs font-medium transition-colors hover:bg-accent disabled:opacity-50"
          >
            <RefreshCw className="h-3.5 w-3.5 text-muted-foreground" />
            Index
          </button>
        </div>

        <div className="rounded-md border border-border bg-background/50 p-3">
          <div className="text-sm font-semibold">Upload source snapshot</div>
          <div className="mt-0.5 text-sm text-muted-foreground">ZIP only. Used when Git access isn’t available.</div>

          <div className="mt-3">
            <FileDropzone
              value={zipFiles}
              onFilesChange={setZipFiles}
              disabled={zipFetcher.state !== "idle"}
              multiple={false}
              accept=".zip,application/zip,application/x-zip-compressed"
              supportedTypes={["ZIP"]}
              supportedTypesVariant="inline"
            />
            <div className="mt-3 flex items-center gap-2">
              <Button
                size="sm"
                disabled={!canUploadZip}
                onClick={() => {
                  const f = zipFiles[0];
                  if (!f) return;
                  appendProgressLog("repo_index", "ZIP upload requested. Waiting for progress updates…");
                  const fd = new FormData();
                  fd.set("intent", "uploadRepoZip");
                  fd.set("projectId", props.projectId);
                  fd.set("source_language", sourceLanguage);
                  fd.set("file", f);
                  zipFetcher.submit(fd, { method: "post", encType: "multipart/form-data" });
                  setZipFiles([]);
                }}
              >
                <Upload className="h-4 w-4" />
                Upload ZIP
              </Button>
              <Select
                value={sourceLanguage}
                onValueChange={(v) => {
                  if (isSourceLanguage(v)) setSourceLanguage(v);
                }}
              >
                <SelectTrigger className="h-8 w-[10.5rem]">
                  <SelectValue placeholder="Language" />
                </SelectTrigger>
                <SelectContent>
                  <SelectItem value="javascript">JavaScript</SelectItem>
                  <SelectItem value="java">Java</SelectItem>
                  <SelectItem value="cobol">COBOL</SelectItem>
                </SelectContent>
              </Select>
              <div className="text-xs text-muted-foreground">{zipFetcher.state === "idle" ? "Ready" : "Uploading…"}</div>
            </div>
          </div>
        </div>
      </div>
    </CollapsiblePanel>
  );
}


