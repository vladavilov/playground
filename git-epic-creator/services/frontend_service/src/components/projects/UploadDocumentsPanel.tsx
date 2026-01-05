import { useEffect, useRef, useState } from "react";
import { useFetcher } from "react-router";

import { useUiStore } from "../../state/store";
import { useToastStore } from "../../state/toast";
import { FileDropzone } from "./FileDropzone";
import { CollapsiblePanel } from "./CollapsiblePanel";
import { Button } from "../ui/button";
import { isOkResponse } from "./projectUtils";

export function UploadDocumentsPanel(props: {
  projectId: string;
  logLines: string[];
  clearLog: () => void;
}) {
  const fetcher = useFetcher();
  const show = useToastStore((s) => s.show);
  const [files, setFiles] = useState<File[]>([]);
  const appendProgressLog = useUiStore((s) => s.appendProgressLog);

  const toastArmedRef = useRef(false);
  useEffect(() => {
    if (fetcher.state !== "idle") toastArmedRef.current = true;
    if (fetcher.state !== "idle") return;
    if (!toastArmedRef.current) return;
    if (!isOkResponse(fetcher.data)) return;
    toastArmedRef.current = false;
    show({ title: "Upload started", description: "Documents are being processed." });
  }, [fetcher.data, fetcher.state, show]);

  const canUpload = files.length > 0 && fetcher.state === "idle";

  return (
    <CollapsiblePanel
      title="Upload documents"
      description="Drag & drop files to kick off RAG ingestion."
      defaultOpen={false}
      logLines={props.logLines}
      showLog={fetcher.state !== "idle"}
      onClearLog={props.clearLog}
      logDefaultOpen
    >
      <FileDropzone value={files} onFilesChange={setFiles} disabled={fetcher.state !== "idle"} />
      <div className="mt-3 flex items-center gap-2">
        <Button
          size="sm"
          disabled={!canUpload}
          onClick={() => {
            appendProgressLog("upload_documents", "Upload requested. Waiting for progress updates…");
            const fd = new FormData();
            fd.set("intent", "upload");
            fd.set("projectId", props.projectId);
            for (const f of files) fd.append("files", f);
            fetcher.submit(fd, { method: "post", encType: "multipart/form-data" });
            setFiles([]);
          }}
        >
          Upload
        </Button>
        <div className="text-xs text-muted-foreground">{fetcher.state === "idle" ? "Ready" : "Uploading…"}</div>
      </div>
    </CollapsiblePanel>
  );
}


