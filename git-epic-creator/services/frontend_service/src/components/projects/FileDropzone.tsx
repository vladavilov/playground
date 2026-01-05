import { UploadCloud, X } from "lucide-react";
import { useCallback, useMemo, useRef, useState } from "react";
import { AnimatePresence, motion } from "motion/react";

import { cn } from "../../lib/cn";
import { motionDuration } from "../../lib/motion";
import { Button } from "../ui/button";
import { Badge } from "../ui/badge";
import { Collapsible, CollapsibleContent, CollapsibleTrigger } from "../ui/collapsible";

type FileDropzoneProps = {
  accept?: string;
  onFilesChange: (files: File[]) => void;
  value: File[];
  disabled?: boolean;
  multiple?: boolean;
  supportedTypes?: string[];
  supportedTypesVariant?: "collapsible" | "inline" | "none";
  supportedTypesPreviewCount?: number;
};

export function FileDropzone(props: FileDropzoneProps) {
  const inputRef = useRef<HTMLInputElement | null>(null);
  const [isDragging, setIsDragging] = useState(false);
  const [isTypesOpen, setIsTypesOpen] = useState(false);

  const files = props.value;
  const countLabel = files.length === 0 ? "No files selected" : `${files.length} file${files.length === 1 ? "" : "s"}`;
  const supportedTypes = props.supportedTypes ?? ["PDF", "DOC", "DOCX", "MD", "TXT", "XLS", "XLSX", "PNG", "IMG"];
  const supportedTypesVariant = props.supportedTypesVariant ?? "collapsible";
  const previewCount = props.supportedTypesPreviewCount ?? 2;
  const previewTypes = supportedTypes.slice(0, Math.max(0, previewCount));
  const remainingCount = Math.max(0, supportedTypes.length - previewTypes.length);

  const openPicker = useCallback(() => inputRef.current?.click(), []);

  const onInputChange = useCallback(
    (e: React.ChangeEvent<HTMLInputElement>) => {
      const next = Array.from(e.target.files ?? []);
      props.onFilesChange(next);
    },
    [props],
  );

  const onDrop = useCallback(
    (e: React.DragEvent) => {
      e.preventDefault();
      if (props.disabled) return;
      setIsDragging(false);
      const dropped = Array.from(e.dataTransfer.files ?? []);
      if (dropped.length) props.onFilesChange(dropped);
    },
    [props],
  );

  const onDragOver = useCallback((e: React.DragEvent) => {
    e.preventDefault();
    if (props.disabled) return;
    setIsDragging(true);
  }, [props.disabled]);

  const onDragLeave = useCallback(() => setIsDragging(false), []);

  const fileRows = useMemo(
    () =>
      files.map((f) => ({
        key: `${f.name}-${f.size}-${f.lastModified}`,
        label: f.name,
        meta: `${Math.max(1, Math.round(f.size / 1024))} KB`,
      })),
    [files],
  );

  return (
    <div className="space-y-3">
      <div className="flex items-center justify-between">
        <div className="text-xs text-muted-foreground">{countLabel}</div>
        {files.length ? (
          <Button variant="ghost" size="sm" onClick={() => props.onFilesChange([])} disabled={props.disabled}>
            <X className="h-4 w-4" />
            Clear
          </Button>
        ) : null}
      </div>

      <div
        role="button"
        tabIndex={0}
        onClick={openPicker}
        onKeyDown={(e) => (e.key === "Enter" || e.key === " " ? openPicker() : null)}
        onDrop={onDrop}
        onDragOver={onDragOver}
        onDragLeave={onDragLeave}
        className={cn(
          "rounded-lg border-2 border-dashed p-5 text-left transition-colors",
          "cursor-pointer select-none",
          isDragging ? "border-primary/60 bg-primary/5" : "border-border hover:bg-accent/40",
          props.disabled && "pointer-events-none opacity-60",
        )}
        aria-label="Upload files"
      >
        <div className="flex items-start gap-3">
          <div className="mt-0.5 inline-flex h-10 w-10 items-center justify-center rounded-full bg-muted text-muted-foreground">
            <UploadCloud className="h-5 w-5" />
          </div>
          <div className="min-w-0">
            <div className="text-sm font-semibold">Drag & drop files</div>
            <div className="mt-0.5 text-sm text-muted-foreground">
              Or click to browse. {props.multiple === false ? "Single file." : "Multiple files supported."}
            </div>

            {supportedTypesVariant === "none" ? null : supportedTypesVariant === "inline" ? (
              <div className="mt-2 flex flex-wrap gap-1.5">
                {supportedTypes.map((t) => (
                  <Badge key={t} variant="outline">
                    {t}
                  </Badge>
                ))}
              </div>
            ) : (
              <Collapsible open={isTypesOpen} onOpenChange={setIsTypesOpen}>
                <div className="mt-2 flex items-center gap-1.5 overflow-hidden">
                  {previewTypes.map((t) => (
                    <Badge key={t} variant="outline">
                      {t}
                    </Badge>
                  ))}
                  {remainingCount > 0 ? (
                    <CollapsibleTrigger asChild>
                      <button
                        type="button"
                        className="shrink-0 inline-flex items-center rounded-full border border-border bg-background px-2 py-0.5 text-xs font-medium text-muted-foreground hover:text-foreground"
                        onClick={(e) => e.stopPropagation()}
                        onKeyDown={(e) => e.stopPropagation()}
                        aria-label={isTypesOpen ? "Hide supported file types" : "Show supported file types"}
                      >
                        +{remainingCount}
                      </button>
                    </CollapsibleTrigger>
                  ) : null}
                </div>
                <CollapsibleContent asChild forceMount>
                  <AnimatePresence initial={false}>
                    {isTypesOpen ? (
                      <motion.div
                        key="supported-types"
                        initial={{ height: 0, opacity: 0 }}
                        animate={{ height: "auto", opacity: 1 }}
                        exit={{ height: 0, opacity: 0 }}
                        transition={{ duration: motionDuration(0.18), ease: "easeOut" }}
                        className="overflow-hidden"
                        onClick={(e) => e.stopPropagation()}
                        onKeyDown={(e) => e.stopPropagation()}
                      >
                        <div className="mt-2 rounded-md border border-border bg-background/50 p-2">
                          <div className="flex flex-wrap gap-1.5">
                            {supportedTypes.map((t) => (
                              <Badge key={t} variant="outline">
                                {t}
                              </Badge>
                            ))}
                          </div>
                        </div>
                      </motion.div>
                    ) : null}
                  </AnimatePresence>
                </CollapsibleContent>
              </Collapsible>
            )}
          </div>
        </div>

        <input
          ref={inputRef}
          type="file"
          multiple={props.multiple ?? true}
          className="sr-only"
          accept={props.accept}
          onChange={onInputChange}
          disabled={props.disabled}
        />
      </div>

      <AnimatePresence initial={false}>
        {fileRows.length ? (
          <motion.div
            key="selected-files"
            initial={{ height: 0, opacity: 0 }}
            animate={{ height: "auto", opacity: 1 }}
            exit={{ height: 0, opacity: 0 }}
            transition={{ duration: motionDuration(0.18), ease: "easeOut" }}
            className="overflow-hidden"
          >
            <div className="rounded-md border border-border bg-background/50 p-3">
              <div className="space-y-2">
                {fileRows.map((r) => (
                  <div key={r.key} className="flex items-center justify-between gap-3 text-sm">
                    <div className="truncate font-medium">{r.label}</div>
                    <div className="shrink-0 text-xs text-muted-foreground">{r.meta}</div>
                  </div>
                ))}
              </div>
            </div>
          </motion.div>
        ) : null}
      </AnimatePresence>
    </div>
  );
}


