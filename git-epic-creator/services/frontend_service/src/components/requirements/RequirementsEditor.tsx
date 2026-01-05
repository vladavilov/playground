import { useEffect, useMemo, useState } from "react";
import { Expand, Sparkles, Trash2, X } from "lucide-react";

import type { RequirementsBundle, Requirement } from "../../routes/requirements.types";
import { Button } from "../ui/button";
import { Dialog, DialogContent, DialogDescription, DialogHeader, DialogTitle, DialogTrigger } from "../ui/dialog";
import { Select, SelectContent, SelectItem, SelectTrigger, SelectValue } from "../ui/select";
import {
  deleteRequirementFromBundle,
  joinAcceptanceCriteria,
  splitAcceptanceCriteria,
  type RequirementType,
  updateRequirement,
} from "./requirements.edit";
import { cn } from "../../lib/cn";
import { Markdown } from "../markdown/Markdown";

export function RequirementsEditor(props: {
  bundle: RequirementsBundle;
  onChange: (next: RequirementsBundle) => void;
  onEnhance?: (args: { requirementId: string; requirementType: RequirementType; currentContent: unknown }) => void;
  enhancingRequirementId?: string | null;
  className?: string;
}) {
  return (
    <div
      className={cn("flex min-h-0 flex-1 flex-col gap-2 overflow-auto pr-1", props.className)}
      data-testid="requirements-editor"
    >
      {[
        ...props.bundle.business_requirements.map((r) => ({ r, type: "business" as const })),
        ...props.bundle.functional_requirements.map((r) => ({ r, type: "functional" as const })),
      ].map(({ r, type }) => (
        <RequirementEditorCard
          key={r.id}
          type={type}
          item={r}
          onEnhance={props.onEnhance}
          enhancing={props.enhancingRequirementId === r.id}
          onDelete={() => props.onChange(deleteRequirementFromBundle(props.bundle, type, r.id))}
          onChange={(next) => {
            if (type === "business") {
              props.onChange({
                ...props.bundle,
                business_requirements: props.bundle.business_requirements.map((x) =>
                  x.id === r.id ? next : x,
                ),
              });
            } else {
              props.onChange({
                ...props.bundle,
                functional_requirements: props.bundle.functional_requirements.map((x) =>
                  x.id === r.id ? next : x,
                ),
              });
            }
          }}
        />
      ))}
    </div>
  );
}

function RequirementEditorCard(props: {
  type: RequirementType;
  item: Requirement;
  onEnhance?: (args: { requirementId: string; requirementType: RequirementType; currentContent: unknown }) => void;
  enhancing: boolean;
  onDelete: () => void;
  onChange: (next: Requirement) => void;
}) {
  const canEnhance = Boolean(props.onEnhance);
  const [editing, setEditing] = useState<null | "title" | "description" | "acceptance">(null);
  const [acText, setAcText] = useState(() => joinAcceptanceCriteria(props.item.acceptance_criteria));
  const acList = useMemo(() => splitAcceptanceCriteria(acText), [acText]);

  const commit = (patch: Partial<Pick<Requirement, "title" | "description" | "priority">>) =>
    props.onChange(updateRequirement(props.item, { ...patch, acceptance_criteria: acList }));

  const acceptanceMarkdown = useMemo(() => {
    if (!props.item.acceptance_criteria?.length) return "_No acceptance criteria yet._";
    return props.item.acceptance_criteria.map((l) => `- ${l}`).join("\n");
  }, [props.item.acceptance_criteria]);

  return (
    <div className="rounded-md border border-border p-3">
      <div className="flex flex-wrap items-start justify-between gap-3">
        <div className="min-w-0 flex-1">
          {/* Title */}
          <div>
            {editing === "title" ? (
              <input
                className="w-full rounded-md border border-border bg-background px-3 py-2 text-sm"
                value={props.item.title}
                autoFocus
                onChange={(e) => commit({ title: e.target.value })}
                onBlur={() => setEditing(null)}
              />
            ) : (
              <div
                role="button"
                tabIndex={0}
                className={cn(
                  "rounded-md px-2 py-1 text-sm font-semibold outline-none",
                  "cursor-text hover:bg-muted/40 hover:ring-2 hover:ring-primary/15",
                  "focus-visible:ring-2 focus-visible:ring-ring",
                )}
                title="Click to edit title"
                onClick={() => setEditing("title")}
                onKeyDown={(e) => {
                  if (e.key === "Enter") setEditing("title");
                }}
              >
                {props.item.title}
              </div>
            )}
          </div>

          <div className="mt-2 w-[10rem]">
            <Select value={props.item.priority || "Must"} onValueChange={(v) => commit({ priority: v })}>
              <SelectTrigger className="h-7 w-full rounded-full px-3 text-xs">
                <SelectValue placeholder="Priority" />
              </SelectTrigger>
              <SelectContent>
                {["Must", "Should", "Could", "Won’t"].map((p) => (
                  <SelectItem key={p} value={p}>
                    {p}
                  </SelectItem>
                ))}
              </SelectContent>
            </Select>
          </div>

          {/* Description */}
          <div className="mt-3">
            <div className="text-[11px] font-medium text-muted-foreground">Description</div>
            {editing === "description" ? (
              <textarea
                className="mt-1 w-full rounded-md border border-border bg-background px-3 py-2 text-sm"
                rows={6}
                value={props.item.description}
                autoFocus
                onChange={(e) => commit({ description: e.target.value })}
                onBlur={() => setEditing(null)}
              />
            ) : (
              <div
                role="button"
                tabIndex={0}
                className={cn(
                  "mt-1 rounded-md border border-transparent px-2 py-2 text-sm outline-none",
                  "cursor-text hover:bg-muted/30 hover:ring-2 hover:ring-primary/15",
                  "focus-visible:ring-2 focus-visible:ring-ring",
                )}
                title="Click to edit description"
                onClick={() => setEditing("description")}
                onKeyDown={(e) => {
                  if (e.key === "Enter") setEditing("description");
                }}
              >
                <div className="prose prose-sm max-w-none dark:prose-invert">
                  <Markdown markdown={props.item.description || "_No description yet._"} />
                </div>
              </div>
            )}
          </div>

          {/* Acceptance criteria */}
          <div className="mt-3">
            <div className="text-[11px] font-medium text-muted-foreground">Acceptance criteria</div>
            {editing === "acceptance" ? (
              <textarea
                className="mt-1 w-full rounded-md border border-border bg-background px-3 py-2 text-sm"
                rows={6}
                value={acText}
                autoFocus
                onChange={(e) => {
                  setAcText(e.target.value);
                  props.onChange(
                    updateRequirement(props.item, {
                      acceptance_criteria: splitAcceptanceCriteria(e.target.value),
                    }),
                  );
                }}
                onBlur={() => setEditing(null)}
              />
            ) : (
              <div
                role="button"
                tabIndex={0}
                className={cn(
                  "mt-1 rounded-md border border-transparent px-2 py-2 text-sm outline-none",
                  "cursor-text hover:bg-muted/30 hover:ring-2 hover:ring-primary/15",
                  "focus-visible:ring-2 focus-visible:ring-ring",
                )}
                title="Click to edit acceptance criteria"
                onClick={() => setEditing("acceptance")}
                onKeyDown={(e) => {
                  if (e.key === "Enter") setEditing("acceptance");
                }}
              >
                <div className="prose prose-sm max-w-none dark:prose-invert">
                  <Markdown markdown={acceptanceMarkdown} />
                </div>
              </div>
            )}
          </div>
        </div>

        <div className="flex items-center gap-2">
          <Button
            variant="ghost"
            size="icon"
            className="h-9 w-9 text-destructive hover:bg-destructive/10 hover:text-destructive"
            title="Delete requirement"
            aria-label={`Delete requirement: ${props.item.title}`}
            disabled={props.enhancing}
            onClick={props.onDelete}
          >
            <Trash2 className="h-4 w-4" />
          </Button>

          <Button
            variant="outline"
            size="icon"
            title={canEnhance ? "Enhance with AI" : "Enhance with AI (unavailable)"}
            aria-label="Enhance with AI"
            disabled={!canEnhance || props.enhancing}
            onClick={() =>
              props.onEnhance?.({
                requirementId: props.item.id,
                requirementType: props.type,
                currentContent: props.item,
              })
            }
          >
            <Sparkles className="h-4 w-4" />
          </Button>

          {/* Focus edit (icon tag) */}
          <RequirementFocusEditorDialog
            item={props.item}
            type={props.type}
            canEnhance={canEnhance}
            enhancing={props.enhancing}
            onEnhance={props.onEnhance}
            onDelete={props.onDelete}
            onCommit={props.onChange}
          />
        </div>
      </div>
    </div>
  );
}

function RequirementFocusEditorDialog(props: {
  item: Requirement;
  type: RequirementType;
  canEnhance: boolean;
  enhancing: boolean;
  onEnhance?: (args: { requirementId: string; requirementType: RequirementType; currentContent: unknown }) => void;
  onDelete: () => void;
  onCommit: (next: Requirement) => void;
}) {
  const [open, setOpen] = useState(false);
  const [draftTitle, setDraftTitle] = useState(props.item.title);
  const [draftPriority, setDraftPriority] = useState(props.item.priority || "Must");
  const [draftDescription, setDraftDescription] = useState(props.item.description);
  const [draftAcceptanceText, setDraftAcceptanceText] = useState(() =>
    joinAcceptanceCriteria(props.item.acceptance_criteria),
  );

  useEffect(() => {
    if (!open) return;
    setDraftTitle(props.item.title);
    setDraftPriority(props.item.priority || "Must");
    setDraftDescription(props.item.description);
    setDraftAcceptanceText(joinAcceptanceCriteria(props.item.acceptance_criteria));
  }, [open, props.item]);

  const draftAcceptanceList = useMemo(
    () => splitAcceptanceCriteria(draftAcceptanceText),
    [draftAcceptanceText],
  );

  const draftItem = useMemo(() => {
    return updateRequirement(props.item, {
      title: draftTitle,
      priority: draftPriority,
      description: draftDescription,
      acceptance_criteria: draftAcceptanceList,
    });
  }, [draftAcceptanceList, draftDescription, draftPriority, draftTitle, props.item]);

  const previewMarkdown = useMemo(() => {
    const ac =
      draftAcceptanceList.length > 0
        ? draftAcceptanceList.map((l) => `- ${l}`).join("\n")
        : "_No acceptance criteria yet._";
    return [
      `## ${draftTitle || "(untitled)"}`,
      `**Priority:** ${draftPriority || "Must"}`,
      "",
      draftDescription?.trim() ? draftDescription : "_No description yet._",
      "",
      "### Acceptance criteria",
      ac,
    ].join("\n");
  }, [draftAcceptanceList, draftDescription, draftPriority, draftTitle]);

  function cancel() {
    setOpen(false);
  }

  function saveAndClose() {
    props.onCommit(draftItem);
    setOpen(false);
  }

  return (
    <Dialog open={open} onOpenChange={setOpen}>
      <DialogTrigger asChild>
        <Button variant="outline" size="icon" title="Focus edit" aria-label="Focus edit">
          <Expand className="h-4 w-4" />
        </Button>
      </DialogTrigger>
      {/* Offset from sticky app header (computed at runtime via `--app-header-h`). */}
      {/* Override base DialogContent `overflow-auto` — fullscreen editors must NOT scroll at the dialog level. */}
      <DialogContent className="fixed left-0 right-0 top-[var(--app-header-h)] bottom-0 z-80 w-screen max-w-none max-h-none translate-x-0 translate-y-0 overflow-hidden rounded-none border-0 p-0 shadow-none">
        <div className="flex h-full flex-col bg-background">
          <div className="border-b border-border px-6 py-3">
            <div className="flex items-start justify-between gap-3">
              <DialogHeader className="space-y-0">
                <DialogTitle>{draftTitle}</DialogTitle>
                <DialogDescription className="sr-only">
                  Edit requirement fields on the left and preview the rendered Markdown/Mermaid on the right.
                </DialogDescription>
              </DialogHeader>
              <div className="flex items-center gap-2">
                <Button
                  variant="outline"
                  size="sm"
                  className="gap-2"
                  disabled={!props.canEnhance || props.enhancing}
                  title={props.canEnhance ? "Enhance with AI" : "Enhance with AI (unavailable)"}
                  onClick={() =>
                    props.onEnhance?.({
                      requirementId: props.item.id,
                      requirementType: props.type,
                      currentContent: draftItem,
                    })
                  }
                >
                  <Sparkles className="h-4 w-4" />
                  Enhance with AI
                </Button>
                <Button
                  variant="outline"
                  size="icon"
                  className="h-9 w-9 rounded-full"
                  aria-label="Close"
                  title="Close"
                  onClick={cancel}
                >
                  <X className="h-4 w-4" />
                </Button>
              </div>
            </div>
          </div>
          <div className="min-h-0 flex-1 overflow-hidden px-6 py-4">
            <div className="grid h-full min-h-0 gap-4 lg:grid-cols-2">
              <div className="h-full min-h-0 overflow-auto pr-1">
                <div className="grid gap-3">
                  <div>
                    <div className="text-xs font-medium text-muted-foreground">Title</div>
                    <input
                      className="mt-1 w-full rounded-md border border-border bg-background px-3 py-2 text-sm"
                      value={draftTitle}
                      onChange={(e) => setDraftTitle(e.target.value)}
                    />
                  </div>

                  <div>
                    <div className="text-xs font-medium text-muted-foreground">Priority</div>
                    <div className="mt-1 w-full">
                      <Select value={draftPriority} onValueChange={(v) => setDraftPriority(v)}>
                        <SelectTrigger className="w-full">
                          <SelectValue placeholder="Priority" />
                        </SelectTrigger>
                        <SelectContent>
                          {["Must", "Should", "Could", "Won’t"].map((p) => (
                            <SelectItem key={p} value={p}>
                              {p}
                            </SelectItem>
                          ))}
                        </SelectContent>
                      </Select>
                    </div>
                  </div>

                  <div>
                    <div className="text-xs font-medium text-muted-foreground">Description (markdown)</div>
                    <textarea
                      className="mt-1 w-full rounded-md border border-border bg-background px-3 py-2 text-sm"
                      rows={10}
                      value={draftDescription}
                      onChange={(e) => setDraftDescription(e.target.value)}
                    />
                  </div>

                  <div>
                    <div className="text-xs font-medium text-muted-foreground">Acceptance criteria (one per line)</div>
                    <textarea
                      className="mt-1 w-full rounded-md border border-border bg-background px-3 py-2 text-sm font-mono"
                      rows={10}
                      value={draftAcceptanceText}
                      onChange={(e) => setDraftAcceptanceText(e.target.value)}
                    />
                  </div>
                </div>
              </div>

              <div className="h-full min-h-0 overflow-auto rounded-lg border border-border bg-muted/10 p-4">
                <div className="text-xs font-semibold text-muted-foreground">Preview</div>
                <div className="prose prose-sm mt-3 max-w-none dark:prose-invert">
                  <Markdown markdown={previewMarkdown} />
                </div>
              </div>
            </div>
          </div>

          <div className="shrink-0 border-t border-border px-6 py-3">
            <div className="flex items-center justify-between gap-3">
              <Button variant="outline" onClick={cancel}>
                Cancel
              </Button>
              <div className="flex items-center gap-2">
                <Button
                  variant="outline"
                  className="text-destructive hover:bg-destructive/10 hover:text-destructive"
                  onClick={() => {
                    props.onDelete();
                    setOpen(false);
                  }}
                >
                  Delete
                </Button>
                <Button onClick={saveAndClose}>Save</Button>
              </div>
            </div>
          </div>
        </div>
      </DialogContent>
    </Dialog>
  );
}



