import { useMemo, useState } from "react";
import { AnimatePresence, motion } from "motion/react";
import { Expand, Sparkles, Trash2, X } from "lucide-react";

import type { BacklogEpic, BacklogTask, GeneratedBacklogBundle, SimilarMatch } from "../../routes/tasks.types";
import { motionDuration } from "../../lib/motion";
import { Markdown } from "../markdown/Markdown";
import { Button } from "../ui/button";
import { Badge } from "../ui/badge";
import { Select, SelectContent, SelectItem, SelectTrigger, SelectValue } from "../ui/select";
import { Collapsible, CollapsibleContent, CollapsibleTrigger } from "../ui/collapsible";
import { Dialog, DialogContent, DialogDescription, DialogHeader, DialogTitle, DialogTrigger } from "../ui/dialog";
import {
  deleteEpicFromBundle,
  deleteTaskFromBundle,
  setEpicSimilarDecision,
  setEpicTargetProject,
  setTaskSimilarDecision,
  setTaskTargetProject,
  type LinkDecision,
} from "./backlog.edit";

export function BacklogEditor(props: {
  backlog: GeneratedBacklogBundle;
  gitlabProjectIds: string[];
  onChange: (next: GeneratedBacklogBundle) => void;
  onEnhance: (args: {
    itemId: string;
    itemType: "epic" | "task";
    currentContent: unknown;
    parentEpicContent?: unknown;
  }) => void;
}) {
  return (
    <div className="space-y-3">
      {props.backlog.epics.map((e, epicIdx) => (
        <EpicCard
          key={e.id}
          epic={e}
          epicIndex={epicIdx}
          gitlabProjectIds={props.gitlabProjectIds}
          onUpdateEpic={(patch) =>
            props.onChange({
              ...props.backlog,
              epics: props.backlog.epics.map((x) => (x.id === e.id ? { ...x, ...patch } : x)),
            })
          }
          onSetEpicTarget={(target) =>
            props.onChange(setEpicTargetProject(props.backlog, e.id, target))
          }
          onSetEpicDecision={(matchId, decision) =>
            props.onChange(setEpicSimilarDecision(props.backlog, e.id, matchId, decision))
          }
          onUpdateTask={(taskId, patch) =>
            props.onChange({
              ...props.backlog,
              epics: props.backlog.epics.map((ep) =>
                ep.id !== e.id
                  ? ep
                  : {
                      ...ep,
                      tasks: ep.tasks.map((t) => (t.id === taskId ? { ...t, ...patch } : t)),
                    },
              ),
            })
          }
          onSetTaskTarget={(taskId, target) =>
            props.onChange(setTaskTargetProject(props.backlog, taskId, target))
          }
          onSetTaskDecision={(taskId, matchId, decision) =>
            props.onChange(setTaskSimilarDecision(props.backlog, taskId, matchId, decision))
          }
          onDeleteEpic={() => props.onChange(deleteEpicFromBundle(props.backlog, e.id))}
          onDeleteTask={(taskId) => props.onChange(deleteTaskFromBundle(props.backlog, taskId))}
          onEnhance={props.onEnhance}
        />
      ))}
    </div>
  );
}

function EpicCard(props: {
  epic: BacklogEpic;
  epicIndex: number;
  gitlabProjectIds: string[];
  onUpdateEpic: (patch: Partial<Pick<BacklogEpic, "title" | "description">>) => void;
  onSetEpicTarget: (target: string | null) => void;
  onSetEpicDecision: (matchId: string, decision: LinkDecision) => void;
  onUpdateTask: (taskId: string, patch: Partial<Pick<BacklogTask, "title" | "description">>) => void;
  onSetTaskTarget: (taskId: string, target: string | null) => void;
  onSetTaskDecision: (taskId: string, matchId: string, decision: LinkDecision) => void;
  onDeleteEpic: () => void;
  onDeleteTask: (taskId: string) => void;
  onEnhance: (args: {
    itemId: string;
    itemType: "epic" | "task";
    currentContent: unknown;
    parentEpicContent?: unknown;
  }) => void;
}) {
  const parentEpicContent = useMemo(
    () => ({ id: props.epic.id, title: props.epic.title, description: props.epic.description }),
    [props.epic.description, props.epic.id, props.epic.title],
  );
  const [isOpen, setIsOpen] = useState(true);
  const [editing, setEditing] = useState<null | "title" | "description">(null);
  const [draftTitle, setDraftTitle] = useState(props.epic.title);
  const [draftDesc, setDraftDesc] = useState(props.epic.description);
  const [focusOpen, setFocusOpen] = useState(false);
  const [focusSnapshot, setFocusSnapshot] = useState<{ title: string; desc: string } | null>(null);

  return (
    <div className="rounded-md border border-border bg-card p-3 shadow-sm">
      <div className="flex flex-wrap items-start justify-between gap-3">
        <div>
          {editing === "title" ? (
            <input
              className="w-full rounded-md border border-border bg-background px-3 py-2 text-sm font-semibold"
              value={draftTitle}
              autoFocus
              onChange={(e) => setDraftTitle(e.target.value)}
              onBlur={() => {
                setEditing(null);
                props.onUpdateEpic({ title: draftTitle, description: draftDesc });
              }}
            />
          ) : (
            <div
              role="button"
              tabIndex={0}
              className="rounded-md px-2 py-1 text-sm font-semibold outline-none cursor-text hover:bg-muted/30 hover:ring-2 hover:ring-primary/15 focus-visible:ring-2 focus-visible:ring-ring"
              title="Click to edit title"
              onClick={() => setEditing("title")}
              onKeyDown={(e) => {
                if (e.key === "Enter") setEditing("title");
              }}
            >
              {props.epic.title}
            </div>
          )}
          <div className="mt-1 flex items-center gap-2 text-xs text-muted-foreground">
            <span>epic #{props.epicIndex + 1}</span>
            <span>·</span>
            <Badge variant="outline">{props.epic.tasks.length} tasks</Badge>
          </div>
        </div>

        <div className="flex items-center gap-2">
          <TargetProjectSelect
            value={props.epic.target_project_id ?? ""}
            options={props.gitlabProjectIds}
            onChange={(v) => props.onSetEpicTarget(v || null)}
          />
          <Button
            variant="ghost"
            size="icon"
            className="h-9 w-9 text-destructive hover:bg-destructive/10 hover:text-destructive"
            title="Delete epic"
            aria-label={`Delete epic: ${props.epic.title}`}
            onClick={props.onDeleteEpic}
          >
            <Trash2 className="h-4 w-4" />
          </Button>
          <Button
            variant="outline"
            size="icon"
            title="Enhance with AI"
            aria-label="Enhance with AI"
            onClick={() =>
              props.onEnhance({
                itemId: props.epic.id,
                itemType: "epic",
                currentContent: props.epic,
              })
            }
          >
            <Sparkles className="h-4 w-4" />
          </Button>

          <Dialog
            open={focusOpen}
            onOpenChange={(next) => {
              if (next) setFocusSnapshot({ title: draftTitle, desc: draftDesc });
              setFocusOpen(next);
            }}
          >
            <DialogTrigger asChild>
              <Button variant="outline" size="icon" title="Focus edit" aria-label="Focus edit">
                <Expand className="h-4 w-4" />
              </Button>
            </DialogTrigger>
            {/* Override base DialogContent `overflow-auto` — fullscreen editors must NOT scroll at the dialog level. */}
            <DialogContent className="fixed left-0 right-0 top-[var(--app-header-h)] bottom-0 z-80 w-screen max-w-none max-h-none translate-x-0 translate-y-0 overflow-hidden rounded-none border-0 p-0 shadow-none">
              <div className="flex h-full flex-col bg-background">
                <div className="border-b border-border px-6 py-3">
                  <div className="flex items-start justify-between gap-3">
                    <DialogHeader className="space-y-0">
                      <DialogTitle>Epic</DialogTitle>
                      <DialogDescription className="sr-only">
                        Edit epic fields on the left and preview the rendered Markdown/Mermaid on the right.
                      </DialogDescription>
                    </DialogHeader>
                    <div className="flex items-center gap-2">
                      <Button
                        variant="outline"
                        size="sm"
                        className="gap-2"
                        title="Enhance with AI"
                        onClick={() =>
                          props.onEnhance({
                            itemId: props.epic.id,
                            itemType: "epic",
                            currentContent: { ...props.epic, title: draftTitle, description: draftDesc },
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
                        onClick={() => {
                          const snap = focusSnapshot ?? { title: props.epic.title, desc: props.epic.description };
                          setDraftTitle(snap.title);
                          setDraftDesc(snap.desc);
                          setFocusOpen(false);
                        }}
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
                          <div className="text-xs font-medium text-muted-foreground">Description (markdown)</div>
                          <textarea
                            className="mt-1 w-full rounded-md border border-border bg-background px-3 py-2 text-sm"
                            rows={14}
                            value={draftDesc}
                            onChange={(e) => setDraftDesc(e.target.value)}
                          />
                        </div>
                      </div>
                    </div>

                    <div className="h-full min-h-0 overflow-auto rounded-lg border border-border bg-muted/10 p-4">
                      <div className="text-xs font-semibold text-muted-foreground">Preview</div>
                      <div className="prose prose-sm mt-3 max-w-none dark:prose-invert">
                        <Markdown markdown={`## ${draftTitle}\n\n${draftDesc || "_No description yet._"}`} />
                      </div>
                    </div>
                  </div>
                </div>

                <div className="shrink-0 border-t border-border px-6 py-3">
                  <div className="flex items-center justify-between gap-3">
                    <Button
                      variant="outline"
                      onClick={() => {
                        const snap = focusSnapshot ?? { title: props.epic.title, desc: props.epic.description };
                        setDraftTitle(snap.title);
                        setDraftDesc(snap.desc);
                        setFocusOpen(false);
                      }}
                    >
                      Cancel
                    </Button>
                    <div className="flex items-center gap-2">
                      <Button
                        variant="outline"
                        className="text-destructive hover:bg-destructive/10 hover:text-destructive"
                        onClick={() => {
                          props.onDeleteEpic();
                          setFocusOpen(false);
                        }}
                      >
                        Delete
                      </Button>
                      <Button
                        onClick={() => {
                          props.onUpdateEpic({ title: draftTitle, description: draftDesc });
                          setFocusOpen(false);
                        }}
                      >
                        Save
                      </Button>
                    </div>
                  </div>
                </div>
              </div>
            </DialogContent>
          </Dialog>
        </div>
      </div>

      <div className="prose prose-sm mt-3 max-w-none dark:prose-invert">
        {editing === "description" ? (
          <textarea
            className="w-full rounded-md border border-border bg-background px-3 py-2 text-sm"
            rows={8}
            value={draftDesc}
            autoFocus
            onChange={(e) => setDraftDesc(e.target.value)}
            onBlur={() => {
              setEditing(null);
              props.onUpdateEpic({ title: draftTitle, description: draftDesc });
            }}
          />
        ) : (
          <div
            role="button"
            tabIndex={0}
            className="rounded-md px-2 py-2 cursor-text hover:bg-muted/20 hover:ring-2 hover:ring-primary/15 focus-visible:ring-2 focus-visible:ring-ring"
            title="Click to edit description"
            onClick={() => setEditing("description")}
            onKeyDown={(e) => {
              if (e.key === "Enter") setEditing("description");
            }}
          >
            <Markdown markdown={props.epic.description} />
          </div>
        )}
      </div>

      <SimilarMatches
        label="Similar epics"
        matches={props.epic.similar ?? []}
        onDecision={props.onSetEpicDecision}
      />

      <Collapsible open={isOpen} onOpenChange={setIsOpen}>
        <div className="mt-4 flex items-center justify-between">
          <div className="text-xs font-semibold">Tasks</div>
          <CollapsibleTrigger asChild>
            <Button variant="ghost" size="sm">
              {isOpen ? "Collapse" : "Expand"}
            </Button>
          </CollapsibleTrigger>
        </div>

        <AnimatePresence initial={false}>
          {isOpen ? (
            <CollapsibleContent asChild forceMount>
              <motion.div
                initial={{ height: 0, opacity: 0 }}
                animate={{ height: "auto", opacity: 1 }}
                exit={{ height: 0, opacity: 0 }}
                transition={{ duration: motionDuration(0.18), ease: "easeOut" }}
                className="overflow-hidden"
              >
                <div className="mt-2 space-y-2">
                  {props.epic.tasks.map((t) => (
                    <TaskCard
                      key={t.id}
                      task={t}
                      gitlabProjectIds={props.gitlabProjectIds}
                      onUpdate={(patch) => props.onUpdateTask(t.id, patch)}
                      onSetTarget={(target) => props.onSetTaskTarget(t.id, target)}
                      onSetDecision={(matchId, decision) =>
                        props.onSetTaskDecision(t.id, matchId, decision)
                      }
                      onDelete={() => props.onDeleteTask(t.id)}
                      onEnhance={() =>
                        props.onEnhance({
                          itemId: t.id,
                          itemType: "task",
                          currentContent: t,
                          parentEpicContent,
                        })
                      }
                    />
                  ))}
                </div>
              </motion.div>
            </CollapsibleContent>
          ) : null}
        </AnimatePresence>
      </Collapsible>
    </div>
  );
}

function TaskCard(props: {
  task: BacklogTask;
  gitlabProjectIds: string[];
  onUpdate: (patch: Partial<Pick<BacklogTask, "title" | "description">>) => void;
  onSetTarget: (target: string | null) => void;
  onSetDecision: (matchId: string, decision: LinkDecision) => void;
  onDelete: () => void;
  onEnhance: () => void;
}) {
  const [editing, setEditing] = useState<null | "title" | "description">(null);
  const [draftTitle, setDraftTitle] = useState(props.task.title);
  const [draftDesc, setDraftDesc] = useState(props.task.description);
  const [focusOpen, setFocusOpen] = useState(false);
  const [focusSnapshot, setFocusSnapshot] = useState<{ title: string; desc: string } | null>(null);

  return (
    <div className="rounded-md border border-border bg-background/50 p-3">
      <div className="flex flex-wrap items-start justify-between gap-3">
        {editing === "title" ? (
          <input
            className="w-full rounded-md border border-border bg-background px-3 py-2 text-sm font-medium"
            value={draftTitle}
            autoFocus
            onChange={(e) => setDraftTitle(e.target.value)}
            onBlur={() => {
              setEditing(null);
              props.onUpdate({ title: draftTitle, description: draftDesc });
            }}
          />
        ) : (
          <div
            role="button"
            tabIndex={0}
            className="rounded-md px-2 py-1 text-sm font-medium outline-none cursor-text hover:bg-muted/30 hover:ring-2 hover:ring-primary/15 focus-visible:ring-2 focus-visible:ring-ring"
            title="Click to edit title"
            onClick={() => setEditing("title")}
            onKeyDown={(e) => {
              if (e.key === "Enter") setEditing("title");
            }}
          >
            {props.task.title}
          </div>
        )}
        <div className="flex items-center gap-2">
          <TargetProjectSelect
            value={props.task.target_project_id ?? ""}
            options={props.gitlabProjectIds}
            onChange={(v) => props.onSetTarget(v || null)}
          />
          <Button
            variant="ghost"
            size="icon"
            className="h-9 w-9 text-destructive hover:bg-destructive/10 hover:text-destructive"
            title="Delete task"
            aria-label={`Delete task: ${props.task.title}`}
            onClick={props.onDelete}
          >
            <Trash2 className="h-4 w-4" />
          </Button>
          <Button variant="outline" size="icon" title="Enhance with AI" aria-label="Enhance with AI" onClick={props.onEnhance}>
            <Sparkles className="h-4 w-4" />
          </Button>

          <Dialog
            open={focusOpen}
            onOpenChange={(next) => {
              if (next) setFocusSnapshot({ title: draftTitle, desc: draftDesc });
              setFocusOpen(next);
            }}
          >
            <DialogTrigger asChild>
              <Button variant="outline" size="icon" title="Focus edit" aria-label="Focus edit">
                <Expand className="h-4 w-4" />
              </Button>
            </DialogTrigger>
            {/* Override base DialogContent `overflow-auto` — fullscreen editors must NOT scroll at the dialog level. */}
            <DialogContent className="fixed left-0 right-0 top-[var(--app-header-h)] bottom-0 z-80 w-screen max-w-none max-h-none translate-x-0 translate-y-0 overflow-hidden rounded-none border-0 p-0 shadow-none">
              <div className="flex h-full flex-col bg-background">
                <div className="border-b border-border px-6 py-3">
                  <div className="flex items-start justify-between gap-3">
                    <DialogHeader className="space-y-0">
                      <DialogTitle>Task</DialogTitle>
                      <DialogDescription className="sr-only">
                        Edit task fields on the left and preview the rendered Markdown/Mermaid on the right.
                      </DialogDescription>
                    </DialogHeader>
                    <div className="flex items-center gap-2">
                      <Button variant="outline" size="sm" className="gap-2" title="Enhance with AI" onClick={props.onEnhance}>
                        <Sparkles className="h-4 w-4" />
                        Enhance with AI
                      </Button>
                      <Button
                        variant="outline"
                        size="icon"
                        className="h-9 w-9 rounded-full"
                        aria-label="Close"
                        title="Close"
                        onClick={() => {
                          const snap = focusSnapshot ?? { title: props.task.title, desc: props.task.description };
                          setDraftTitle(snap.title);
                          setDraftDesc(snap.desc);
                          setFocusOpen(false);
                        }}
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
                          <div className="text-xs font-medium text-muted-foreground">Description (markdown)</div>
                          <textarea
                            className="mt-1 w-full rounded-md border border-border bg-background px-3 py-2 text-sm"
                            rows={14}
                            value={draftDesc}
                            onChange={(e) => setDraftDesc(e.target.value)}
                          />
                        </div>
                      </div>
                    </div>

                    <div className="h-full min-h-0 overflow-auto rounded-lg border border-border bg-muted/10 p-4">
                      <div className="text-xs font-semibold text-muted-foreground">Preview</div>
                      <div className="prose prose-sm mt-3 max-w-none dark:prose-invert">
                        <Markdown markdown={`## ${draftTitle}\n\n${draftDesc || "_No description yet._"}`} />
                      </div>
                    </div>
                  </div>
                </div>

                <div className="shrink-0 border-t border-border px-6 py-3">
                  <div className="flex items-center justify-between gap-3">
                    <Button
                      variant="outline"
                      onClick={() => {
                        const snap = focusSnapshot ?? { title: props.task.title, desc: props.task.description };
                        setDraftTitle(snap.title);
                        setDraftDesc(snap.desc);
                        setFocusOpen(false);
                      }}
                    >
                      Cancel
                    </Button>
                    <div className="flex items-center gap-2">
                      <Button
                        variant="outline"
                        className="text-destructive hover:bg-destructive/10 hover:text-destructive"
                        onClick={() => {
                          props.onDelete();
                          setFocusOpen(false);
                        }}
                      >
                        Delete
                      </Button>
                      <Button
                        onClick={() => {
                          props.onUpdate({ title: draftTitle, description: draftDesc });
                          setFocusOpen(false);
                        }}
                      >
                        Save
                      </Button>
                    </div>
                  </div>
                </div>
              </div>
            </DialogContent>
          </Dialog>
        </div>
      </div>

      <div className="prose prose-sm mt-3 max-w-none dark:prose-invert">
        {editing === "description" ? (
          <textarea
            className="w-full rounded-md border border-border bg-background px-3 py-2 text-sm"
            rows={8}
            value={draftDesc}
            autoFocus
            onChange={(e) => setDraftDesc(e.target.value)}
            onBlur={() => {
              setEditing(null);
              props.onUpdate({ title: draftTitle, description: draftDesc });
            }}
          />
        ) : (
          <div
            role="button"
            tabIndex={0}
            className="rounded-md px-2 py-2 cursor-text hover:bg-muted/20 hover:ring-2 hover:ring-primary/15 focus-visible:ring-2 focus-visible:ring-ring"
            title="Click to edit description"
            onClick={() => setEditing("description")}
            onKeyDown={(e) => {
              if (e.key === "Enter") setEditing("description");
            }}
          >
            <Markdown markdown={props.task.description} />
          </div>
        )}
      </div>

      <SimilarMatches
        label="Similar issues"
        matches={props.task.similar ?? []}
        onDecision={props.onSetDecision}
      />
    </div>
  );
}

function TargetProjectSelect(props: {
  value: string;
  options: string[];
  onChange: (v: string) => void;
}) {
  const DEFAULT_TARGET_PROJECT_VALUE = "__default__";
  const selectValue = props.value === "" ? DEFAULT_TARGET_PROJECT_VALUE : props.value;
  return (
    <div className="w-40">
      <Select
        value={selectValue}
        onValueChange={(v) => props.onChange(v === DEFAULT_TARGET_PROJECT_VALUE ? "" : v)}
      >
        <SelectTrigger>
          <SelectValue placeholder="Route to…" />
        </SelectTrigger>
        <SelectContent>
          <SelectItem value={DEFAULT_TARGET_PROJECT_VALUE}>(default)</SelectItem>
          {props.options.map((id) => (
            <SelectItem key={id} value={id}>
              {id}
            </SelectItem>
          ))}
        </SelectContent>
      </Select>
    </div>
  );
}

function SimilarMatches(props: {
  label: string;
  matches: SimilarMatch[];
  onDecision: (matchId: string, decision: LinkDecision) => void;
}) {
  if (props.matches.length === 0) return null;

  return (
    <div className="mt-3 rounded-md border border-border bg-muted/30 p-3">
      <div className="text-xs font-semibold">{props.label}</div>
      <div className="mt-2 space-y-2">
        {props.matches.map((m) => (
          <div key={m.id} className="flex items-start justify-between gap-2">
            <div className="min-w-0">
              <div className="truncate text-sm">{m.title}</div>
              <div className="mt-1 text-xs text-muted-foreground">
                iid: {m.iid} · project: {m.project_id}
              </div>
            </div>
            <div className="flex items-center gap-1">
              <DecisionButton
                active={m.link_decision === "accepted"}
                onClick={() => props.onDecision(m.id, m.link_decision === "accepted" ? null : "accepted")}
              >
                Accept
              </DecisionButton>
              <DecisionButton
                active={m.link_decision === "rejected"}
                onClick={() => props.onDecision(m.id, m.link_decision === "rejected" ? null : "rejected")}
              >
                Reject
              </DecisionButton>
            </div>
          </div>
        ))}
      </div>
    </div>
  );
}

function DecisionButton(props: { active: boolean; onClick: () => void; children: React.ReactNode }) {
  return (
    <Button
      variant={props.active ? "default" : "outline"}
      size="sm"
      onClick={props.onClick}
    >
      {props.children}
    </Button>
  );
}


