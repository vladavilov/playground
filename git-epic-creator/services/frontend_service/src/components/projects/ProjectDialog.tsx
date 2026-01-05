import { useEffect, useState } from "react";
import { useFetcher, useRevalidator } from "react-router";
import { Plus } from "lucide-react";

import { Button } from "../ui/button";
import { Dialog, DialogContent, DialogHeader, DialogTitle, DialogTrigger } from "../ui/dialog";
import { extractGitlabPath } from "./projectUtils";

export type Project = {
  id: string;
  name: string;
  description?: string | null;
  gitlab_repository_urls?: string[];
  gitlab_backlog_project_urls?: string[];
};

type Props =
  | {
      mode: "create";
      triggerClassName?: string;
      triggerSize?: "default" | "sm";
      triggerVariant?: "default" | "outline" | "ghost";
      triggerChildren?: React.ReactNode;
    }
  | {
      mode: "edit";
      project: Project;
      triggerClassName?: string;
      triggerSize?: "default" | "sm";
      triggerVariant?: "default" | "outline" | "ghost";
      triggerChildren?: React.ReactNode;
    };

export function ProjectDialog(props: Props) {
  const fetcher = useFetcher();
  const isEdit = props.mode === "edit";
  const revalidator = useRevalidator();
  const [open, setOpen] = useState(false);

  const project = isEdit ? props.project : null;
  const [name, setName] = useState(project?.name ?? "");
  const [description, setDescription] = useState(project?.description ?? "");
  const [repoUrls, setRepoUrls] = useState<string[]>(project?.gitlab_repository_urls ?? []);
  const [newRepoUrl, setNewRepoUrl] = useState("");
  const [repoFeedback, setRepoFeedback] = useState<string | null>(null);

  const [backlogUrls, setBacklogUrls] = useState<string[]>(project?.gitlab_backlog_project_urls ?? []);
  const [newBacklogUrl, setNewBacklogUrl] = useState("");
  const [backlogFeedback, setBacklogFeedback] = useState<string | null>(null);

  const isOk = (v: unknown): v is { ok: true } =>
    Boolean(v && typeof v === "object" && (v as { ok?: unknown }).ok === true);

  useEffect(() => {
    if (fetcher.state !== "idle") return;
    if (!isOk(fetcher.data)) return;

    setOpen(false);
    revalidator.revalidate();

    if (!isEdit) {
      setName("");
      setDescription("");
      setRepoUrls([]);
      setNewRepoUrl("");
      setRepoFeedback(null);
      setBacklogUrls([]);
      setNewBacklogUrl("");
      setBacklogFeedback(null);
    }
  }, [fetcher.data, fetcher.state, isEdit, revalidator]);

  function isValidGitlabRepoRef(raw: string) {
    const url = raw.trim();
    if (!url) return false;
    if (url.startsWith("git@")) return true;
    if (url.startsWith("ssh://")) return true;
    try {
      const parsed = new URL(url);
      if (parsed.protocol !== "https:" && parsed.protocol !== "http:") return false;
      if (!parsed.hostname) return false;
      const segments = parsed.pathname.split("/").filter(Boolean);
      return segments.length >= 2;
    } catch {
      return false;
    }
  }

  function addRepo() {
    const url = newRepoUrl.trim();
    if (!url) {
      setRepoFeedback("Please enter a GitLab repository URL.");
      return;
    }
    if (!isValidGitlabRepoRef(url)) {
      setRepoFeedback("Invalid URL. Expected: https://host/group/project or git@host:group/project.git");
      return;
    }
    if (repoUrls.includes(url)) {
      setRepoFeedback("This repository is already added.");
      return;
    }
    setRepoUrls((prev) => [...prev, url]);
    setNewRepoUrl("");
    setRepoFeedback(null);
  }

  function isValidGitlabUrl(url: string) {
    try {
      const parsed = new URL(url);
      if (parsed.protocol !== "https:" && parsed.protocol !== "http:") return false;
      if (!parsed.hostname) return false;
      const segments = parsed.pathname.split("/").filter(Boolean);
      return segments.length >= 2;
    } catch {
      return false;
    }
  }

  function addBacklogUrl() {
    const url = newBacklogUrl.trim();
    if (!url) {
      setBacklogFeedback("Please enter a GitLab project URL.");
      return;
    }
    if (!isValidGitlabUrl(url)) {
      setBacklogFeedback("Invalid URL. Expected: https://host/group/project (nested groups supported).");
      return;
    }
    if (backlogUrls.includes(url)) {
      setBacklogFeedback("This project is already added.");
      return;
    }
    setBacklogUrls((prev) => [...prev, url]);
    setNewBacklogUrl("");
    setBacklogFeedback(null);
  }

  return (
    <Dialog open={open} onOpenChange={setOpen}>
      <DialogTrigger asChild>
        <Button
          variant={props.triggerVariant ?? (isEdit ? "outline" : "default")}
          size={props.triggerSize ?? "sm"}
          className={props.triggerClassName}
        >
          {props.triggerChildren ??
            (isEdit ? (
              "Edit"
            ) : (
              <span className="flex items-center gap-2">
                <span className="inline-flex h-5 w-5 items-center justify-center rounded-full border border-border">
                  <Plus className="h-3.5 w-3.5" />
                </span>
                New project
              </span>
            ))}
        </Button>
      </DialogTrigger>
      <DialogContent>
        <DialogHeader>
          <DialogTitle>{isEdit ? "Edit project" : "Create project"}</DialogTitle>
        </DialogHeader>

        <fetcher.Form method="post" className="mt-3 space-y-3">
          <input type="hidden" name="intent" value={isEdit ? "update" : "create"} />
          {isEdit ? <input type="hidden" name="projectId" value={project!.id} /> : null}

          <div>
            <label className="text-sm" htmlFor="name">
              Name
            </label>
            <input
              id="name"
              name="name"
              className="mt-1 w-full rounded-md border border-border bg-background px-3 py-2 text-sm"
              value={name}
              onChange={(e) => setName(e.target.value)}
              required
            />
          </div>

          <div>
            <label className="text-sm" htmlFor="description">
              Description
            </label>
            <textarea
              id="description"
              name="description"
              className="mt-1 w-full rounded-md border border-border bg-background px-3 py-2 text-sm"
              rows={3}
              value={description}
              onChange={(e) => setDescription(e.target.value)}
            />
          </div>

          <div>
            <label className="text-sm" htmlFor="repo">
              GitLab repository URL(s)
            </label>
            <textarea
              name="gitlab_repository_urls"
              className="sr-only"
              readOnly
              value={repoUrls.join("\n")}
            />

            <div className="mt-1 flex items-center gap-2">
              <input
                id="repo"
                className="w-full rounded-md border border-border bg-background px-3 py-2 text-sm"
                value={newRepoUrl}
                onChange={(e) => {
                  setNewRepoUrl(e.target.value);
                  if (!e.target.value.trim()) setRepoFeedback(null);
                  else if (!isValidGitlabRepoRef(e.target.value.trim())) setRepoFeedback("⚠️ Invalid URL format");
                  else if (repoUrls.includes(e.target.value.trim())) setRepoFeedback("ℹ️ Already added");
                  else setRepoFeedback("✓ Looks valid");
                }}
                placeholder="https://gitlab.example.com/group/project or git@gitlab:group/project.git"
                onKeyDown={(e) => {
                  if (e.key === "Enter") {
                    e.preventDefault();
                    addRepo();
                  }
                }}
              />
              <Button type="button" variant="outline" size="sm" className="px-2" onClick={addRepo} aria-label="Add repository">
                <Plus className="h-4 w-4" />
              </Button>
            </div>
            {repoFeedback ? <div className="mt-1 text-xs text-muted-foreground">{repoFeedback}</div> : null}

            <div className="mt-2 flex flex-wrap gap-2">
              {repoUrls.length ? (
                repoUrls.map((u) => {
                  const display = extractGitlabPath(u) ?? u;
                  const isHttp = u.startsWith("http://") || u.startsWith("https://");
                  return (
                    <div
                      key={u}
                      className="inline-flex items-center gap-2 rounded-full border border-border bg-background px-3 py-1 text-xs font-medium"
                      title={u}
                    >
                      {isHttp ? (
                        <a
                          href={u}
                          target="_blank"
                          rel="noreferrer"
                          className="max-w-[18rem] truncate hover:underline"
                        >
                          {display}
                        </a>
                      ) : (
                        <span className="max-w-[18rem] truncate">{display}</span>
                      )}
                      <button
                        type="button"
                        className="text-muted-foreground hover:text-foreground"
                        aria-label="Remove repository"
                        onClick={() => setRepoUrls((prev) => prev.filter((x) => x !== u))}
                      >
                        ×
                      </button>
                    </div>
                  );
                })
              ) : (
                <div className="text-sm text-muted-foreground">No repositories added yet.</div>
              )}
            </div>
          </div>

          <div>
            <label className="text-sm" htmlFor="newBacklogUrl">
              Backlog projects (GitLab project URLs)
            </label>
            <div className="mt-1 flex items-center gap-2">
              <input
                id="newBacklogUrl"
                className="w-full rounded-md border border-border bg-background px-3 py-2 text-sm"
                value={newBacklogUrl}
                onChange={(e) => {
                  setNewBacklogUrl(e.target.value);
                  if (!e.target.value.trim()) setBacklogFeedback(null);
                  else if (!isValidGitlabUrl(e.target.value.trim())) setBacklogFeedback("⚠️ Invalid URL format");
                  else if (backlogUrls.includes(e.target.value.trim())) setBacklogFeedback("ℹ️ Already added");
                  else setBacklogFeedback("✓ Looks valid");
                }}
                placeholder="https://gitlab.example.com/group/subgroup/project"
                onKeyDown={(e) => {
                  if (e.key === "Enter") {
                    e.preventDefault();
                    addBacklogUrl();
                  }
                }}
              />
              <Button type="button" variant="outline" size="sm" className="px-2" onClick={addBacklogUrl} aria-label="Add backlog project">
                <Plus className="h-4 w-4" />
              </Button>
            </div>
            {backlogFeedback ? <div className="mt-1 text-xs text-muted-foreground">{backlogFeedback}</div> : null}

            <div className="mt-2 flex flex-wrap gap-2">
              {backlogUrls.length ? (
                backlogUrls.map((u) => (
                  <div
                    key={u}
                    className="inline-flex items-center gap-2 rounded-full border border-border bg-background px-3 py-1 text-xs font-medium"
                    title={u}
                  >
                    <span className="max-w-[18rem] truncate">{extractGitlabPath(u) ?? u}</span>
                    <button
                      type="button"
                      className="text-muted-foreground hover:text-foreground"
                      aria-label="Remove backlog project"
                      onClick={() => setBacklogUrls((prev) => prev.filter((x) => x !== u))}
                    >
                      ×
                    </button>
                  </div>
                ))
              ) : (
                <div className="text-sm text-muted-foreground">No backlog projects added yet.</div>
              )}
            </div>

            {/* Server expects newline-separated URLs; keep a hidden field for compatibility */}
            <textarea
              name="gitlab_backlog_project_urls"
              className="sr-only"
              readOnly
              value={backlogUrls.join("\n")}
            />
          </div>

          <div className="flex justify-end">
            <Button type="submit">{isEdit ? "Save" : "Create"}</Button>
          </div>
        </fetcher.Form>
      </DialogContent>
    </Dialog>
  );
}


