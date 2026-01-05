import type { Route } from "./+types/projects";
import { useEffect, useMemo, useState } from "react";
import { Link, useSearchParams } from "react-router";
import { ArrowRight, ListTodo, Plus } from "lucide-react";

import { Button } from "../components/ui/button";
import { RouteErrorBoundary } from "../components/RouteErrorBoundary";
import { Badge } from "../components/ui/badge";
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from "../components/ui/card";
import { uiFetch } from "../lib/uiFetch.server";
import { buildCacheEmbeddingsPath, parseBacklogUrls } from "./projects.utils";
import { Page } from "../components/Page";
import { useUiStore } from "../state/store";
import { ProjectDialog } from "../components/projects/ProjectDialog";
import { DeleteProjectButton } from "../components/projects/DeleteProjectButton";
import { UploadDocumentsPanel } from "../components/projects/UploadDocumentsPanel";
import { GitlabBacklogPanel } from "../components/projects/GitlabBacklogPanel";
import { GitlabRepoPanel } from "../components/projects/GitlabRepoPanel";
import { isRecord } from "../lib/guards";

type Project = {
  id: string;
  name: string;
  description?: string | null;
  gitlab_repository_urls?: string[];
  gitlab_backlog_project_ids?: string[];
  gitlab_backlog_project_urls?: string[];
  status: string;
  created_at: string;
  updated_at: string;
};

export async function loader({ request }: Route.LoaderArgs) {
  const res = await uiFetch({ request, path: "/project/projects" });
  const raw: unknown = await res.json();
  if (!Array.isArray(raw) || !raw.every((p) => isRecord(p) && typeof p.id === "string" && typeof p.name === "string")) {
    throw new Response("Invalid /project/projects response", { status: 502 });
  }
  const projects = raw as Project[];
  return { projects };
}

export const handle = {
  breadcrumb: () => ({ label: "Projects", to: "/projects" }),
};

type ActionIntent =
  | "create"
  | "update"
  | "delete"
  | "upload"
  | "cacheEmbeddings"
  | "indexRepo"
  | "uploadRepoZip";

type OkResponse = { ok: true };
type NotOkResponse = { ok: false };

function getString(formData: FormData, key: string) {
  return String(formData.get(key) ?? "");
}

function getTrimmed(formData: FormData, key: string) {
  return getString(formData, key).trim();
}

function getIntent(formData: FormData): ActionIntent | null {
  const raw = getString(formData, "intent");
  switch (raw) {
    case "create":
    case "update":
    case "delete":
    case "upload":
    case "cacheEmbeddings":
    case "indexRepo":
    case "uploadRepoZip":
      return raw;
    default:
      return null;
  }
}

async function handleCreateOrUpdate(args: {
  request: Request;
  formData: FormData;
  mode: "create" | "update";
}): Promise<OkResponse> {
  const { request, formData, mode } = args;

  const projectId = getTrimmed(formData, "projectId");
  const name = getTrimmed(formData, "name");
  const description = getTrimmed(formData, "description");
  const repoUrlsRaw = getString(formData, "gitlab_repository_urls");
  const repoUrls = repoUrlsRaw
    .split("\n")
    .map((x) => x.trim())
    .filter(Boolean);
  const backlogUrls = parseBacklogUrls(getString(formData, "gitlab_backlog_project_urls"));

  const payload = {
    name,
    description: description ? description : null,
    gitlab_repository_urls: repoUrls,
    gitlab_backlog_project_urls: backlogUrls.length ? backlogUrls : [],
  } satisfies {
    name: string;
    description: string | null;
    gitlab_repository_urls: string[];
    gitlab_backlog_project_urls: string[];
  };

  if (mode === "create") {
    await uiFetch({
      request,
      path: "/project/projects",
      init: {
        method: "POST",
        headers: { "Content-Type": "application/json" },
        body: JSON.stringify(payload),
      },
    });
    return { ok: true };
  }

  await uiFetch({
    request,
    path: `/project/projects/${projectId}`,
    init: {
      method: "PUT",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify(payload),
    },
  });
  return { ok: true };
}

async function handleDelete(args: { request: Request; formData: FormData }): Promise<OkResponse> {
  const projectId = getTrimmed(args.formData, "projectId");
  await uiFetch({
    request: args.request,
    path: `/project/projects/${projectId}`,
    init: { method: "DELETE" },
  });
  return { ok: true };
}

async function handleUpload(args: { request: Request; formData: FormData }): Promise<OkResponse> {
  const projectId = getTrimmed(args.formData, "projectId");
  const outgoing = new FormData();
  for (const [k, v] of args.formData.entries()) {
    if (k === "files") outgoing.append(k, v);
  }
  await uiFetch({
    request: args.request,
    path: `/project/projects/${projectId}/documents/upload`,
    init: { method: "POST", body: outgoing },
  });
  return { ok: true };
}

async function handleCacheEmbeddings(args: { request: Request; formData: FormData }): Promise<OkResponse> {
  const projectId = getTrimmed(args.formData, "projectId");
  const raw = getString(args.formData, "gitlab_project_ids");
  const ids = raw
    .split(",")
    .map((x) => x.trim())
    .filter(Boolean);

  await uiFetch({
    request: args.request,
    path: buildCacheEmbeddingsPath({ projectId, gitlabProjectIds: ids }),
    init: { method: "POST" },
  });
  return { ok: true };
}

async function handleIndexRepo(args: { request: Request; formData: FormData }): Promise<OkResponse> {
  const projectId = getTrimmed(args.formData, "projectId");
  const repoUrl = getTrimmed(args.formData, "repoUrl");
  const sourceLanguage = getTrimmed(args.formData, "source_language");

  await uiFetch({
    request: args.request,
    path: "/code-graph/ingest/git",
    init: {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({
        project_id: projectId,
        source_language: sourceLanguage,
        git_url: repoUrl,
      }),
    },
  });
  return { ok: true };
}

async function handleUploadRepoZip(args: { request: Request; formData: FormData }): Promise<OkResponse> {
  const projectId = getTrimmed(args.formData, "projectId");
  const sourceLanguage = getTrimmed(args.formData, "source_language");
  const outgoing = new FormData();
  outgoing.set("project_id", projectId);
  outgoing.set("source_language", sourceLanguage);
  for (const [k, v] of args.formData.entries()) {
    if (k === "file") outgoing.set("file", v);
  }
  await uiFetch({
    request: args.request,
    path: "/code-graph/ingest/zip",
    init: { method: "POST", body: outgoing },
  });
  return { ok: true };
}

export async function action({ request }: Route.ActionArgs) {
  const formData = await request.formData();
  const intent = getIntent(formData);
  if (!intent) return { ok: false } satisfies NotOkResponse;

  switch (intent) {
    case "create":
      return await handleCreateOrUpdate({ request, formData, mode: "create" });
    case "update":
      return await handleCreateOrUpdate({ request, formData, mode: "update" });
    case "delete":
      return await handleDelete({ request, formData });
    case "upload":
      return await handleUpload({ request, formData });
    case "cacheEmbeddings":
      return await handleCacheEmbeddings({ request, formData });
    case "indexRepo":
      return await handleIndexRepo({ request, formData });
    case "uploadRepoZip":
      return await handleUploadRepoZip({ request, formData });
  }
}

export default function ProjectsRoute({ loaderData }: Route.ComponentProps) {
  const [searchParams, setSearchParams] = useSearchParams();
  const [query, setQuery] = useState("");
  const selectedFromUrl = searchParams.get("selected");
  const [selectedId, setSelectedId] = useState<string | null>(
    selectedFromUrl ?? loaderData.projects[0]?.id ?? null,
  );
  useEffect(() => {
    if (!selectedFromUrl) return;
    if (loaderData.projects.some((p) => p.id === selectedFromUrl)) {
      setSelectedId(selectedFromUrl);
    }
  }, [loaderData.projects, selectedFromUrl]);

  // Keep selected project reflected in URL so the header (breadcrumbs + RT status) can use it.
  useEffect(() => {
    if (!selectedId) return;
    setSearchParams((prev) => {
      if (prev.get("selected") === selectedId) return prev;
      const next = new URLSearchParams(prev);
      next.set("selected", selectedId);
      return next;
    });
  }, [selectedId, setSearchParams]);

  const projects = useMemo(() => {
    const q = query.trim().toLowerCase();
    if (!q) return loaderData.projects;
    return loaderData.projects.filter((p) => p.name.toLowerCase().includes(q));
  }, [loaderData.projects, query]);

  const selected = useMemo(
    () => loaderData.projects.find((p) => p.id === selectedId) ?? null,
    [loaderData.projects, selectedId],
  );

  const progressLogByFeature = useUiStore((s) => s.progressLogByFeature);
  const clearProgressLog = useUiStore((s) => s.clearProgressLog);

  const formatFeatureLog = useMemo(() => {
    return (feature: "upload_documents" | "cache_embeddings" | "repo_index") => {
      const items = progressLogByFeature[feature] ?? [];
      return items.slice(-200).map((l) => `${new Date(l.at).toLocaleTimeString()} | ${String(l.text ?? "").trim()}`);
    };
  }, [progressLogByFeature]);

  const uploadLogLines = useMemo(() => formatFeatureLog("upload_documents"), [formatFeatureLog]);
  const backlogLogLines = useMemo(() => formatFeatureLog("cache_embeddings"), [formatFeatureLog]);
  const repoLogLines = useMemo(() => formatFeatureLog("repo_index"), [formatFeatureLog]);

  return (
    <Page>
      <h1 className="sr-only">Projects</h1>

      <div className="grid gap-6 lg:grid-cols-[22rem_1fr]">
        <div className="space-y-3">
          <label className="sr-only" htmlFor="search">
            Search projects
          </label>
          <input
            id="search"
            className="w-full rounded-md border border-border bg-background px-3 py-2 text-sm shadow-sm"
            value={query}
            onChange={(e) => setQuery(e.target.value)}
            placeholder="Search projects…"
          />

          <ProjectDialog
            mode="create"
            triggerSize="default"
            triggerVariant="ghost"
            triggerClassName={[
              "w-full justify-start rounded-md border border-border px-3 py-2 text-left transition-colors",
              "bg-card hover:bg-accent/60",
            ].join(" ")}
            triggerChildren={
              <span className="flex items-center gap-2 text-sm font-semibold">
                <span className="inline-flex h-5 w-5 items-center justify-center rounded-full border border-border">
                  <Plus className="h-3.5 w-3.5" />
                </span>
                Add new project
              </span>
            }
          />

          <div className="space-y-2">
            {projects.length ? (
              projects.map((p) => (
                <button
                  key={p.id}
                  type="button"
                  onClick={() => {
                    setSelectedId(p.id);
                    setSearchParams((prev) => {
                      const next = new URLSearchParams(prev);
                      next.set("selected", p.id);
                      return next;
                    });
                  }}
                  className={[
                    "w-full rounded-md border border-border px-3 py-2 text-left transition-colors",
                    selectedId === p.id ? "bg-accent" : "bg-card hover:bg-accent/60",
                  ].join(" ")}
                >
                  <div className="flex items-center justify-between gap-3">
                    <div className="min-w-0">
                      <div className="truncate text-sm font-semibold">{p.name}</div>
                      <div className="mt-0.5 truncate text-xs text-muted-foreground">
                        {p.description ?? "No description"}
                      </div>
                    </div>
                    <Badge variant="outline" className="shrink-0">
                      {p.status}
                    </Badge>
                  </div>
                </button>
              ))
            ) : (
              <Card>
                <CardHeader>
                  <CardDescription>No projects match your search.</CardDescription>
                </CardHeader>
              </Card>
            )}
          </div>
        </div>

        <div className="space-y-6">
          {selected ? (
            <Card className="border-0 shadow-none bg-transparent">
              <CardHeader className="flex-col items-stretch justify-start">
                <div className="flex items-start justify-between gap-3">
                  <div className="min-w-0">
                    <CardTitle className="text-base leading-none">{selected.name}</CardTitle>
                  </div>
                  <div className="flex flex-wrap items-center justify-end gap-2">
                    <ProjectDialog mode="edit" project={selected} />
                    <DeleteProjectButton projectId={selected.id} name={selected.name} />
                    <Link to={`/projects/${selected.id}/requirements`}>
                      <Button variant="outline" size="sm">
                        <ArrowRight className="h-4 w-4" />
                        Requirements
                      </Button>
                    </Link>
                    <Link to={`/projects/${selected.id}/tasks`}>
                      <Button variant="outline" size="sm">
                        <ListTodo className="h-4 w-4" />
                        Tasks
                      </Button>
                    </Link>
                  </div>
                </div>

                <CardDescription className="mt-1">
                  {selected.description ?? "No description"}
                </CardDescription>
              </CardHeader>
              <CardContent className="grid gap-3 lg:grid-cols-2 pt-3">
                <UploadDocumentsPanel
                  projectId={selected.id}
                  logLines={uploadLogLines}
                  clearLog={() => clearProgressLog("upload_documents")}
                />
                <GitlabBacklogPanel
                  project={selected}
                  logLines={backlogLogLines}
                  clearLog={() => clearProgressLog("cache_embeddings")}
                />
                <GitlabRepoPanel
                  projectId={selected.id}
                  repoUrls={selected.gitlab_repository_urls ?? []}
                  logLines={repoLogLines}
                  clearLog={() => clearProgressLog("repo_index")}
                />
              </CardContent>
            </Card>
          ) : (
            <Card>
              <CardHeader>
                <CardTitle>Select a project</CardTitle>
                <CardDescription>Pick a project on the left to see details.</CardDescription>
              </CardHeader>
            </Card>
          )}
        </div>
      </div>
    </Page>
  );
}

export function ErrorBoundary() {
  return <RouteErrorBoundary />;
}