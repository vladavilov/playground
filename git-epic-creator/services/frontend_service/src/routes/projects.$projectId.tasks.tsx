import type { Route } from "./+types/projects.$projectId.tasks";

import { useEffect, useMemo, useRef, useState } from "react";
import { Link, useFetcher, useLocation, useSearchParams } from "react-router";

import { Button } from "../components/ui/button";
import { BacklogEditor } from "../components/tasks/BacklogEditor";
import { RouteErrorBoundary } from "../components/RouteErrorBoundary";
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from "../components/ui/card";
import { uiFetch } from "../lib/uiFetch.server";
import { parseJsonUnknown } from "../lib/json";
import { isRecord } from "../lib/guards";
import { useToastStore } from "../state/toast";
import { Page } from "../components/Page";
import type { RequirementsBundle } from "./requirements.types";
import { requirementsBundleToPrompt } from "./requirements.handoff";
import type { BacklogEpic, BacklogTask, EnhancedTask, GeneratedBacklogBundle, SimilarMatch } from "./tasks.types";
import { isBacklogEpic, isEnhancedTask, isGeneratedBacklogBundle } from "./tasks.validators";
import { buildApplyBacklogRequest } from "./tasks.submit";
import { ChatShell } from "../components/chat/ChatShell";
import { useUiStore } from "../state/store";
import { ScoreBadge } from "../components/ui/score-badge";
import { useChatSession } from "./useChatSession";

type Project = {
  id: string;
  name: string;
  gitlab_backlog_project_ids?: string[];
};

type ApplyBacklogResponse = {
  project_results?: Array<{
    project_id?: string;
    success?: boolean;
    results?: {
      epics?: Array<{ input_index?: number; action?: string; id?: string; web_url?: string }>;
      issues?: Array<{ input_index?: number; action?: string; id?: string; web_url?: string }>;
    };
    errors?: unknown[];
    error_message?: string | null;
  }>;
  total_epics_created?: number;
  total_issues_created?: number;
  total_errors?: number;
  projects_succeeded?: number;
  projects_failed?: number;
};

function isApplyBacklogResponse(v: unknown): v is ApplyBacklogResponse {
  if (!isRecord(v)) return false;
  if (!("project_results" in v) || !Array.isArray(v.project_results)) return false;
  return true;
}

function fmtCount(label: string, count: number | undefined) {
  if (typeof count !== "number") return null;
  return `- **${label}:** ${count}`;
}

function safeJsonStringify(v: unknown): string {
  try {
    return JSON.stringify(v, null, 2);
  } catch {
    return String(v);
  }
}

function formatGitlabSubmitSystemMessage(args: {
  submitOk?: boolean;
  submitStatus?: number;
  submitBody?: unknown;
}): string {
  const status = typeof args.submitStatus === "number" ? args.submitStatus : null;
  const ok = args.submitOk === true;

  if (!isApplyBacklogResponse(args.submitBody)) {
    const raw = typeof args.submitBody === "string" ? args.submitBody : safeJsonStringify(args.submitBody);
    return [
      `**GitLab submit:** ${ok ? "✅ OK" : "⚠️ Error"}${status ? ` (HTTP ${status})` : ""}`,
      "",
      "_Response format not recognized; showing raw payload._",
      "",
      "```json",
      raw,
      "```",
    ].join("\n");
  }

  const body = args.submitBody;
  const totalEpics = body.total_epics_created;
  const totalIssues = body.total_issues_created;
  const totalErrors = body.total_errors;

  const createdParts: string[] = [];
  if (typeof totalEpics === "number") createdParts.push(`${totalEpics} epic${totalEpics === 1 ? "" : "s"}`);
  if (typeof totalIssues === "number") createdParts.push(`${totalIssues} issue${totalIssues === 1 ? "" : "s"}`);
  const createdSummary = createdParts.length ? `Created ${createdParts.join(", ")}` : "Completed";

  const lines: string[] = [];
  lines.push(`**GitLab submit:** ${ok ? "✅" : "⚠️"} ${createdSummary}${status ? ` (HTTP ${status})` : ""}`);
  lines.push("");

  const counts = [
    fmtCount("Projects succeeded", body.projects_succeeded),
    fmtCount("Projects failed", body.projects_failed),
    fmtCount("Errors", totalErrors),
  ].filter(Boolean) as string[];
  if (counts.length) {
    lines.push(...counts);
    lines.push("");
  }

  // Per-project details with links (matches mock fixture + real-world style)
  for (const pr of body.project_results ?? []) {
    const projectId = pr.project_id ? String(pr.project_id) : "Unknown project";
    const projectOk = pr.success === true;
    lines.push(`### Project ${projectId} ${projectOk ? "✅" : "⚠️"}`);

    const epics = pr.results?.epics ?? [];
    const issues = pr.results?.issues ?? [];

    if (epics.length) {
      lines.push(`- **Epics** (${epics.length})`);
      for (const e of epics) {
        const action = e.action ? String(e.action) : "done";
        const id = e.id ? String(e.id) : "";
        const url = e.web_url ? String(e.web_url) : "";
        const label = id ? `Epic ${id}` : "Epic";
        const link = url ? `[${label}](${url})` : label;
        lines.push(`  - ${action}: ${link}`);
      }
    }

    if (issues.length) {
      lines.push(`- **Issues** (${issues.length})`);
      for (const i of issues) {
        const action = i.action ? String(i.action) : "done";
        const id = i.id ? String(i.id) : "";
        const url = i.web_url ? String(i.web_url) : "";
        const label = id ? `Issue ${id}` : "Issue";
        const link = url ? `[${label}](${url})` : label;
        lines.push(`  - ${action}: ${link}`);
      }
    }

    const errorMsg = pr.error_message ? String(pr.error_message).trim() : "";
    if (errorMsg) {
      lines.push(`- **Error:** ${errorMsg}`);
    } else if (Array.isArray(pr.errors) && pr.errors.length) {
      lines.push(`- **Errors:** ${pr.errors.length}`);
    }

    lines.push("");
  }

  // If there are errors, encourage the user to inspect and retry.
  if (typeof totalErrors === "number" && totalErrors > 0) {
    lines.push("_Some items failed. Review errors above, then adjust routing/links and re-submit._");
  }

  return lines.join("\n").trim();
}

export async function loader({ request, params }: Route.LoaderArgs) {
  const projectRes = await uiFetch({
    request,
    path: `/project/projects/${params.projectId}`,
  });
  const raw: unknown = await projectRes.json();
  if (!isRecord(raw) || typeof raw.id !== "string" || typeof raw.name !== "string") {
    throw new Response("Invalid /project/projects/:projectId response", { status: 502 });
  }
  const project = raw as Project;
  return { projectId: params.projectId, project };
}

export async function action({ request, params }: Route.ActionArgs) {
  const formData = await request.formData();
  const intent = String(formData.get("intent") ?? "");

  if (intent === "generate") {
    const message = String(formData.get("message") ?? "").trim();
    if (!message) return { ok: false };
    const prompt_id = String(formData.get("prompt_id") ?? "").trim();
    const payload = {
      project_id: params.projectId,
      message,
      ...(prompt_id ? { prompt_id } : {}),
    } satisfies { project_id: string; message: string; prompt_id?: string };

    const res = await uiFetch({
      request,
      path: "/tasks/generate",
      init: {
        method: "POST",
        headers: { "Content-Type": "application/json" },
        body: JSON.stringify(payload),
      },
    });
    const raw: unknown = await res.json();
    if (!isGeneratedBacklogBundle(raw)) {
      throw new Response("Invalid /tasks/generate response", { status: 502 });
    }
    const backlog = raw;
    return { backlog };
  }

  if (intent === "enhance") {
    const itemId = String(formData.get("item_id") ?? "");
    const itemType = String(formData.get("item_type") ?? "");
    const currentContent = parseJsonUnknown(String(formData.get("current_content") ?? "{}"), {});
    const parentEpicContentRaw = String(formData.get("parent_epic_content") ?? "");
    const parentEpicContent = parentEpicContentRaw
      ? parseJsonUnknown(parentEpicContentRaw, null)
      : null;

    const res = await uiFetch({
      request,
      path: "/tasks/enhance",
      init: {
        method: "POST",
        headers: { "Content-Type": "application/json" },
        body: JSON.stringify({
          project_id: params.projectId,
          item_id: itemId,
          item_type: itemType,
          current_content: currentContent,
          parent_epic_content: parentEpicContent,
        }),
      },
    });
    const raw: unknown = await res.json();
    if (!isEnhancedTask(raw)) {
      throw new Response("Invalid /tasks/enhance response", { status: 502 });
    }
    const enhanced = raw;
    return { enhanced };
  }

  if (intent === "submit") {
    const promptId = String(formData.get("prompt_id") ?? "");
    const defaultGitlabProjectId = String(formData.get("default_gitlab_project_id") ?? "");
    const epicsJson = String(formData.get("epics_json") ?? "[]");
    const parsed = parseJsonUnknown(epicsJson, []);
    if (!Array.isArray(parsed)) return { ok: false };
    if (!parsed.every(isBacklogEpic)) return { ok: false };
    const epics = parsed;

    const payload = buildApplyBacklogRequest({
      internalProjectId: params.projectId,
      promptId,
      defaultGitlabProjectId,
      epics,
    });

    const res = await uiFetch({
      request,
      path: "/gitlab/projects/apply-backlog",
      init: {
        method: "POST",
        headers: { "Content-Type": "application/json" },
        body: JSON.stringify(payload),
      },
    });
    const submitStatus = res.status;
    const submitOk = res.ok;
    const text = await res.text();
    const parsedBody = parseJsonUnknown(text, null);
    const submitBody = parsedBody === null ? text : parsedBody;
    return { submitStatus, submitOk, submitBody };
  }

  return { ok: false };
}

export default function TasksRoute({ loaderData, actionData }: Route.ComponentProps) {
  const loc = useLocation();
  const [searchParams] = useSearchParams();
  const generateFetcher = useFetcher<typeof action>();
  const enhanceFetcher = useFetcher<typeof action>();
  const submitFetcher = useFetcher<typeof action>();

  const reqBundle: RequirementsBundle | undefined =
    loc.state && typeof loc.state === "object" && "requirementsBundle" in loc.state
      ? (loc.state as { requirementsBundle?: RequirementsBundle }).requirementsBundle
      : undefined;

  const draft = useUiStore((s) => s.tasksDraft);
  const setDraft = useUiStore((s) => s.setTasksDraft);
  const activePromptId = useUiStore((s) => s.activePromptId);
  const thinkingByPromptId = useUiStore((s) => s.thinkingByPromptId);

  const [backlog, setBacklog] = useState<GeneratedBacklogBundle | null>(() => {
    if (actionData && typeof actionData === "object" && "backlog" in actionData) {
      return (actionData as { backlog?: GeneratedBacklogBundle }).backlog ?? null;
    }
    return null;
  });
  const showToast = useToastStore((s) => s.show);

  const enhanced: EnhancedTask | undefined =
    enhanceFetcher.data && typeof enhanceFetcher.data === "object" && "enhanced" in enhanceFetcher.data
      ? (enhanceFetcher.data as { enhanced: EnhancedTask }).enhanced
      : undefined;
  const lastAppliedEnhancementRef = useRef<string | null>(null);
  useEffect(() => {
    if (!backlog || !enhanced) return;
    if (lastAppliedEnhancementRef.current === enhanced.item_id) return;
    lastAppliedEnhancementRef.current = enhanced.item_id;

    setBacklog((st) => {
      if (!st) return st;
      const epics = st.epics.map((e) => {
        if (e.id === enhanced.item_id) {
          return { ...e, title: enhanced.title, description: enhanced.description };
        }
        return {
          ...e,
          tasks: e.tasks.map((t) =>
            t.id === enhanced.item_id
              ? {
                  ...t,
                  title: enhanced.title,
                  description: enhanced.description,
                  acceptance_criteria: enhanced.acceptance_criteria,
                  dependencies: enhanced.dependencies ?? t.dependencies,
                }
              : t,
          ),
        };
      });
      return { ...st, epics };
    });
  }, [backlog, enhanced]);

  const activeBacklog = backlog;
  const defaultGitlabProjectId = loaderData.project.gitlab_backlog_project_ids?.[0] ?? "";
  const gitlabProjectIds = loaderData.project.gitlab_backlog_project_ids ?? [];

  const isSending = generateFetcher.state !== "idle";
  const { messages, setMessages, send, appendText } = useChatSession({
    initialSystemMessage: "Chat with the agent to generate a backlog. Review and edit on the right.",
    draft,
    setDraft,
    generateFetcher,
    buildSubmitFormData: (text, promptId) => {
      const fd = new FormData();
      fd.set("intent", "generate");
      fd.set("message", text);
      if (promptId) fd.set("prompt_id", promptId);
      return fd;
    },
    extractResult: (data) =>
      data && typeof data === "object" && "backlog" in data
        ? ((data as { backlog?: GeneratedBacklogBundle }).backlog ?? null)
        : null,
    getResultPromptId: (b) => b.prompt_id ?? null,
    onResetForNewSend: () => setBacklog(null),
    onResult: ({ result: next, appendText }) => {
      setBacklog(next);
      const epicCount = next.epics?.length ?? 0;
      const taskCount = next.epics?.reduce((s, e) => s + (e.tasks?.length ?? 0), 0) ?? 0;
      const scorePercent = Math.round((next.score ?? 0) * 100);

      appendText({
        kind: "text",
        role: "assistant",
        meta: "AI Agent",
        content: `✓ Generated **${epicCount}** epic(s) with **${taskCount}** task(s)\n\nScore: **${scorePercent}%**\n\nReview and edit on the right.`,
      });

      if (next.markdown_text) {
        appendText({ kind: "text", role: "assistant", meta: "AI Agent", content: next.markdown_text });
      }
    },
    activePromptId,
    thinkingByPromptId,
  });

  // Legacy parity: if coming from Requirements confirmation, auto-run once.
  const autoRef = useRef(false);
  useEffect(() => {
    const fromReq = searchParams.get("from") === "requirements";
    if (!fromReq || autoRef.current) return;
    if (!reqBundle) return;
    autoRef.current = true;
    const seed = requirementsBundleToPrompt(reqBundle);
    appendText({ kind: "text", role: "user", content: "Generate tasks from confirmed requirements" });
    setDraft(seed);
    // send immediately (uses seed)
    send(seed);
  }, [appendText, reqBundle, searchParams, send, setDraft]);

  const submitResult =
    submitFetcher.data && typeof submitFetcher.data === "object" && "submitStatus" in submitFetcher.data
      ? (submitFetcher.data as { submitStatus?: number; submitOk?: boolean; submitBody?: unknown })
      : undefined;
  const didSubmitToastRef = useRef(false);
  const didSubmitChatRef = useRef(false);
  useEffect(() => {
    if (!submitResult || didSubmitToastRef.current) return;
    didSubmitToastRef.current = true;
    showToast({ title: "Submitted to GitLab", description: "See results in response." });
  }, [showToast, submitResult]);
  useEffect(() => {
    if (!submitResult || didSubmitChatRef.current) return;
    didSubmitChatRef.current = true;
    appendText({
      kind: "text",
      role: "system",
      content: formatGitlabSubmitSystemMessage(submitResult),
    });
  }, [appendText, submitResult]);

  return (
    <Page className="overflow-hidden">
      <ChatShell
        messages={messages}
        onToggleThinking={(id) =>
          setMessages((prev) =>
            prev.map((m) => (m.kind === "thinking" && m.id === id ? { ...m, collapsed: !m.collapsed } : m)),
          )
        }
        input={draft}
        setInput={setDraft}
        disabled={isSending}
        onSend={() => send(draft)}
        rightPanel={
          activeBacklog ? (
            <div className="flex min-h-0 flex-1 flex-col gap-3 overflow-hidden">
              <div className="flex items-center justify-between gap-2 shrink-0">
                <ScoreBadge score={activeBacklog.score} />
                {defaultGitlabProjectId ? (
                  <submitFetcher.Form method="post">
                    <input type="hidden" name="intent" value="submit" />
                    <input type="hidden" name="prompt_id" value={activeBacklog.prompt_id} />
                    <input type="hidden" name="default_gitlab_project_id" value={defaultGitlabProjectId} />
                    <input type="hidden" name="epics_json" value={JSON.stringify(activeBacklog.epics)} />
                    <Button type="submit" size="sm" disabled={submitFetcher.state !== "idle"}>
                      {submitFetcher.state !== "idle" ? "Submitting..." : "Submit to GitLab"}
                    </Button>
                  </submitFetcher.Form>
                ) : null}
              </div>
              <div className="min-h-0 flex-1 overflow-auto pr-1" data-testid="tasks-editor-scroll">
                <BacklogEditor
                  backlog={activeBacklog}
                  gitlabProjectIds={gitlabProjectIds}
                  onChange={setBacklog}
                  onEnhance={(args) => {
                    const f = new FormData();
                    f.set("intent", "enhance");
                    f.set("item_id", args.itemId);
                    f.set("item_type", args.itemType);
                    f.set("current_content", JSON.stringify(args.currentContent));
                    if (args.parentEpicContent)
                      f.set("parent_epic_content", JSON.stringify(args.parentEpicContent));
                    enhanceFetcher.submit(f, { method: "post" });
                  }}
                />
              </div>
            </div>
          ) : (
            <div className="text-sm text-muted-foreground">
              Generate a backlog to review epics/tasks here.
            </div>
          )
        }
      />
    </Page>
  );
}

export function ErrorBoundary() {
  return <RouteErrorBoundary />;
}
