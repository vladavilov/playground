import type { Route } from "./+types/projects.$projectId.requirements";

import { useEffect, useMemo, useRef, useState } from "react";
import { useFetcher, useNavigate } from "react-router";

import { uiFetch } from "../lib/uiFetch.server";
import { parseJsonUnknown } from "../lib/json";
import { isRecord } from "../lib/guards";
import type { ClarificationQuestion, RequirementsBundle, Requirement } from "./requirements.types";
import { isRequirement, isRequirementsBundle } from "./requirements.validators";
import { RequirementsEditor } from "../components/requirements/RequirementsEditor";
import { Page } from "../components/Page";
import { Button } from "../components/ui/button";
import { ChatShell } from "../components/chat/ChatShell";
import { QuickReplies, type QuickReply } from "../components/chat/QuickReplies";
import { useUiStore } from "../state/store";
import { ScoreBadge } from "../components/ui/score-badge";
import { ArrowRight } from "lucide-react";
import { useChatSession } from "./useChatSession";

type Project = { id: string; name: string; gitlab_backlog_project_ids?: string[] };

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
    const prompt = String(formData.get("prompt") ?? "").trim();
    if (!prompt) return { ok: false };
    const prompt_id = String(formData.get("prompt_id") ?? "").trim();
    const payload = {
      project_id: params.projectId,
      prompt,
      ...(prompt_id ? { prompt_id } : {}),
    } satisfies { project_id: string; prompt: string; prompt_id?: string };

    const res = await uiFetch({
      request,
      path: "/workflow/requirements",
      init: {
        method: "POST",
        headers: { "Content-Type": "application/json" },
        body: JSON.stringify(payload),
      },
    });
    const raw: unknown = await res.json();
    if (!isRequirementsBundle(raw)) {
      throw new Response("Invalid /workflow/requirements response", { status: 502 });
    }
    const bundle = raw;
    return { bundle };
  }

  if (intent === "enhance") {
    const requirementId = String(formData.get("requirement_id") ?? "");
    const requirementType = String(formData.get("requirement_type") ?? "");
    const currentContent = parseJsonUnknown(String(formData.get("current_content") ?? "{}"), {});

    const res = await uiFetch({
      request,
      path: "/workflow/enhance",
      init: {
        method: "POST",
        headers: { "Content-Type": "application/json" },
        body: JSON.stringify({
          project_id: params.projectId,
          requirement_id: requirementId,
          requirement_type: requirementType,
          current_content: currentContent,
        }),
      },
    });
    const raw: unknown = await res.json();
    if (!isRequirement(raw)) {
      throw new Response("Invalid /workflow/enhance response", { status: 502 });
    }
    const enhanced = raw;
    return { enhanced, requirementId, requirementType };
  }

  return { ok: false };
}

export default function RequirementsRoute({ loaderData, actionData }: Route.ComponentProps) {
  const navigate = useNavigate();
  const generateFetcher = useFetcher<typeof action>();
  const enhanceFetcher = useFetcher<typeof action>();
  const [enhancingRequirementId, setEnhancingRequirementId] = useState<string | null>(null);

  const draft = useUiStore((s) => s.requirementsDraft);
  const setDraft = useUiStore((s) => s.setRequirementsDraft);
  const activePromptId = useUiStore((s) => s.activePromptId);
  const thinkingByPromptId = useUiStore((s) => s.thinkingByPromptId);

  const [bundle, setBundle] = useState<RequirementsBundle | null>(() => {
    if (actionData && typeof actionData === "object" && "bundle" in actionData) {
      return (actionData as { bundle?: RequirementsBundle }).bundle ?? null;
    }
    return null;
  });

  // merge enhance results into the currently displayed bundle
  const enhanceResult: { enhanced: Requirement; requirementId: string; requirementType: string } | null =
    actionData && typeof actionData === "object" && "enhanced" in actionData
      ? (actionData as { enhanced: Requirement; requirementId: string; requirementType: string })
      : null;

  const mergedBundle = useMemo(() => {
    if (!bundle) return null;
    if (!enhanceResult?.enhanced) return bundle;

    const apply = (list: Requirement[]) =>
      list.map((r) => (r.id === enhanceResult.requirementId ? enhanceResult.enhanced : r));

    return {
      ...bundle,
      business_requirements:
        enhanceResult.requirementType === "business"
          ? apply(bundle.business_requirements)
          : bundle.business_requirements,
      functional_requirements:
        enhanceResult.requirementType === "functional"
          ? apply(bundle.functional_requirements)
          : bundle.functional_requirements,
    };
  }, [bundle, enhanceResult]);

  const activeBundle = mergedBundle;

  const isSending = generateFetcher.state !== "idle";

  const quickReplies = useMemo<QuickReply[]>(() => {
    const qs = activeBundle?.clarification_questions ?? [];
    if (!qs || !qs.length) return [];

    const sorted = [...qs].sort((a, b) => (a.priority ?? 999) - (b.priority ?? 999));
    const items: QuickReply[] = [];
    for (const q of sorted) {
      if (q.options && q.options.length) {
        for (const opt of q.options) {
          items.push({
            id: `${q.id}:${opt}`,
            label: opt,
            value: `${q.text}\nAnswer: ${opt}`,
          });
        }
      } else {
        items.push({
          id: `${q.id}:answer`,
          label: q.text,
          value: `${q.text}\nAnswer: `,
        });
      }
    }
    return items.slice(0, 12);
  }, [activeBundle?.clarification_questions]);

  const { messages, setMessages, send } = useChatSession({
    initialSystemMessage: "Chat with the agent to generate requirements. Results appear on the right.",
    draft,
    setDraft,
    generateFetcher,
    buildSubmitFormData: (text, promptId) => {
      const fd = new FormData();
      fd.set("intent", "generate");
      fd.set("prompt", text);
      if (promptId) fd.set("prompt_id", promptId);
      return fd;
    },
    extractResult: (data) =>
      data && typeof data === "object" && "bundle" in data
        ? ((data as { bundle?: RequirementsBundle }).bundle ?? null)
        : null,
    getResultPromptId: (b) => b.prompt_id ?? null,
    onResetForNewSend: () => setBundle(null),
    onResult: ({ result: next, appendText }) => {
      setBundle(next);
      const businessCount = next.business_requirements?.length ?? 0;
      const functionalCount = next.functional_requirements?.length ?? 0;
      const scorePercent = Math.round((next.score ?? 0) * 100);

      appendText({
        kind: "text",
        role: "assistant",
        meta: "AI Agent",
        content: `✓ Generated **${businessCount + functionalCount}** requirement(s) (${businessCount} business, ${functionalCount} functional)\n\nScore: **${scorePercent}%**\n\nReview and edit on the right.`,
      });

      if (next.clarification_questions?.length) {
        appendText({
          kind: "text",
          role: "assistant",
          meta: "AI Agent",
          content: renderClarificationQuestionsMd(next.clarification_questions),
        });
      }
    },
    activePromptId,
    thinkingByPromptId,
  });

  useEffect(() => {
    const data = enhanceFetcher.data;
    if (data && typeof data === "object" && "enhanced" in data) setEnhancingRequirementId(null);
  }, [enhanceFetcher.data]);

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
        quickReplies={
          <QuickReplies
            title={quickReplies.length ? "Quick answers" : undefined}
            items={quickReplies}
            onPick={(v) => setDraft(v)}
          />
        }
        rightPanel={
          activeBundle ? (
            <div className="flex min-h-0 flex-1 flex-col gap-3 overflow-hidden">
              <div className="flex items-center justify-between gap-2 shrink-0">
                <ScoreBadge score={activeBundle.score} />
                <Button
                  size="sm"
                  onClick={() =>
                    navigate(`/projects/${loaderData.projectId}/tasks?from=requirements`, {
                      state: { requirementsBundle: activeBundle },
                    })
                  }
                >
                  Confirm tasks <ArrowRight className="h-4 w-4" />
                </Button>
              </div>
              <RequirementsEditor
                className="min-h-0 flex-1"
                bundle={activeBundle}
                onChange={(next) => setBundle(next)}
                enhancingRequirementId={enhancingRequirementId}
                onEnhance={({ requirementId, requirementType, currentContent }) => {
                  setEnhancingRequirementId(requirementId);
                  const f = new FormData();
                  f.set("intent", "enhance");
                  f.set("requirement_id", requirementId);
                  f.set("requirement_type", requirementType);
                  f.set("current_content", JSON.stringify(currentContent));
                  enhanceFetcher.submit(f, { method: "post" });
                }}
              />
            </div>
          ) : (
            <div className="text-sm text-muted-foreground">
              Generate requirements to review the bundle here.
            </div>
          )
        }
      />
    </Page>
  );
}

function renderClarificationQuestionsMd(questions: ClarificationQuestion[]) {
  const sorted = [...questions].sort((a, b) => (a.priority ?? 999) - (b.priority ?? 999));
  const lines = sorted.map((q, idx) => {
    const opts = q.options?.length ? `\n   - Options: ${q.options.join(" · ")}` : "";
    return `${idx + 1}. ${q.text}${opts}`;
  });
  return `I need a bit more detail to improve quality:\n\n${lines.join("\n")}`;
}

