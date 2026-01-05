import type { BacklogEpic, SimilarMatch } from "./tasks.types";

type ApplyBacklogWorkItem = {
  title: string;
  description: string;
  labels: string[];
  target_project_id?: string;
  related_to_iids: string[];
};

type ApplyBacklogIssue = ApplyBacklogWorkItem & { parent_epic_index: number };

type BatchApplyBacklogProjectPayload = {
  project_id: string;
  epics: ApplyBacklogWorkItem[];
  issues: ApplyBacklogIssue[];
};

export type BatchApplyBacklogRequest = {
  prompt_id: string;
  internal_project_id: string;
  projects: BatchApplyBacklogProjectPayload[];
};

function acceptedIids(similar: SimilarMatch[] | null | undefined) {
  return (similar ?? [])
    .filter((s) => s.link_decision === "accepted")
    .map((s) => String(s.iid ?? s.id));
}

export function buildApplyBacklogRequest(args: {
  internalProjectId: string;
  promptId: string;
  defaultGitlabProjectId: string;
  epics: BacklogEpic[];
}): BatchApplyBacklogRequest {
  const byProject = new Map<string, BatchApplyBacklogProjectPayload>();

  const getBucket = (projectId: string) => {
    const key = projectId || args.defaultGitlabProjectId;
    const existing = byProject.get(key);
    if (existing) return existing;
    const created: BatchApplyBacklogProjectPayload = { project_id: key, epics: [], issues: [] };
    byProject.set(key, created);
    return created;
  };

  args.epics.forEach((epic, epicIdx) => {
    const epicTarget = epic.target_project_id || args.defaultGitlabProjectId;
    getBucket(epicTarget).epics.push({
      title: epic.title,
      description: epic.description ?? "",
      labels: [],
      target_project_id: epicTarget,
      related_to_iids: acceptedIids(epic.similar),
    });

    epic.tasks.forEach((task) => {
      const taskTarget = task.target_project_id || epicTarget;
      getBucket(taskTarget).issues.push({
        title: task.title,
        description: task.description ?? "",
        labels: [],
        target_project_id: taskTarget,
        related_to_iids: acceptedIids(task.similar),
        parent_epic_index: epicIdx,
      });
    });
  });

  return {
    prompt_id: args.promptId,
    internal_project_id: args.internalProjectId,
    projects: Array.from(byProject.values()),
  };
}



