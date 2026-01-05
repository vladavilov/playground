import type { BacklogEpic, BacklogTask, GeneratedBacklogBundle, SimilarMatch } from "../../routes/tasks.types";

export type LinkDecision = "accepted" | "rejected" | null;

function updateMatch(match: SimilarMatch, decision: LinkDecision): SimilarMatch {
  return { ...match, link_decision: decision };
}

export function setEpicTargetProject(bundle: GeneratedBacklogBundle, epicId: string, targetProjectId: string | null) {
  return {
    ...bundle,
    epics: bundle.epics.map((e) =>
      e.id === epicId ? { ...e, target_project_id: targetProjectId } : e,
    ),
  };
}

export function setTaskTargetProject(
  bundle: GeneratedBacklogBundle,
  taskId: string,
  targetProjectId: string | null,
) {
  return {
    ...bundle,
    epics: bundle.epics.map((e) => ({
      ...e,
      tasks: e.tasks.map((t) => (t.id === taskId ? { ...t, target_project_id: targetProjectId } : t)),
    })),
  };
}

export function setEpicSimilarDecision(
  bundle: GeneratedBacklogBundle,
  epicId: string,
  matchId: string,
  decision: LinkDecision,
) {
  return {
    ...bundle,
    epics: bundle.epics.map((e) =>
      e.id === epicId
        ? {
            ...e,
            similar: (e.similar ?? []).map((m) => (m.id === matchId ? updateMatch(m, decision) : m)),
          }
        : e,
    ),
  };
}

export function setTaskSimilarDecision(
  bundle: GeneratedBacklogBundle,
  taskId: string,
  matchId: string,
  decision: LinkDecision,
) {
  return {
    ...bundle,
    epics: bundle.epics.map((e) => ({
      ...e,
      tasks: e.tasks.map((t) =>
        t.id === taskId
          ? {
              ...t,
              similar: (t.similar ?? []).map((m) => (m.id === matchId ? updateMatch(m, decision) : m)),
            }
          : t,
      ),
    })),
  };
}

export function applyEnhancedToBundle(bundle: GeneratedBacklogBundle, enhanced: { item_id: string; title: string; description: string; acceptance_criteria: string[]; dependencies?: string[] | null }) {
  return {
    ...bundle,
    epics: bundle.epics.map((e) => {
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
    }),
  };
}

export function deleteEpicFromBundle(bundle: GeneratedBacklogBundle, epicId: string): GeneratedBacklogBundle {
  return { ...bundle, epics: bundle.epics.filter((e) => e.id !== epicId) };
}

export function deleteTaskFromBundle(bundle: GeneratedBacklogBundle, taskId: string): GeneratedBacklogBundle {
  return {
    ...bundle,
    epics: bundle.epics.map((e) => ({
      ...e,
      tasks: e.tasks.filter((t) => t.id !== taskId),
    })),
  };
}



