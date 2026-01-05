import { isRecord } from "../lib/guards";
import type { BacklogEpic, BacklogTask, EnhancedTask, GeneratedBacklogBundle, SimilarMatch } from "./tasks.types";

function isStringArray(v: unknown): v is string[] {
  return Array.isArray(v) && v.every((x) => typeof x === "string");
}

function isSimilarMatch(v: unknown): v is SimilarMatch {
  if (!isRecord(v)) return false;
  if (v.kind !== "epic" && v.kind !== "issue") return false;
  if (typeof v.id !== "string") return false;
  if (typeof v.iid !== "string") return false;
  if (typeof v.title !== "string") return false;
  if (typeof v.project_id !== "string") return false;
  return true;
}

function isBacklogTask(v: unknown): v is BacklogTask {
  if (!isRecord(v)) return false;
  if (typeof v.id !== "string") return false;
  if (typeof v.title !== "string") return false;
  if (typeof v.description !== "string") return false;
  if (!isStringArray(v.acceptance_criteria)) return false;
  if (!isStringArray(v.dependencies)) return false;
  if ("similar" in v && v.similar != null) {
    if (!Array.isArray(v.similar) || !v.similar.every(isSimilarMatch)) return false;
  }
  return true;
}

export function isBacklogEpic(v: unknown): v is BacklogEpic {
  if (!isRecord(v)) return false;
  if (typeof v.id !== "string") return false;
  if (typeof v.title !== "string") return false;
  if (typeof v.description !== "string") return false;
  if (!Array.isArray(v.tasks) || !v.tasks.every(isBacklogTask)) return false;
  if ("similar" in v && v.similar != null) {
    if (!Array.isArray(v.similar) || !v.similar.every(isSimilarMatch)) return false;
  }
  return true;
}

export function isGeneratedBacklogBundle(v: unknown): v is GeneratedBacklogBundle {
  if (!isRecord(v)) return false;
  if (typeof v.prompt_id !== "string") return false;
  if (typeof v.project_id !== "string") return false;
  if (!Array.isArray(v.epics) || !v.epics.every(isBacklogEpic)) return false;
  if (!isStringArray(v.assumptions)) return false;
  if (!isStringArray(v.risks)) return false;
  if (typeof v.score !== "number" || !Number.isFinite(v.score)) return false;
  if ("markdown_text" in v && v.markdown_text != null && typeof v.markdown_text !== "string") return false;
  return true;
}

export function isEnhancedTask(v: unknown): v is EnhancedTask {
  if (!isRecord(v)) return false;
  if (typeof v.item_id !== "string") return false;
  if (typeof v.title !== "string") return false;
  if (typeof v.description !== "string") return false;
  if (!isStringArray(v.acceptance_criteria)) return false;
  if ("dependencies" in v && v.dependencies != null) {
    if (!isStringArray(v.dependencies)) return false;
  }
  return true;
}


