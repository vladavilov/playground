import { isRecord } from "../lib/guards";
import type { ClarificationQuestion, RequirementsBundle, Requirement } from "./requirements.types";

function isStringArray(v: unknown): v is string[] {
  return Array.isArray(v) && v.every((x) => typeof x === "string");
}

export function isRequirement(v: unknown): v is Requirement {
  if (!isRecord(v)) return false;
  if (typeof v.id !== "string") return false;
  if (typeof v.title !== "string") return false;
  if (typeof v.description !== "string") return false;
  if (!isStringArray(v.acceptance_criteria)) return false;
  if (typeof v.priority !== "string") return false;
  return true;
}

function isClarificationQuestion(v: unknown): v is ClarificationQuestion {
  if (!isRecord(v)) return false;
  if (typeof v.id !== "string") return false;
  if (typeof v.text !== "string") return false;
  if (typeof v.expected_impact !== "string") return false;
  if ("options" in v && v.options != null) {
    if (!Array.isArray(v.options) || !v.options.every((x) => typeof x === "string")) return false;
  }
  return true;
}

export function isRequirementsBundle(v: unknown): v is RequirementsBundle {
  if (!isRecord(v)) return false;
  if (typeof v.prompt_id !== "string") return false;
  if (typeof v.project_id !== "string") return false;
  if (!Array.isArray(v.business_requirements) || !v.business_requirements.every(isRequirement)) return false;
  if (!Array.isArray(v.functional_requirements) || !v.functional_requirements.every(isRequirement)) return false;
  if (!isStringArray(v.assumptions)) return false;
  if (!isStringArray(v.risks)) return false;
  if (typeof v.score !== "number" || !Number.isFinite(v.score)) return false;
  if ("clarification_questions" in v && v.clarification_questions != null) {
    if (!Array.isArray(v.clarification_questions) || !v.clarification_questions.every(isClarificationQuestion))
      return false;
  }
  return true;
}


