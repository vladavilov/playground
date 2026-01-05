import type { Requirement, RequirementsBundle } from "../../routes/requirements.types";

export type RequirementType = "business" | "functional";

export function splitAcceptanceCriteria(text: string) {
  return text
    .split(/\r?\n/)
    .map((l) => l.trim())
    .filter(Boolean);
}

export function joinAcceptanceCriteria(list: string[]) {
  return list.map((x) => x.trim()).filter(Boolean).join("\n");
}

export function updateRequirement(
  req: Requirement,
  patch: Partial<Pick<Requirement, "title" | "description" | "priority">> & {
    acceptance_criteria?: string[];
  },
): Requirement {
  return {
    ...req,
    ...patch,
    acceptance_criteria: patch.acceptance_criteria ?? req.acceptance_criteria,
  };
}

export function deleteRequirementFromBundle(
  bundle: RequirementsBundle,
  requirementType: RequirementType,
  requirementId: string,
): RequirementsBundle {
  if (requirementType === "business") {
    return {
      ...bundle,
      business_requirements: bundle.business_requirements.filter((r) => r.id !== requirementId),
    };
  }

  return {
    ...bundle,
    functional_requirements: bundle.functional_requirements.filter((r) => r.id !== requirementId),
  };
}



