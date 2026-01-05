import type { RequirementsBundle } from "./requirements.types";

export function requirementsBundleToPrompt(bundle: RequirementsBundle) {
  const br = bundle.business_requirements
    .map((r) => `- BR: ${r.title}\n  - ${r.description}`)
    .join("\n");
  const fr = bundle.functional_requirements
    .map((r) => `- FR: ${r.title}\n  - ${r.description}`)
    .join("\n");

  return [
    `Project ${bundle.project_id}`,
    "",
    "Business requirements:",
    br || "- (none)",
    "",
    "Functional requirements:",
    fr || "- (none)",
  ].join("\n");
}



