import { describe, expect, it } from "vitest";

import { requirementsBundleToPrompt } from "./requirements.handoff";

describe("requirementsBundleToPrompt", () => {
  it("includes BR/FR titles and descriptions", () => {
    const text = requirementsBundleToPrompt({
      prompt_id: "pr",
      project_id: "p",
      score: 1,
      assumptions: [],
      risks: [],
      business_requirements: [
        {
          id: "b1",
          title: "BR1",
          description: "bd",
          acceptance_criteria: [],
          priority: "Must",
        },
      ],
      functional_requirements: [
        {
          id: "f1",
          title: "FR1",
          description: "fd",
          acceptance_criteria: [],
          priority: "Must",
        },
      ],
    });

    expect(text).toContain("Business requirements:");
    expect(text).toContain("BR: BR1");
    expect(text).toContain("Functional requirements:");
    expect(text).toContain("FR: FR1");
  });
});



