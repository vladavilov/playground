import { describe, expect, it } from "vitest";

import {
  deleteRequirementFromBundle,
  joinAcceptanceCriteria,
  splitAcceptanceCriteria,
  updateRequirement,
} from "./requirements.edit";

describe("requirements editor helpers", () => {
  it("splits/join acceptance criteria lines", () => {
    expect(splitAcceptanceCriteria(" a \n\nb\r\n  ")).toEqual(["a", "b"]);
    expect(joinAcceptanceCriteria([" a ", "", "b"])).toBe("a\nb");
  });

  it("updates requirement immutably", () => {
    const r = {
      id: "1",
      title: "t",
      description: "d",
      acceptance_criteria: ["a"],
      priority: "Must",
    };
    const next = updateRequirement(r, { title: "t2" });
    expect(next.title).toBe("t2");
    expect(r.title).toBe("t");
  });

  it("deletes a requirement from its typed list", () => {
    const bundle = {
      prompt_id: "pr",
      project_id: "p",
      score: 1,
      assumptions: [],
      risks: [],
      business_requirements: [
        { id: "b1", title: "BR1", description: "bd", acceptance_criteria: [], priority: "Must" },
        { id: "b2", title: "BR2", description: "bd2", acceptance_criteria: [], priority: "Must" },
      ],
      functional_requirements: [
        { id: "f1", title: "FR1", description: "fd", acceptance_criteria: [], priority: "Must" },
      ],
    };

    const next = deleteRequirementFromBundle(bundle, "business", "b1");
    expect(next.business_requirements.map((r) => r.id)).toEqual(["b2"]);
    expect(next.functional_requirements.map((r) => r.id)).toEqual(["f1"]);
    expect(bundle.business_requirements.map((r) => r.id)).toEqual(["b1", "b2"]);
  });
});



