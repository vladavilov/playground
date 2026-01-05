import { describe, expect, it } from "vitest";

import { deleteEpicFromBundle, deleteTaskFromBundle, setEpicSimilarDecision, setTaskSimilarDecision, setEpicTargetProject, setTaskTargetProject } from "./backlog.edit";

describe("backlog editor helpers", () => {
  it("deletes an epic from the bundle", () => {
    const bundle = {
      prompt_id: "pr",
      project_id: "p",
      score: 1,
      assumptions: [],
      risks: [],
      epics: [
        {
          id: "e1",
          title: "E1",
          description: "d",
          tasks: [
            { id: "t1", title: "T1", description: "td", acceptance_criteria: [], dependencies: [] },
          ],
        },
        { id: "e2", title: "E2", description: "d2", tasks: [] },
      ],
    };

    const next = deleteEpicFromBundle(bundle, "e1");
    expect(next.epics.map((e) => e.id)).toEqual(["e2"]);
    expect(bundle.epics.map((e) => e.id)).toEqual(["e1", "e2"]);
  });

  it("deletes a task from whichever epic contains it", () => {
    const bundle = {
      prompt_id: "pr",
      project_id: "p",
      score: 1,
      assumptions: [],
      risks: [],
      epics: [
        {
          id: "e1",
          title: "E1",
          description: "d",
          tasks: [
            { id: "t1", title: "T1", description: "td", acceptance_criteria: [], dependencies: [] },
            { id: "t2", title: "T2", description: "td2", acceptance_criteria: [], dependencies: [] },
          ],
        },
        { id: "e2", title: "E2", description: "d2", tasks: [] },
      ],
    };

    const next = deleteTaskFromBundle(bundle, "t1");
    expect(next.epics.find((e) => e.id === "e1")?.tasks.map((t) => t.id)).toEqual(["t2"]);
    expect(bundle.epics.find((e) => e.id === "e1")?.tasks.map((t) => t.id)).toEqual(["t1", "t2"]);
  });
});

describe("backlog edit helpers", () => {
  const base: any = {
    prompt_id: "pr",
    project_id: "p",
    score: 1,
    assumptions: [],
    risks: [],
    epics: [
      {
        id: "e1",
        title: "E1",
        description: "d",
        tasks: [
          {
            id: "t1",
            title: "T1",
            description: "d",
            acceptance_criteria: [],
            dependencies: [],
            similar: [{ kind: "issue", id: "m1", iid: "1", title: "s", project_id: "100" }],
          },
        ],
        similar: [{ kind: "epic", id: "m2", iid: "2", title: "s", project_id: "100" }],
      },
    ],
  };

  it("sets target project id on epic and task", () => {
    expect(setEpicTargetProject(base, "e1", "200").epics[0]!.target_project_id).toBe("200");
    expect(setTaskTargetProject(base, "t1", "300").epics[0]!.tasks[0]!.target_project_id).toBe("300");
  });

  it("sets link decision and supports multi-accept (does not clear others)", () => {
    const next = setEpicSimilarDecision(base, "e1", "m2", "accepted");
    expect(next.epics[0]!.similar![0]!.link_decision).toBe("accepted");

    const next2 = setTaskSimilarDecision(base, "t1", "m1", "rejected");
    expect(next2.epics[0]!.tasks[0]!.similar![0]!.link_decision).toBe("rejected");
  });
});


