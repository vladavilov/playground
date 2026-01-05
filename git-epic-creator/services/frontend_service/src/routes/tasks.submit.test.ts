import { describe, expect, it } from "vitest";

import { buildApplyBacklogRequest } from "./tasks.submit";

describe("buildApplyBacklogRequest", () => {
  it("groups by target project and sets parent_epic_index for issues", () => {
    const req = buildApplyBacklogRequest({
      internalProjectId: "internal-1",
      promptId: "prompt-1",
      defaultGitlabProjectId: "100",
      epics: [
        {
          id: "e1",
          title: "E1",
          description: "ed",
          target_project_id: "200",
          tasks: [
            {
              id: "t1",
              title: "T1",
              description: "td",
              acceptance_criteria: [],
              dependencies: [],
              similar: [{ kind: "issue", id: "x", iid: "7", title: "S", project_id: "200", link_decision: "accepted" }],
              target_project_id: "300",
            },
          ],
          similar: [{ kind: "epic", id: "y", iid: "9", title: "S", project_id: "200", link_decision: "accepted" }],
        },
      ],
    });

    expect(req.projects).toHaveLength(2);
    const p200 = req.projects.find((p) => p.project_id === "200")!;
    const p300 = req.projects.find((p) => p.project_id === "300")!;
    expect(p200.epics[0].related_to_iids).toEqual(["9"]);
    expect(p300.issues[0].parent_epic_index).toBe(0);
    expect(p300.issues[0].related_to_iids).toEqual(["7"]);
  });
});



