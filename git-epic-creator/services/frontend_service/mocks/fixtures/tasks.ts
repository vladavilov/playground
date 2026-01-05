export const backlogBundleFixture = {
  prompt_id: "bbbbbbbb-bbbb-bbbb-bbbb-bbbbbbbbbbbb",
  project_id: "11111111-1111-1111-1111-111111111111",
  epics: [
    {
      id: "epic-1",
      title: "Epic 1",
      description: "Mock epic description",
      tasks: [
        {
          id: "task-1",
          title: "Task 1",
          description: "Mock task description",
          acceptance_criteria: ["AC1"],
          dependencies: [],
          similar: [
            { kind: "issue", id: "gl-1", iid: "7", title: "Existing issue", project_id: "100", link_decision: null },
          ],
        },
      ],
      similar: [
        { kind: "epic", id: "gl-e1", iid: "9", title: "Existing epic", project_id: "100", link_decision: null },
      ],
    },
  ],
  assumptions: [],
  risks: [],
  score: 0.85,
  markdown_text: null,
};

export const enhancedTaskFixture = {
  item_id: "task-1",
  title: "Task 1 (enhanced)",
  description: "Enhanced description",
  acceptance_criteria: ["AC1", "AC2"],
  dependencies: [],
};


