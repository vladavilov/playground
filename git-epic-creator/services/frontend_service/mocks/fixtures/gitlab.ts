export const applyBacklogResponseFixture = {
  project_results: [
    {
      project_id: "100",
      success: true,
      results: {
        epics: [{ input_index: 0, action: "created", id: "1", web_url: "https://gitlab/epic/1" }],
        issues: [{ input_index: 0, action: "created", id: "2", web_url: "https://gitlab/issue/2" }],
      },
      errors: [],
      error_message: null,
    },
  ],
  total_epics_created: 1,
  total_issues_created: 1,
  total_errors: 0,
  projects_succeeded: 1,
  projects_failed: 0,
};


