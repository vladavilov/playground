export const projectsFixture = [
  {
    id: "11111111-1111-1111-1111-111111111111",
    name: "Mock Project",
    description: "Mock project description",
    gitlab_repository_urls: ["https://gitlab.example.com/group/test.git"],
    gitlab_backlog_project_urls: [
      "https://gitlab.example.com/group/backlog-100",
      "https://gitlab.example.com/group/backlog-200",
    ],
    status: "active",
    created_at: "2025-01-01T00:00:00Z",
    updated_at: "2025-01-01T00:00:00Z",
    gitlab_backlog_project_ids: ["100", "200"],
  },
];

export const projectFixture = projectsFixture[0]!;


