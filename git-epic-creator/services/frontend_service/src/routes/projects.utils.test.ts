import { describe, expect, it } from "vitest";

import { buildCacheEmbeddingsPath, parseBacklogUrls } from "./projects.utils";

describe("projects utils", () => {
  it("builds cache embeddings query path", () => {
    expect(
      buildCacheEmbeddingsPath({
        projectId: "p",
        gitlabProjectIds: ["1", "2"],
      }),
    ).toBe(
      "/project/projects/multi/cache-embeddings?project_id=p&gitlab_project_ids=1%2C2",
    );
  });

  it("parses backlog urls from textarea", () => {
    expect(parseBacklogUrls(" a \n\nb\r\n  ")).toEqual(["a", "b"]);
  });
});



