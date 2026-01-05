export function buildCacheEmbeddingsPath(args: {
  projectId: string;
  gitlabProjectIds: string[];
}) {
  const ids = args.gitlabProjectIds.map((x) => String(x).trim()).filter(Boolean);
  const qp = new URLSearchParams({
    project_id: args.projectId,
    gitlab_project_ids: ids.join(","),
  });
  return `/project/projects/multi/cache-embeddings?${qp.toString()}`;
}

export function parseBacklogUrls(text: string) {
  return text
    .split(/\r?\n/)
    .map((l) => l.trim())
    .filter(Boolean);
}



