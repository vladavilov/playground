MERGE (r:__Repo__ {project_id: $project_id, repo_fingerprint: $repo_fingerprint})
MERGE (p:__Project__ {id: $project_id})
MERGE (r)-[:IN_PROJECT {project_id:$project_id, repo_fingerprint:$repo_fingerprint, confidence:1.0, metadata:{}}]->(p)
RETURN r.repo_fingerprint AS repo_fingerprint
