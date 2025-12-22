UNWIND $rows AS row
MERGE (f:__File__ {project_id: row.project_id, repo_fingerprint: row.repo_fingerprint, file_path: row.file_path})
SET f.sha256 = row.sha256,
    f.language = row.language,
    f.line_count = row.line_count
WITH f, row
MERGE (r:__Repo__ {project_id: row.project_id, repo_fingerprint: row.repo_fingerprint})
MERGE (r)-[:HAS_FILE {project_id:row.project_id, repo_fingerprint:row.repo_fingerprint, confidence:1.0, metadata:{}}]->(f)
MERGE (p:__Project__ {id: row.project_id})
MERGE (f)-[:IN_PROJECT {project_id:row.project_id, repo_fingerprint:row.repo_fingerprint, confidence:1.0, metadata:{}}]->(p)
RETURN count(*) AS n
