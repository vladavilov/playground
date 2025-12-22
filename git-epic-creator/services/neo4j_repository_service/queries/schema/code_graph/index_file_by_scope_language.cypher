CREATE INDEX file_by_scope_language IF NOT EXISTS
FOR (f:__File__)
ON (f.project_id, f.repo_fingerprint, f.language)
