CREATE INDEX codenode_by_scope_file IF NOT EXISTS
FOR (n:__CodeNode__)
ON (n.project_id, n.repo_fingerprint, n.file_path)
