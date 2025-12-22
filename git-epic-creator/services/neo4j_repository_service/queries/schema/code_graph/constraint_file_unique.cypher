CREATE CONSTRAINT file_unique IF NOT EXISTS
FOR (f:__File__)
REQUIRE (f.project_id, f.repo_fingerprint, f.file_path) IS UNIQUE
