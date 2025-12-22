CREATE CONSTRAINT repo_unique IF NOT EXISTS
FOR (r:__Repo__)
REQUIRE (r.project_id, r.repo_fingerprint) IS UNIQUE
