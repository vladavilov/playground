CREATE CONSTRAINT codenode_unique IF NOT EXISTS
FOR (n:__CodeNode__)
REQUIRE (n.project_id, n.repo_fingerprint, n.node_id) IS UNIQUE
