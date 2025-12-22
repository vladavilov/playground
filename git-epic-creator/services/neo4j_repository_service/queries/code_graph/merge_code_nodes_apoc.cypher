UNWIND $rows AS row
MERGE (n:__CodeNode__ { project_id: row.project_id, repo_fingerprint: row.repo_fingerprint, node_id: row.node_id })
SET n += row.props
WITH n, row
MERGE (f:__File__ {project_id: row.project_id, repo_fingerprint: row.repo_fingerprint, file_path: row.props.file_path})
MERGE (f)-[:CONTAINS {project_id:row.project_id, repo_fingerprint:row.repo_fingerprint, confidence:1.0, metadata:{}}]->(n)
MERGE (p:__Project__ {id: row.project_id})
MERGE (n)-[:IN_PROJECT {project_id:row.project_id, repo_fingerprint:row.repo_fingerprint, confidence:1.0, metadata:{}}]->(p)
CALL apoc.create.addLabels(n, row.extra_labels) YIELD node
RETURN count(*) AS n
