UNWIND $rows AS row
MATCH (src:__CodeNode__ {project_id: row.project_id, repo_fingerprint: row.repo_fingerprint, node_id: row.src_node_id})
MATCH (dst:__CodeNode__ {project_id: row.project_id, repo_fingerprint: row.repo_fingerprint, node_id: row.dst_node_id})
MERGE (src)-[r:PERFORMS {project_id: row.project_id, repo_fingerprint: row.repo_fingerprint}]->(dst)
SET r.confidence = row.confidence,
    r.metadata = row.metadata
RETURN count(*) AS n
