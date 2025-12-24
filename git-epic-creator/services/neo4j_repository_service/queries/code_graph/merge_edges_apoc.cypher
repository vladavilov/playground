UNWIND $rows AS row
WITH row, toUpper(row.rel_type) AS rel_type

// Source is always a __CodeNode__.
MATCH (src:__CodeNode__ {project_id: row.project_id, repo_fingerprint: row.repo_fingerprint, node_id: row.src_node_id})

// Destination depends on relationship type:
// - INCLUDES: dst is a __File__ (dst_node_id stores file_path)
// - everything else: dst is a __CodeNode__ (dst_node_id stores node_id)
CALL {
  WITH row, rel_type
  WITH row, rel_type WHERE rel_type = "INCLUDES"
  MATCH (dst:__File__ {project_id: row.project_id, repo_fingerprint: row.repo_fingerprint, file_path: row.dst_node_id})
  RETURN dst
  UNION
  WITH row, rel_type
  WITH row, rel_type WHERE rel_type <> "INCLUDES"
  MATCH (dst:__CodeNode__ {project_id: row.project_id, repo_fingerprint: row.repo_fingerprint, node_id: row.dst_node_id})
  RETURN dst
}

// Neo4j does not allow parametrizing relationship types; APOC is required for dynamic type.
CALL apoc.merge.relationship(
  src,
  rel_type,
  {project_id: row.project_id, repo_fingerprint: row.repo_fingerprint},
  {},
  dst
) YIELD rel

SET rel.confidence = row.confidence,
    rel.metadata = row.metadata

RETURN rel_type AS t, count(rel) AS n


