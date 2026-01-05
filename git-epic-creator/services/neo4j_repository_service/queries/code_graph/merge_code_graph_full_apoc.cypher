// Merge the full code graph in one query (single Neo4j transaction):
// - __Project__ + __Repo__ + scoping relationships
// - __File__ rows + repo/file + file/project relationships
// - __CodeNode__ rows + file/node + node/project relationships + extra labels
// - all code relationships (dynamic relationship type via APOC), including INCLUDES->__File__

MERGE (p:__Project__ {id: $project_id})
MERGE (r:__Repo__ {project_id: $project_id, repo_fingerprint: $repo_fingerprint})
MERGE (r)-[:IN_PROJECT {project_id:$project_id, repo_fingerprint:$repo_fingerprint, confidence:1.0}]->(p)

WITH p, r
CALL {
  WITH p, r
  WITH $files AS files
  WITH p, r, files WHERE size(files) = 0
  RETURN 0 AS files_processed
  UNION
  WITH p, r
  WITH $files AS files
  WITH p, r, files WHERE size(files) > 0
  UNWIND files AS row
  MERGE (f:__File__ {project_id: $project_id, repo_fingerprint: $repo_fingerprint, file_path: row.file_path})
  SET f.sha256 = row.sha256,
      f.language = row.language,
      f.line_count = row.line_count
  MERGE (r)-[:HAS_FILE {project_id:$project_id, repo_fingerprint:$repo_fingerprint, confidence:1.0}]->(f)
  MERGE (f)-[:IN_PROJECT {project_id:$project_id, repo_fingerprint:$repo_fingerprint, confidence:1.0}]->(p)
  RETURN count(*) AS files_processed
}

WITH p, r, files_processed
CALL {
  WITH p, r
  WITH $nodes AS nodes
  WITH p, r, nodes WHERE size(nodes) = 0
  RETURN 0 AS nodes_processed
  UNION
  WITH p, r
  WITH $nodes AS nodes
  WITH p, r, nodes WHERE size(nodes) > 0
  UNWIND nodes AS row
  MERGE (n:__CodeNode__ { project_id: $project_id, repo_fingerprint: $repo_fingerprint, node_id: row.node_id })
  SET n += row.props
  MERGE (f:__File__ {project_id: $project_id, repo_fingerprint: $repo_fingerprint, file_path: row.props.file_path})
  MERGE (f)-[:CONTAINS {project_id:$project_id, repo_fingerprint:$repo_fingerprint, confidence:1.0}]->(n)
  MERGE (n)-[:IN_PROJECT {project_id:$project_id, repo_fingerprint:$repo_fingerprint, confidence:1.0}]->(p)
  CALL apoc.create.addLabels(n, row.extra_labels) YIELD node
  RETURN count(*) AS nodes_processed
}

WITH p, r, files_processed, nodes_processed
CALL {
  WITH $edges AS edges
  WITH edges WHERE size(edges) = 0
  RETURN 0 AS edges_processed_total, [] AS pairs
  UNION
  WITH $edges AS edges
  WITH edges WHERE size(edges) > 0
  UNWIND edges AS row
  WITH row, toUpper(row.rel_type) AS rel_type
  MATCH (src:__CodeNode__ {project_id: $project_id, repo_fingerprint: $repo_fingerprint, node_id: row.src_node_id})
  CALL {
    WITH row, rel_type
    WITH row, rel_type WHERE rel_type = "INCLUDES"
    MATCH (dst:__File__ {project_id: $project_id, repo_fingerprint: $repo_fingerprint, file_path: row.dst_node_id})
    RETURN dst
    UNION
    WITH row, rel_type
    WITH row, rel_type WHERE rel_type <> "INCLUDES"
    MATCH (dst:__CodeNode__ {project_id: $project_id, repo_fingerprint: $repo_fingerprint, node_id: row.dst_node_id})
    RETURN dst
  }
  CALL apoc.merge.relationship(
    src,
    rel_type,
    {project_id: $project_id, repo_fingerprint: $repo_fingerprint},
    {},
    dst
  ) YIELD rel
  SET rel.confidence = row.confidence
  WITH rel_type AS t, count(rel) AS n
  WITH collect([t, n]) AS pairs, sum(n) AS edges_processed_total
  RETURN edges_processed_total, pairs
}

RETURN
  files_processed,
  nodes_processed,
  edges_processed_total,
  apoc.map.fromPairs(pairs) AS edges_by_type


