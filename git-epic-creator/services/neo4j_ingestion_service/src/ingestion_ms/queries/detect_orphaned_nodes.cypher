// Detection query to count orphaned nodes by type
// Parameters: $project_id (string)
// Returns counts of nodes that have lost their required relationships

MATCH (p:__Project__ {id: $project_id})

// Count orphaned __Chunk__ nodes (no HAS_CHUNK relationship from any document)
CALL (p) {
  MATCH (ch:__Chunk__)-[:IN_PROJECT]->(p)
  WHERE NOT exists(()-[:HAS_CHUNK]->(ch))
  RETURN count(ch) AS orphaned_chunks
}

// Count orphaned __Entity__ nodes (no HAS_ENTITY relationship from any chunk)
CALL (p) {
  MATCH (e:__Entity__)-[:IN_PROJECT]->(p)
  WHERE NOT exists(()-[:HAS_ENTITY]->(e))
  RETURN count(e) AS orphaned_entities
}

// Count orphaned __Community__ nodes (no entities AND no chunks)
// Communities without parent communities are valid (top-level), so check for content instead
CALL (p) {
  MATCH (c:__Community__)-[:IN_PROJECT]->(p)
  WHERE NOT exists((:__Entity__)-[:IN_COMMUNITY]->(c))
    AND NOT exists((:__Chunk__)-[:IN_COMMUNITY]->(c))
  RETURN count(c) AS orphaned_communities
}

// Count nodes without IN_PROJECT relationship (use UNION for performance)
CALL () {
  MATCH (n:__Document__)
  WHERE NOT exists((n)-[:IN_PROJECT]->(:__Project__))
  RETURN count(n) AS cnt
  UNION ALL
  MATCH (n:__Chunk__)
  WHERE NOT exists((n)-[:IN_PROJECT]->(:__Project__))
  RETURN count(n) AS cnt
  UNION ALL
  MATCH (n:__Entity__)
  WHERE NOT exists((n)-[:IN_PROJECT]->(:__Project__))
  RETURN count(n) AS cnt
  UNION ALL
  MATCH (n:__Community__)
  WHERE NOT exists((n)-[:IN_PROJECT]->(:__Project__))
  RETURN count(n) AS cnt
}
WITH p, orphaned_chunks, orphaned_entities, orphaned_communities, sum(cnt) AS unlinked_nodes

// Count total nodes for this project for context
CALL (p) {
  OPTIONAL MATCH (doc:__Document__)-[:IN_PROJECT]->(p)
  RETURN count(doc) AS total_documents
}

CALL (p) {
  OPTIONAL MATCH (ch:__Chunk__)-[:IN_PROJECT]->(p)
  RETURN count(ch) AS total_chunks
}

CALL (p) {
  OPTIONAL MATCH (e:__Entity__)-[:IN_PROJECT]->(p)
  RETURN count(e) AS total_entities
}

CALL (p) {
  OPTIONAL MATCH (c:__Community__)-[:IN_PROJECT]->(p)
  RETURN count(c) AS total_communities
}

RETURN 
  p.id AS project_id,
  orphaned_chunks,
  orphaned_entities,
  orphaned_communities,
  unlinked_nodes,
  orphaned_chunks + orphaned_entities + orphaned_communities + unlinked_nodes AS total_orphaned,
  total_documents,
  total_chunks,
  total_entities,
  total_communities,
  CASE 
    WHEN total_chunks > 0 THEN toFloat(orphaned_chunks) / total_chunks * 100.0
    ELSE 0.0 
  END AS orphaned_chunks_percentage,
  CASE 
    WHEN total_entities > 0 THEN toFloat(orphaned_entities) / total_entities * 100.0
    ELSE 0.0 
  END AS orphaned_entities_percentage

