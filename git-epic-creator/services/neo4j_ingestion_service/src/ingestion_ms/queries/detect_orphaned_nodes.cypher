// Detection query to count orphaned nodes by type
// Parameters: $project_id (string)
// Returns counts of nodes that have lost their required relationships

MATCH (p:__Project__ {id: $project_id})

// Single-pass collection of all node counts and orphan detection
OPTIONAL MATCH (doc:__Document__)-[:IN_PROJECT]->(p)
WITH p, count(doc) AS total_documents

OPTIONAL MATCH (ch:__Chunk__)-[:IN_PROJECT]->(p)
WITH p, total_documents, count(ch) AS total_chunks,
     sum(CASE WHEN NOT exists(()-[:HAS_CHUNK]->(ch)) THEN 1 ELSE 0 END) AS orphaned_chunks

OPTIONAL MATCH (e:__Entity__)-[:IN_PROJECT]->(p)
WITH p, total_documents, total_chunks, orphaned_chunks,
     count(e) AS total_entities,
     sum(CASE WHEN NOT exists(()-[:HAS_ENTITY]->(e)) 
              AND NOT exists((e)-[:RELATED]-()) 
              AND NOT exists(()-[:RELATED]->(e)) THEN 1 ELSE 0 END) AS orphaned_entities

OPTIONAL MATCH (c:__Community__)-[:IN_PROJECT]->(p)
WITH p, total_documents, total_chunks, orphaned_chunks, total_entities, orphaned_entities,
     count(c) AS total_communities,
     sum(CASE WHEN NOT exists((:__Entity__)-[:IN_COMMUNITY]->(c)) 
              AND NOT exists((:__Chunk__)-[:IN_COMMUNITY]->(c)) THEN 1 ELSE 0 END) AS orphaned_communities

// Count nodes without IN_PROJECT relationship efficiently
CALL () {
  MATCH (n)
  WHERE (n:__Document__ OR n:__Chunk__ OR n:__Entity__ OR n:__Community__)
    AND NOT exists((n)-[:IN_PROJECT]->(:__Project__))
  RETURN count(n) AS cnt
}

WITH p, orphaned_chunks, orphaned_entities, orphaned_communities, cnt AS unlinked_nodes,
     total_documents, total_chunks, total_entities, total_communities

RETURN 
  p.id AS project_id,
  coalesce(orphaned_chunks, 0) AS orphaned_chunks,
  coalesce(orphaned_entities, 0) AS orphaned_entities,
  coalesce(orphaned_communities, 0) AS orphaned_communities,
  coalesce(unlinked_nodes, 0) AS unlinked_nodes,
  coalesce(orphaned_chunks, 0) + coalesce(orphaned_entities, 0) + coalesce(orphaned_communities, 0) + coalesce(unlinked_nodes, 0) AS total_orphaned,
  total_documents,
  total_chunks,
  total_entities,
  total_communities,
  CASE 
    WHEN total_chunks > 0 THEN toFloat(coalesce(orphaned_chunks, 0)) / total_chunks * 100.0
    ELSE 0.0 
  END AS orphaned_chunks_percentage,
  CASE 
    WHEN total_entities > 0 THEN toFloat(coalesce(orphaned_entities, 0)) / total_entities * 100.0
    ELSE 0.0 
  END AS orphaned_entities_percentage

