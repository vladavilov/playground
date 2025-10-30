// Validate embedding presence and dimensions across all node types
// Returns counts of nodes with/without embeddings for data quality monitoring
MATCH (p:__Project__ {id: $project_id})

// Chunks
OPTIONAL MATCH (ch:__Chunk__)-[:IN_PROJECT]->(p)
WITH p, 
     count(ch) AS total_chunks,
     count(CASE WHEN ch.embedding IS NOT NULL AND size(ch.embedding) = 3072 THEN 1 END) AS chunks_with_embedding

// Entities
OPTIONAL MATCH (e:__Entity__)-[:IN_PROJECT]->(p)
WITH p, total_chunks, chunks_with_embedding,
     count(e) AS total_entities,
     count(CASE WHEN e.embedding IS NOT NULL AND size(e.embedding) = 3072 THEN 1 END) AS entities_with_embedding

// Communities
OPTIONAL MATCH (c:__Community__)-[:IN_PROJECT]->(p)

RETURN 
  p.id AS project_id,
  total_chunks,
  chunks_with_embedding,
  total_chunks - chunks_with_embedding AS chunks_missing_embedding,
  total_entities,
  entities_with_embedding,
  total_entities - entities_with_embedding AS entities_missing_embedding,
  count(c) AS total_communities,
  count(CASE WHEN c.embedding IS NOT NULL AND size(c.embedding) = 3072 THEN 1 END) AS communities_with_embedding,
  count(c) - count(CASE WHEN c.embedding IS NOT NULL AND size(c.embedding) = 3072 THEN 1 END) AS communities_missing_embedding,
  // Overall health metrics
  CASE 
    WHEN count(c) = 0 THEN 0.0  // No communities = critical
    ELSE toFloat(count(CASE WHEN c.embedding IS NOT NULL AND size(c.embedding) = 3072 THEN 1 END)) / count(c)
  END AS community_embedding_coverage

