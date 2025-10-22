// Cleanup orphaned nodes after ingestion and deduplication
// Parameters: $project_id (string)
// Removes nodes that lost their connections during merge/dedup operations

MATCH (p:__Project__ {id: $project_id})

// Step 1: Remove orphaned __Chunk__ nodes (no HAS_CHUNK relationship from any document)
WITH p
MATCH (ch:__Chunk__)-[:IN_PROJECT]->(p)
WHERE NOT exists(()-[:HAS_CHUNK]->(ch))
WITH collect(ch) AS orphaned_chunks
CALL (orphaned_chunks) {
  UNWIND orphaned_chunks AS ch
  DETACH DELETE ch
  RETURN count(*) AS deleted
}
WITH deleted AS total_orphaned_chunks

// Step 2: Remove orphaned __Entity__ nodes (no HAS_ENTITY relationship from any chunk)
MATCH (p:__Project__ {id: $project_id})
MATCH (e:__Entity__)-[:IN_PROJECT]->(p)
WHERE NOT exists(()-[:HAS_ENTITY]->(e))
WITH total_orphaned_chunks, collect(e) AS orphaned_entities
CALL (orphaned_entities) {
  UNWIND orphaned_entities AS e
  DETACH DELETE e
  RETURN count(*) AS deleted
}
WITH total_orphaned_chunks, deleted AS total_orphaned_entities

// Step 3: Remove orphaned __Community__ nodes (no IN_COMMUNITY relationships from entities or chunks)
MATCH (p:__Project__ {id: $project_id})
MATCH (c:__Community__)-[:IN_PROJECT]->(p)
WHERE NOT exists(()-[:IN_COMMUNITY]->(c))
WITH total_orphaned_chunks, total_orphaned_entities, collect(c) AS orphaned_communities
CALL (orphaned_communities) {
  UNWIND orphaned_communities AS c
  DETACH DELETE c
  RETURN count(*) AS deleted
}
WITH total_orphaned_chunks, total_orphaned_entities, deleted AS total_orphaned_communities

// Step 4: Remove any remaining nodes without IN_PROJECT relationship
// (Safety net for nodes that lost project association during dedup)
// Use UNION for better performance than OR on labels
CALL () {
  MATCH (n:__Document__)
  WHERE NOT exists((n)-[:IN_PROJECT]->(:__Project__))
  RETURN n
  UNION
  MATCH (n:__Chunk__)
  WHERE NOT exists((n)-[:IN_PROJECT]->(:__Project__))
  RETURN n
  UNION
  MATCH (n:__Entity__)
  WHERE NOT exists((n)-[:IN_PROJECT]->(:__Project__))
  RETURN n
  UNION
  MATCH (n:__Community__)
  WHERE NOT exists((n)-[:IN_PROJECT]->(:__Project__))
  RETURN n
}
WITH total_orphaned_chunks, total_orphaned_entities, total_orphaned_communities, collect(n) AS unlinked_nodes
CALL (unlinked_nodes) {
  UNWIND unlinked_nodes AS n
  DETACH DELETE n
  RETURN count(*) AS deleted
}
WITH total_orphaned_chunks, total_orphaned_entities, total_orphaned_communities, deleted AS total_unlinked

RETURN 
  coalesce(total_orphaned_chunks, 0) AS orphaned_chunks_removed,
  coalesce(total_orphaned_entities, 0) AS orphaned_entities_removed,
  coalesce(total_orphaned_communities, 0) AS orphaned_communities_removed,
  coalesce(total_unlinked, 0) AS unlinked_nodes_removed,
  coalesce(total_orphaned_chunks, 0) + coalesce(total_orphaned_entities, 0) + 
  coalesce(total_orphaned_communities, 0) + coalesce(total_unlinked, 0) AS total_removed

