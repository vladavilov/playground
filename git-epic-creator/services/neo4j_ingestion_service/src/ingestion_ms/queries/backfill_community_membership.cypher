// Backfill community.entity_ids and IN_COMMUNITY relationships for current project
// Uses merged_ids to match entities even if their IDs were deduplicated
// SCOPED to specific project for multi-tenancy safety
// ROBUST: Handles empty entity_ids via fallback logic (existing relationships or text_unit_ids)
MATCH (p:__Project__ {id: $project_id})
MATCH (c:__Community__)-[:IN_PROJECT]->(p) 
WHERE c.project_id = $project_id
WITH c, p, coalesce(c.entity_ids, []) AS stored_entity_ids, coalesce(c.text_unit_ids, []) AS text_unit_ids

// Path 1: Use stored entity_ids if available
CALL (c, p, stored_entity_ids) {
  WITH c, p, stored_entity_ids
  WHERE size(stored_entity_ids) > 0
  UNWIND stored_entity_ids AS entity_id
  MATCH (e:__Entity__)-[:IN_PROJECT]->(p)
  WHERE entity_id IN coalesce(e.merged_ids, [e.id])
  RETURN collect(DISTINCT e) AS entities_from_stored
}

// Path 2: Fallback to existing IN_COMMUNITY relationships
CALL (c, p) {
  OPTIONAL MATCH (e:__Entity__)-[:IN_COMMUNITY]->(c)
  WHERE (e)-[:IN_PROJECT]->(p)
  RETURN collect(DISTINCT e) AS entities_from_rels
}

// Path 3: Fallback to text_unit_ids -> chunks -> entities
CALL (c, p, text_unit_ids) {
  WITH c, p, text_unit_ids
  WHERE size(text_unit_ids) > 0
  UNWIND text_unit_ids AS chunk_id
  MATCH (ch:__Chunk__)-[:IN_PROJECT]->(p)
  WHERE ch.id = chunk_id
  OPTIONAL MATCH (ch)-[:HAS_ENTITY]->(e:__Entity__)-[:IN_PROJECT]->(p)
  RETURN collect(DISTINCT e) AS entities_from_chunks
}

// Combine all sources (prioritize stored, then relationships, then chunks)
WITH c, p, 
     CASE 
       WHEN size(entities_from_stored) > 0 THEN entities_from_stored
       WHEN size(entities_from_rels) > 0 THEN entities_from_rels
       WHEN size(entities_from_chunks) > 0 THEN entities_from_chunks
       ELSE []
     END AS effective_entities

// Only proceed if we found entities from any source
WITH c, p, effective_entities
WHERE size(effective_entities) > 0

// Update entity_ids with actual entity IDs after deduplication
SET c.entity_ids = [e IN effective_entities | e.id]

// Create IN_COMMUNITY relationships for entities
WITH c, p, effective_entities
UNWIND effective_entities AS e
MERGE (e)-[:IN_COMMUNITY]->(c)

// Create IN_COMMUNITY relationships for chunks containing these entities
WITH c, p, e
MATCH (ch:__Chunk__)-[:HAS_ENTITY]->(e)
WHERE (ch)-[:IN_PROJECT]->(p)
MERGE (ch)-[:IN_COMMUNITY]->(c)

// Return statistics for monitoring
WITH c, p
RETURN count(DISTINCT c) AS communities_processed
