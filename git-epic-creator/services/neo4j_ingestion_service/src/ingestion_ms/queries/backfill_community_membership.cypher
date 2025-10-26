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
  OPTIONAL MATCH (e:__Entity__)-[:IN_PROJECT]->(p)
  WHERE size(stored_entity_ids) > 0 
    AND ANY(eid IN stored_entity_ids WHERE eid IN coalesce(e.merged_ids, [e.id]))
  // Filter out NULLs from collection (when OPTIONAL MATCH found nothing)
  WITH collect(DISTINCT e) AS all_entities
  RETURN [e IN all_entities WHERE e IS NOT NULL] AS entities_from_stored
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
  OPTIONAL MATCH (ch:__Chunk__)-[:IN_PROJECT]->(p)
  WHERE size(text_unit_ids) > 0 AND ch.id IN text_unit_ids
  OPTIONAL MATCH (ch)-[:HAS_ENTITY]->(e:__Entity__)-[:IN_PROJECT]->(p)
  // Filter out NULLs from collection (when OPTIONAL MATCH found nothing)
  WITH collect(DISTINCT e) AS all_entities
  RETURN [e IN all_entities WHERE e IS NOT NULL] AS entities_from_chunks
}

// Combine all sources (prioritize stored, then relationships, then chunks)
WITH c, p, 
     CASE 
       WHEN size(coalesce(entities_from_stored, [])) > 0 THEN entities_from_stored
       WHEN size(coalesce(entities_from_rels, [])) > 0 THEN entities_from_rels
       WHEN size(coalesce(entities_from_chunks, [])) > 0 THEN entities_from_chunks
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

// Deduplicate Entity -> Community relationships
WITH c, p
MATCH (e:__Entity__)-[r:IN_COMMUNITY]->(c)
WITH c, p, e, collect(r) AS rels
WHERE size(rels) > 1
FOREACH (rel IN rels[1..] | DELETE rel)

// Deduplicate Chunk -> Community relationships
WITH c, p
MATCH (ch:__Chunk__)-[r:IN_COMMUNITY]->(c)
WITH c, p, ch, collect(r) AS rels
WHERE size(rels) > 1
FOREACH (rel IN rels[1..] | DELETE rel)

// Return statistics for monitoring
WITH c, p
RETURN count(DISTINCT c) AS communities_processed
