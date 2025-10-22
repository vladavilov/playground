// Backfill community.entity_ids and IN_COMMUNITY relationships for current project
// Uses merged_ids to match entities even if their IDs were deduplicated
// SCOPED to specific project for multi-tenancy safety
MATCH (p:__Project__ {id: $project_id})
MATCH (c:__Community__)-[:IN_PROJECT]->(p) 
WHERE c.project_id = $project_id
WITH c, p, coalesce(c.entity_ids, []) AS stored_entity_ids
WHERE size(stored_entity_ids) > 0

// Match entities by merged_ids to handle deduplication
UNWIND stored_entity_ids AS entity_id
MATCH (e:__Entity__)-[:IN_PROJECT]->(p)
WHERE entity_id IN coalesce(e.merged_ids, [e.id])
WITH c, p, collect(DISTINCT e) AS matched_entities
WHERE size(matched_entities) > 0

// Update entity_ids with actual entity IDs after deduplication
SET c.entity_ids = [e IN matched_entities | e.id]

// Create IN_COMMUNITY relationships for entities
WITH c, p, matched_entities
UNWIND matched_entities AS e
MERGE (e)-[:IN_COMMUNITY]->(c)

// Create IN_COMMUNITY relationships for chunks containing these entities
WITH c, p, e
MATCH (ch:__Chunk__)-[:HAS_ENTITY]->(e)
WHERE (ch)-[:IN_PROJECT]->(p)
MERGE (ch)-[:IN_COMMUNITY]->(c)

// Return statistics for monitoring
WITH c, p
RETURN count(DISTINCT c) AS communities_processed
