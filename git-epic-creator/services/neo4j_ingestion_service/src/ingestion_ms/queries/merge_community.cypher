UNWIND $rows AS value
WITH value
WITH value, value.project_id AS pid
MERGE (p:__Project__ {id: pid})
MERGE (c:__Community__ {community:value.community, project_id: pid}) 
SET c.id = toString(value.community) + '_' + pid,
    c += value 
MERGE (c)-[:IN_PROJECT]->(p)

// Match entities by merged_ids to handle entity deduplication
// entity_ids from parquet may reference IDs that were merged into other entities
WITH c, p, value, range(0, coalesce(size(value.entity_ids),0)-1) AS idxs 
UNWIND idxs AS i 
WITH c, p, value.entity_ids[i] AS entity_id 
WITH c, p, entity_id WHERE entity_id IS NOT NULL 
// Match by merged_ids to find entity even if this ID was merged into another
MATCH (e:__Entity__)-[:IN_PROJECT]->(p)
WHERE entity_id IN coalesce(e.merged_ids, [e.id])
WITH c, p, collect(DISTINCT e) AS matched_entities

// Update entity_ids with actual entity IDs after deduplication
SET c.entity_ids = [e IN matched_entities | e.id]

// Create Entity -> Community relationships
WITH c, p, matched_entities
UNWIND matched_entities AS e
MERGE (e)-[:IN_COMMUNITY]->(c)

// Create Chunk -> Community relationships for all chunks containing these entities
WITH c, p, e
MATCH (ch:__Chunk__)-[:HAS_ENTITY]->(e)
WHERE (ch)-[:IN_PROJECT]->(p)
MERGE (ch)-[:IN_COMMUNITY]->(c)
