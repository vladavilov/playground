// Only process communities with missing entity_ids AND non-empty text_unit_ids
MATCH (c:__Community__)-[:IN_PROJECT]->(p:__Project__) 
WHERE NOT exists(c.entity_ids) OR c.entity_ids = []
WITH c, p, coalesce(c.text_unit_ids, []) AS chunk_ids
WHERE size(chunk_ids) > 0

// Process each chunk to find entities
UNWIND chunk_ids AS cid 
MATCH (ch:__Chunk__ {id:cid})-[:IN_PROJECT]->(p)
MATCH (ch)-[:HAS_ENTITY]->(e:__Entity__)-[:IN_PROJECT]->(p) 
WITH c, p, collect(DISTINCT e.id) AS eids 
WHERE size(eids) > 0
SET c.entity_ids = eids 

// Create IN_COMMUNITY relationships for entities
WITH c, p, eids 
UNWIND eids AS eid 
MATCH (e:__Entity__ {id:eid})-[:IN_PROJECT]->(p) 
MERGE (e)-[:IN_COMMUNITY]->(c)

// Create IN_COMMUNITY relationships for chunks containing these entities
WITH c, p, e
MATCH (ch:__Chunk__)-[:HAS_ENTITY]->(e)
WHERE (ch)-[:IN_PROJECT]->(p)
MERGE (ch)-[:IN_COMMUNITY]->(c)
