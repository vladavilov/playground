MATCH (c:__Community__)-[:IN_PROJECT]->(p:__Project__) 
WHERE coalesce(size(c.entity_ids),0) = 0 
WITH c, p, coalesce(c.text_unit_ids, []) AS chunk_ids 
UNWIND chunk_ids AS cid 
MATCH (ch:__Chunk__ {id:cid})-[:IN_PROJECT]->(p)
MATCH (ch)-[:HAS_ENTITY]->(e:__Entity__)-[:IN_PROJECT]->(p) 
WITH c, p, collect(DISTINCT e.id) AS eids 
SET c.entity_ids = eids 
WITH c, p, eids 
UNWIND eids AS eid 
MATCH (e:__Entity__ {id:eid})-[:IN_PROJECT]->(p) 
MERGE (e)-[:IN_COMMUNITY]->(c)
WITH c, p, e
MATCH (ch:__Chunk__)-[:HAS_ENTITY]->(e)
MATCH (ch)-[:IN_PROJECT]->(p)
MERGE (ch)-[:IN_COMMUNITY]->(c)
