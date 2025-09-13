MATCH (c:__Community__) 
WHERE coalesce(size(c.entity_ids),0) = 0 
WITH c, coalesce(c.text_unit_ids, []) AS chunk_ids 
UNWIND chunk_ids AS cid 
MATCH (:__Chunk__ {id:cid})-[:HAS_ENTITY]->(e:__Entity__) 
WITH c, collect(DISTINCT e.id) AS eids 
SET c.entity_ids = eids 
WITH c, eids 
UNWIND eids AS eid 
MATCH (e:__Entity__ {id:eid}) 
MERGE (e)-[:IN_COMMUNITY]->(c)
