UNWIND $rows AS value
WITH value
MERGE (c:__Community__ {community:value.community}) 
SET c += value 
WITH c, value, value.project_id AS pid 
MERGE (p:__Project__ {id: pid})
MERGE (c)-[:IN_PROJECT]->(p)
WITH c, value, range(0, coalesce(size(value.entity_ids),0)-1) AS idxs 
UNWIND idxs AS i 
WITH c, value.entity_ids[i] AS entity_id 
WITH c, entity_id WHERE entity_id IS NOT NULL 
MATCH (e:__Entity__ {id:entity_id}) 
MERGE (e)-[:IN_COMMUNITY]->(c)
WITH c, e
MATCH (ch:__Chunk__)-[:HAS_ENTITY]->(e)
MERGE (ch)-[:IN_COMMUNITY]->(c)
