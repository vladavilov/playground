UNWIND $rows AS value
WITH value
WITH value, value.project_id AS pid
MERGE (p:__Project__ {id: pid})
MERGE (c:__Community__ {community:value.community, project_id: pid}) 
SET c.id = toString(value.community) + '_' + pid,
    c += value 
MERGE (c)-[:IN_PROJECT]->(p)
WITH c, p, value, range(0, coalesce(size(value.entity_ids),0)-1) AS idxs 
UNWIND idxs AS i 
WITH c, p, value.entity_ids[i] AS entity_id 
WITH c, p, entity_id WHERE entity_id IS NOT NULL 
MATCH (e:__Entity__ {id:entity_id})-[:IN_PROJECT]->(p)
MERGE (e)-[:IN_COMMUNITY]->(c)
WITH c, p, e
MATCH (ch:__Chunk__)-[:HAS_ENTITY]->(e)
MATCH (ch)-[:IN_PROJECT]->(p)
MERGE (ch)-[:IN_COMMUNITY]->(c)
