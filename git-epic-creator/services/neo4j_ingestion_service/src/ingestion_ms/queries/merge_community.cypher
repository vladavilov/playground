UNWIND $rows AS value
WITH value, value.project_id AS pid
MERGE (p:__Project__ {id: pid})
MERGE (c:__Community__ {community: value.community, project_id: pid}) 
SET c.id = toString(value.community) + '_' + pid, c += value 
MERGE (c)-[:IN_PROJECT]->(p)

// Match entities by entity_ids (supports merged_ids lookup and norm_title fallback)
WITH c, p, value
OPTIONAL MATCH (e_id:__Entity__)-[:IN_PROJECT]->(p)
WHERE size(coalesce(value.entity_ids, [])) > 0 
  AND (
    ANY(eid IN value.entity_ids WHERE eid = e_id.id OR eid IN e_id.merged_ids)
    OR ANY(eid IN value.entity_ids WHERE toUpper(eid) = e_id.norm_title)
  )
WITH c, p, value, collect(DISTINCT e_id) AS entities_from_ids

// Match chunks and their entities from text_unit_ids
WITH c, p, value, entities_from_ids
OPTIONAL MATCH (ch:__Chunk__)-[:IN_PROJECT]->(p)
WHERE size(coalesce(value.text_unit_ids, [])) > 0 AND ch.id IN value.text_unit_ids
OPTIONAL MATCH (ch)-[:HAS_ENTITY]->(e_ch:__Entity__)-[:IN_PROJECT]->(p)
WITH c, p, entities_from_ids,
     collect(DISTINCT ch) AS related_chunks,
     collect(DISTINCT e_ch) AS entities_from_chunks
WITH c, p, 
     CASE WHEN size(entities_from_ids) > 0 THEN entities_from_ids
          WHEN size(entities_from_chunks) > 0 THEN entities_from_chunks
          ELSE [] END AS effective_entities,
     related_chunks

// Update entity_ids property with canonical IDs when entities found
FOREACH (ignored IN CASE WHEN size(effective_entities) > 0 THEN [1] ELSE [] END |
  SET c.entity_ids = [e IN effective_entities | e.id]
)

// Create entity->community relationships and link their chunks
WITH c, p, effective_entities, related_chunks
FOREACH (e IN effective_entities |
  MERGE (e)-[:IN_COMMUNITY]->(c)
)

// Link chunks that contain these entities
WITH c, p, effective_entities, related_chunks
UNWIND CASE WHEN effective_entities = [] THEN [null] ELSE effective_entities END AS e
WITH c, p, e, related_chunks WHERE e IS NOT NULL
OPTIONAL MATCH (ch:__Chunk__)-[:HAS_ENTITY]->(e)
WHERE (ch)-[:IN_PROJECT]->(p)
FOREACH (ignored IN CASE WHEN ch IS NOT NULL THEN [1] ELSE [] END |
  MERGE (ch)-[:IN_COMMUNITY]->(c)
)

// Link chunks from text_unit_ids directly
WITH DISTINCT c, p, related_chunks
FOREACH (ch IN related_chunks |
  MERGE (ch)-[:IN_COMMUNITY]->(c)
)

WITH c
RETURN count(DISTINCT c) AS communities_created
