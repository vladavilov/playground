UNWIND $rows AS value
WITH value
WITH value, value.project_id AS pid
MERGE (p:__Project__ {id: pid})
MERGE (c:__Community__ {community:value.community, project_id: pid}) 
SET c.id = toString(value.community) + '_' + pid,
    c += value 
MERGE (c)-[:IN_PROJECT]->(p)
WITH c, p, value

// Path 1: Match entities by entity_ids (if provided)
CALL (c, p, value) {
  WITH c, p, value, coalesce(value.entity_ids, []) AS entity_ids
  // Match entities where any entity_id from community is in the entity's merged_ids (handles deduplication)
  MATCH (e:__Entity__)-[:IN_PROJECT]->(p)
  WHERE size(entity_ids) > 0 
    AND ANY(eid IN entity_ids WHERE eid IN coalesce(e.merged_ids, [e.id]))
  RETURN collect(DISTINCT e) AS entities_from_ids
}

// Path 2: Fallback to text_unit_ids -> chunks -> entities (if entity_ids empty)
CALL (c, p, value) {
  WITH c, p, value, coalesce(value.text_unit_ids, []) AS text_unit_ids
  MATCH (ch:__Chunk__)-[:IN_PROJECT]->(p)
  WHERE size(text_unit_ids) > 0 AND ch.id IN text_unit_ids
  OPTIONAL MATCH (ch)-[:HAS_ENTITY]->(e:__Entity__)-[:IN_PROJECT]->(p)
  RETURN collect(DISTINCT e) AS entities_from_chunks, collect(DISTINCT ch) AS chunks_from_text_units
}

// Determine effective entity set (prioritize entity_ids, fallback to chunks)
WITH c, p, 
     CASE 
       WHEN size(coalesce(entities_from_ids, [])) > 0 THEN entities_from_ids
       WHEN size(coalesce(entities_from_chunks, [])) > 0 THEN entities_from_chunks
       ELSE []
     END AS effective_entities,
     coalesce(chunks_from_text_units, []) AS related_chunks

// Only proceed if entities found - prevents overwriting entity_ids from parquet with empty array
WITH c, p, effective_entities, related_chunks
WHERE size(effective_entities) > 0

// Update entity_ids with actual entity IDs after deduplication/inference
SET c.entity_ids = [e IN effective_entities | e.id]

// Create Entity -> Community relationships
WITH c, p, effective_entities, related_chunks
UNWIND effective_entities AS e
MERGE (e)-[:IN_COMMUNITY]->(c)

// Create Chunk -> Community relationships for chunks containing these entities
WITH c, p, e, related_chunks
MATCH (ch:__Chunk__)-[:HAS_ENTITY]->(e)
WHERE (ch)-[:IN_PROJECT]->(p)
MERGE (ch)-[:IN_COMMUNITY]->(c)

// Also link chunks from text_unit_ids directly (may not have entities)
WITH c, p, related_chunks
WHERE size(related_chunks) > 0
UNWIND related_chunks AS ch
MERGE (ch)-[:IN_COMMUNITY]->(c)
