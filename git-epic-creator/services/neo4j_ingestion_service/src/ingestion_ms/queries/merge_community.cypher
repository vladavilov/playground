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
  // Use OPTIONAL MATCH to ensure we always get a row back (even if no entities found)
  OPTIONAL MATCH (e:__Entity__)-[:IN_PROJECT]->(p)
  WHERE size(entity_ids) > 0 
    AND ANY(eid IN entity_ids WHERE eid IN coalesce(e.merged_ids, [e.id]))
  // Filter out NULLs from collection (when OPTIONAL MATCH found nothing)
  WITH collect(DISTINCT e) AS all_entities
  RETURN [e IN all_entities WHERE e IS NOT NULL] AS entities_from_ids
}

// Path 2: Match chunks by text_unit_ids (independent of entity_ids)
WITH c, p, value, entities_from_ids
// Process text_unit_ids if they exist
WITH c, p, value, entities_from_ids, coalesce(value.text_unit_ids, []) AS text_unit_ids
UNWIND CASE WHEN size(text_unit_ids) > 0 THEN text_unit_ids ELSE [null] END AS chunk_id
// Use OPTIONAL MATCH so we get NULL when chunk not found (but still get a row)
OPTIONAL MATCH (ch:__Chunk__ {id: chunk_id})-[:IN_PROJECT]->(p)
WHERE chunk_id IS NOT NULL  // Filter out the null placeholder
OPTIONAL MATCH (ch)-[:HAS_ENTITY]->(e:__Entity__)-[:IN_PROJECT]->(p)
// Aggregate results (NULLs filtered during collection)
WITH c, p, entities_from_ids, collect(DISTINCT e) AS all_entities, collect(DISTINCT ch) AS all_chunks
// Always return collections (even if empty) - critical for downstream CASE statement
WITH c, p, entities_from_ids,
     [e IN all_entities WHERE e IS NOT NULL] AS entities_from_chunks,
     [ch IN all_chunks WHERE ch IS NOT NULL] AS chunks_from_text_units

// Determine effective entity set (prioritize entity_ids, fallback to chunks)
WITH c, p, 
     CASE 
       WHEN size(coalesce(entities_from_ids, [])) > 0 THEN entities_from_ids
       WHEN size(coalesce(entities_from_chunks, [])) > 0 THEN entities_from_chunks
       ELSE []
     END AS effective_entities,
     coalesce(chunks_from_text_units, []) AS related_chunks

// ===================================================================
// ENTITY RELATIONSHIPS (only when entities exist)
// ===================================================================
// Create entity relationships only for communities that have entities
CALL (c, p, effective_entities) {
  WITH c, p, effective_entities
  WHERE size(effective_entities) > 0
  
  // Update entity_ids with actual entity IDs after deduplication/inference
  SET c.entity_ids = [e IN effective_entities | e.id]
  
  // Create Entity -> Community relationships
  WITH c, p, effective_entities
  UNWIND effective_entities AS e
  MERGE (e)-[:IN_COMMUNITY]->(c)
  
  // Create Chunk -> Community relationships for chunks containing these entities
  WITH c, p, e
  MATCH (ch:__Chunk__)-[:HAS_ENTITY]->(e)
  WHERE (ch)-[:IN_PROJECT]->(p)
  MERGE (ch)-[:IN_COMMUNITY]->(c)
  
  RETURN count(DISTINCT c) AS entity_communities
}

// ===================================================================
// CHUNK RELATIONSHIPS (independent of entities)
// ===================================================================
// Link chunks from text_unit_ids directly (may or may not have entities)
// This runs SEPARATELY from entity logic, ensuring all chunks get linked
WITH c, p, related_chunks
WHERE size(related_chunks) > 0
UNWIND related_chunks AS ch
MERGE (ch)-[:IN_COMMUNITY]->(c)

// ===================================================================
// RETURN
// ===================================================================
WITH c
RETURN count(DISTINCT c) AS communities_created
