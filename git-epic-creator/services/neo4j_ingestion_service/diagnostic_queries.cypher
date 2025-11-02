// Diagnostic queries to understand missing Entity->Community relationships

// Query 1: Check if entities exist and have text_unit_ids
MATCH (e:__Entity__)-[:IN_PROJECT]->(p:__Project__ {id: $project_id})
RETURN 
    count(e) AS total_entities,
    count(DISTINCT CASE WHEN size(coalesce(e.text_unit_ids, [])) > 0 THEN e END) AS entities_with_text_units,
    count(DISTINCT CASE WHEN size(coalesce(e.merged_ids, [])) > 0 THEN e END) AS entities_with_merged_ids;

// Query 2: Check if communities exist and have entity_ids/text_unit_ids
MATCH (c:__Community__)-[:IN_PROJECT]->(p:__Project__ {id: $project_id})
RETURN 
    count(c) AS total_communities,
    count(DISTINCT CASE WHEN size(coalesce(c.entity_ids, [])) > 0 THEN c END) AS communities_with_entity_ids,
    count(DISTINCT CASE WHEN size(coalesce(c.text_unit_ids, [])) > 0 THEN c END) AS communities_with_text_unit_ids;

// Query 3: Check if HAS_ENTITY relationships exist
MATCH (ch:__Chunk__)-[:HAS_ENTITY]->(e:__Entity__)-[:IN_PROJECT]->(p:__Project__ {id: $project_id})
RETURN count(*) AS has_entity_relationships;

// Query 4: Check if IN_COMMUNITY relationships exist (Entity->Community)
MATCH (e:__Entity__)-[:IN_COMMUNITY]->(c:__Community__)-[:IN_PROJECT]->(p:__Project__ {id: $project_id})
RETURN count(*) AS entity_to_community_relationships;

// Query 5: Test if Path 3 logic works - can we traverse from community text_unit_ids to entities?
MATCH (p:__Project__ {id: $project_id})
MATCH (c:__Community__)-[:IN_PROJECT]->(p)
WHERE size(coalesce(c.text_unit_ids, [])) > 0
WITH c, p, c.text_unit_ids AS text_unit_ids
LIMIT 5
OPTIONAL MATCH (ch:__Chunk__)-[:IN_PROJECT]->(p)
WHERE ch.id IN text_unit_ids
OPTIONAL MATCH (ch)-[:HAS_ENTITY]->(e:__Entity__)-[:IN_PROJECT]->(p)
RETURN 
    c.id AS community_id,
    c.text_unit_ids AS community_text_units,
    collect(DISTINCT ch.id) AS matched_chunks,
    collect(DISTINCT e.id) AS matched_entities;

// Query 6: Check for ID mismatches - do community entity_ids exist in Entity nodes?
MATCH (p:__Project__ {id: $project_id})
MATCH (c:__Community__)-[:IN_PROJECT]->(p)
WHERE size(coalesce(c.entity_ids, [])) > 0
WITH c, c.entity_ids AS stored_ids
LIMIT 5
UNWIND stored_ids AS eid
OPTIONAL MATCH (e:__Entity__)-[:IN_PROJECT]->(p:__Project__ {id: $project_id})
WHERE eid = e.id OR eid IN e.merged_ids
RETURN 
    c.id AS community_id,
    eid AS stored_entity_id,
    e.id AS matched_entity_id,
    CASE WHEN e IS NULL THEN 'MISSING' ELSE 'FOUND' END AS status;

