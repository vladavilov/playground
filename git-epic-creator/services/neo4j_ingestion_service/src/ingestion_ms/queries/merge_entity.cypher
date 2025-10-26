UNWIND $rows AS value
WITH value, toUpper(coalesce(value.norm_title, value.title)) AS norm_title, trim(coalesce(value.description,'')) AS description

// Deterministic entity matching: find all potential matches
OPTIONAL MATCH (e_id:__Entity__ {id: value.id})
WITH value, norm_title, description, e_id
CALL (norm_title) {
  WITH norm_title WHERE norm_title IS NOT NULL AND norm_title <> ''
  OPTIONAL MATCH (e_nt:__Entity__ {norm_title: norm_title})
  RETURN e_nt ORDER BY e_nt.id LIMIT 1
}
WITH value, norm_title, description, e_id, e_nt
CALL (description) {
  WITH description WHERE description <> ''
  OPTIONAL MATCH (e_desc:__Entity__ {description: description})
  RETURN e_desc ORDER BY e_desc.id LIMIT 1
}

// Deterministic selection: prefer id match, then norm_title, then description
WITH value, norm_title, description,
     CASE
       WHEN e_id IS NOT NULL THEN e_id
       WHEN e_nt IS NOT NULL THEN e_nt
       WHEN e_desc IS NOT NULL THEN e_desc
       ELSE NULL
     END AS e

// Create only if no match found
CALL (value, e) {
  WITH value, e
  WITH value, e WHERE e IS NULL
  CREATE (e_new:__Entity__ {id: value.id})
  RETURN e_new
  UNION
  WITH e
  WITH e WHERE e IS NOT NULL
  RETURN e AS e_new
}
WITH value, norm_title, coalesce(e, e_new) AS e
WITH e, value, e.id AS existing_id, norm_title
SET e += value
SET e.id = coalesce(existing_id, value.id)
SET e.norm_title = coalesce(norm_title, e.norm_title)
// Track all merged entity IDs for reliable community relationship creation
SET e.merged_ids = coalesce(e.merged_ids, [e.id]) + 
    CASE WHEN value.id IN coalesce(e.merged_ids, [e.id]) THEN [] ELSE [value.id] END
WITH e, value, value.project_id AS pid
MERGE (p:__Project__ {id: pid})
MERGE (e)-[:IN_PROJECT]->(p)
// Create HAS_ENTITY relationships from chunks to entities WITH project scoping
WITH e, p, value
UNWIND coalesce(value.text_unit_ids,[]) AS chunk_id
// Use OPTIONAL MATCH to preserve entity even if chunk doesn't exist yet (order dependency)
OPTIONAL MATCH (c:__Chunk__)-[:IN_PROJECT]->(p)
WHERE c.id = chunk_id
// Only create relationship if chunk exists
FOREACH (ignored IN CASE WHEN c IS NOT NULL THEN [1] ELSE [] END |
  MERGE (c)-[:HAS_ENTITY]->(e)
)
WITH e
RETURN count(DISTINCT e) AS entities_created
