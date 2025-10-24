UNWIND $rows AS value
WITH value, toUpper(coalesce(value.norm_title, value.title)) AS norm_title, trim(coalesce(value.description,'')) AS description
OPTIONAL MATCH (e_id:__Entity__ {id: value.id})
WITH value, norm_title, description, e_id
CALL (norm_title) {
WITH norm_title
WITH norm_title WHERE norm_title IS NOT NULL AND norm_title <> ''
OPTIONAL MATCH (e_nt:__Entity__ {norm_title: norm_title})
RETURN e_nt LIMIT 1
}
WITH value, norm_title, description, e_id, e_nt
WITH value, norm_title, description, coalesce(e_id, e_nt) AS e_pref1
// choose a single match by description if present

CALL (description) {
WITH description
WITH description WHERE description <> ''
OPTIONAL MATCH (e_desc:__Entity__ {description: description})
RETURN e_desc LIMIT 1
}
WITH value, norm_title, description, e_pref1, e_desc
WITH value, norm_title, description, coalesce(e_pref1, e_desc) AS e
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
SET e.merged_ids = coalesce(e.merged_ids, []) + 
    CASE WHEN value.id IN coalesce(e.merged_ids, []) THEN [] ELSE [value.id] END
WITH e, value, value.project_id AS pid
MERGE (p:__Project__ {id: pid})
MERGE (e)-[:IN_PROJECT]->(p)
// Create HAS_ENTITY relationships from chunks to entities WITH project scoping
WITH e, p, value
UNWIND coalesce(value.text_unit_ids,[]) AS chunk_id
// Add project scoping to chunk MATCH to prevent cross-project contamination
MATCH (c:__Chunk__)-[:IN_PROJECT]->(p)
WHERE c.id = chunk_id
MERGE (c)-[:HAS_ENTITY]->(e)
