UNWIND $rows AS value
WITH value, 
     toUpper(trim(coalesce(value.norm_title, value.title, ''))) AS norm_title, 
     trim(coalesce(value.description, '')) AS description

// Project scope: prevent cross-project entity merges when matching by non-unique fields.
MERGE (p:__Project__ {id: value.project_id})
WITH value, norm_title, description, p

// Deterministic entity matching by priority: id > norm_title > description.
// - id match is global (id is unique by constraint)
// - norm_title/description matches are project-scoped
OPTIONAL MATCH (e_id:__Entity__ {id: value.id})

OPTIONAL MATCH (e_nt:__Entity__ {norm_title: norm_title})-[:IN_PROJECT]->(p)
WHERE e_id IS NULL AND norm_title <> ''
WITH value, norm_title, description, p, e_id, e_nt
ORDER BY e_nt.id
WITH value, norm_title, description, p, e_id, head(collect(e_nt)) AS e_nt

OPTIONAL MATCH (e_desc:__Entity__ {description: description})-[:IN_PROJECT]->(p)
WHERE e_id IS NULL AND e_nt IS NULL AND description <> ''
WITH value, norm_title, description, p, e_id, e_nt, e_desc
ORDER BY e_desc.id
WITH value, norm_title, description, p, e_id, e_nt, head(collect(e_desc)) AS e_desc

// Select existing entity by priority or use value.id for new entity
WITH value, norm_title, description, p, coalesce(e_id, e_nt, e_desc) AS matched_entity

// MERGE on the determined id (existing entity's id or new value.id)
WITH value, norm_title, description, p, matched_entity, coalesce(matched_entity.id, value.id) AS target_id
MERGE (entity:__Entity__ {id: target_id})
ON CREATE SET 
  entity = value,
  entity.norm_title = norm_title,
  entity.merged_ids = [value.id]
ON MATCH SET 
  entity += value,
  entity.id = target_id,
  entity.norm_title = coalesce(norm_title, entity.norm_title),
  entity.merged_ids = 
    CASE 
      WHEN value.id IN coalesce(entity.merged_ids, [target_id]) 
      THEN entity.merged_ids 
      ELSE coalesce(entity.merged_ids, [target_id]) + [value.id]
    END

// Link to project
WITH entity, p, value
MERGE (entity)-[:IN_PROJECT]->(p)

// Create HAS_ENTITY relationships from chunks with project scoping
WITH entity, p, value
UNWIND coalesce(value.text_unit_ids, []) AS chunk_id
OPTIONAL MATCH (c:__Chunk__ {id: chunk_id})-[:IN_PROJECT]->(p)
FOREACH (ignored IN CASE WHEN c IS NOT NULL THEN [1] ELSE [] END |
  MERGE (c)-[:HAS_ENTITY]->(entity)
)

RETURN count(DISTINCT entity) AS entities_created
