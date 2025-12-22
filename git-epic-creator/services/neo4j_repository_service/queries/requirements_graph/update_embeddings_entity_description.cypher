// Update entity embeddings, scoped to project.
// Backward-compatible input rows:
// - Preferred: { id, embedding }
// - Also supported: { norm_title, embedding }
// - Also supported: { text, embedding } where text = description (legacy)
MATCH (p:__Project__ {id: $project_id})
UNWIND $rows AS r
WITH
  p,
  r,
  toString(coalesce(r.id, '')) AS eid,
  toUpper(trim(toString(coalesce(r.norm_title, '')))) AS norm_title,
  trim(toString(coalesce(r.text, ''))) AS description

// Deterministic match priority: id > norm_title > description (all project-scoped).
OPTIONAL MATCH (e_id:__Entity__ {id: eid})-[:IN_PROJECT]->(p)
WHERE eid <> ''
WITH p, r, e_id, norm_title, description
OPTIONAL MATCH (e_nt:__Entity__ {norm_title: norm_title})-[:IN_PROJECT]->(p)
WHERE e_id IS NULL AND norm_title <> ''
WITH p, r, e_id, head(collect(e_nt)) AS e_nt, description
OPTIONAL MATCH (e_desc:__Entity__ {description: description})-[:IN_PROJECT]->(p)
WHERE e_id IS NULL AND e_nt IS NULL AND description <> ''
WITH r, coalesce(e_id, e_nt, head(collect(e_desc))) AS e

FOREACH (_ IN CASE WHEN e IS NOT NULL THEN [1] ELSE [] END |
  SET e.embedding = r.embedding
)

RETURN count(CASE WHEN e IS NOT NULL THEN 1 END) AS updated


