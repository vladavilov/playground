WITH $rows AS rows
UNWIND rows AS value
// Resolve source entity (pick one deterministically)
MATCH (s:__Entity__)
WHERE toUpper(coalesce(s.norm_title, s.title)) = toUpper(value.source)
WITH rows, value, s
ORDER BY s.id
LIMIT 1
// Resolve target entity (pick one deterministically)
MATCH (t:__Entity__)
WHERE toUpper(coalesce(t.norm_title, t.title)) = toUpper(value.target)
WITH rows, value, s, t
ORDER BY t.id
LIMIT 1
MERGE (s)-[rel:RELATED]->(t)
ON CREATE SET
  rel += value,
  rel.id = coalesce(value.id, s.id + '|' + t.id),
  rel.source_id = s.id,
  rel.target_id = t.id
ON MATCH SET
  rel.description = coalesce(rel.description, value.description),
  rel.source = coalesce(rel.source, value.source),
  rel.target = coalesce(rel.target, value.target),
  rel.source_id = coalesce(rel.source_id, s.id),
  rel.target_id = coalesce(rel.target_id, t.id),
  rel.weight = coalesce(rel.weight, value.weight),
  rel.combined_degree = coalesce(rel.combined_degree, value.combined_degree)
// Deduplicate text_unit_ids without APOC, preserving existing + new
WITH rows, rel, value
WITH rows, rel, coalesce(rel.text_unit_ids, []) + coalesce(value.text_unit_ids, []) AS ids
UNWIND ids AS id
WITH rows, rel, id WHERE id IS NOT NULL
WITH rows, rel, collect(DISTINCT id) AS dedup
SET rel.text_unit_ids = dedup
WITH rows
RETURN size(rows) AS processed
