WITH $rows AS rows
UNWIND rows AS value
OPTIONAL MATCH ()-[r:RELATED {id: value.id}]->()
FOREACH (_ IN CASE WHEN r IS NOT NULL THEN [1] ELSE [] END |
  SET r += value
)
WITH rows, value, r
WHERE r IS NULL
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
MERGE (s)-[rel:RELATED {id: value.id}]->(t)
SET rel += value, rel.source_id = s.id, rel.target_id = t.id
WITH rows
RETURN size(rows) AS processed
