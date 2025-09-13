// Optional helper: fetch neighbor names/types/descriptions for prompt context
MATCH (e:`__Entity__`)
WHERE elementId(e) = $id
WITH e, [(e)-[r]-(n:`__Entity__`) | {rel: type(r), neighbor_id: elementId(n), neighbor_type: coalesce(properties(n)['type'], head(labels(n))), neighbor_desc: n.description}] AS neighbors
RETURN elementId(e) AS id, coalesce(properties(e)['type'], head(labels(e))) AS type, e.description AS description, neighbors


