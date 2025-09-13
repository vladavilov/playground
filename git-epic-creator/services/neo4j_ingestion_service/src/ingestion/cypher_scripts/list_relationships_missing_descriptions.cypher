MATCH (s)-[r]->(t)
WHERE (r.description IS NULL OR trim(r.description) = "")
WITH r, s, t
RETURN elementId(s) AS start, type(r) AS type, elementId(t) AS end, coalesce(properties(s)['type'], head(labels(s))) AS start_type, coalesce(properties(t)['type'], head(labels(t))) AS end_type
ORDER BY type ASC
LIMIT $limit


