MATCH (e:`__Entity__`)
WHERE e.description IS NULL OR trim(e.description) = ""
WITH e, COUNT { (e)--() } AS degree
RETURN elementId(e) AS id, coalesce(properties(e)['type'], head(labels(e))) AS type, degree
ORDER BY degree DESC
LIMIT $limit


