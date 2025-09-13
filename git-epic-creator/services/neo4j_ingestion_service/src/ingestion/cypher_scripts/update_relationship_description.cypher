MATCH (s)-[r]->(t)
WHERE elementId(s) = $start AND elementId(t) = $end AND type(r) = $type
SET r.description = $description
RETURN elementId(s) AS start, type(r) AS type, elementId(t) AS end


