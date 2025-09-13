MATCH (e:`__Entity__`)
WHERE elementId(e) = $id
SET e.description = $description
RETURN elementId(e) AS id


