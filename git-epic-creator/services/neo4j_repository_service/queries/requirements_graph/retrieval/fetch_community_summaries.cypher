MATCH (p:__Project__ {id: $projectId})
MATCH (c:__Community__)-[:IN_PROJECT]->(p)
WHERE c.community IN $ids AND c.project_id = $projectId
RETURN c.community AS id, c.summary AS summary

