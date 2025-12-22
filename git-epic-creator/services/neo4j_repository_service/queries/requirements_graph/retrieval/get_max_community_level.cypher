MATCH (c:__Community__)-[:IN_PROJECT]->(:__Project__ {id: $projectId})
RETURN max(c.level) AS max_level

