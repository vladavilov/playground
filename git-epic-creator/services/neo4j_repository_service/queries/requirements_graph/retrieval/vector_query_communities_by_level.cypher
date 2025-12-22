MATCH (p:__Project__ {id: $projectId})
CALL db.index.vector.queryNodes($name, $k * 3, $qvec) YIELD node AS n, score
WHERE (n)-[:IN_PROJECT]->(p) AND n.level = $level
RETURN n.community AS community, score
ORDER BY score DESC LIMIT $k

