MATCH (p:__Project__ {id: $projectId})
CALL db.index.vector.queryNodes($name, $k, $qvec) YIELD node AS n, score
WHERE (n)-[:IN_PROJECT]->(p)
RETURN n.community AS community, score

