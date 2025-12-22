// Optimized: call vector index once, then scope results to the project + selected communities.
// Parameters:
// - $projectId (string)
// - $cids (list<int>)
// - $chunkIndex (string) vector index name
// - $qvec (list<float>)
// - $limit (int)
MATCH (p:__Project__ {id: $projectId})

// Oversample because vector search is approximate and we post-filter.
WITH p, $cids AS cids, $chunkIndex AS chunkIndex, $qvec AS qvec, $limit AS limit
CALL db.index.vector.queryNodes(chunkIndex, limit * 20, qvec) YIELD node AS ch, score
WHERE (ch:__Chunk__) AND (ch)-[:IN_PROJECT]->(p)
  AND EXISTS {
    MATCH (ch)-[:IN_COMMUNITY]->(c:__Community__)-[:IN_PROJECT]->(p)
    WHERE c.community IN cids AND c.project_id = $projectId
  }
RETURN DISTINCT ch.id AS chunk_id, score
ORDER BY score DESC
LIMIT limit

