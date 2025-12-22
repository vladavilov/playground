// Update chunk embeddings, scoped to project.
// Input rows: { id, embedding }
MATCH (p:__Project__ {id: $project_id})
UNWIND $rows AS r
WITH p, r, toString(coalesce(r.id, '')) AS cid
WHERE cid <> ''
MATCH (c:__Chunk__ {id: cid})-[:IN_PROJECT]->(p)
SET c.embedding = r.embedding
RETURN count(DISTINCT c) AS updated
