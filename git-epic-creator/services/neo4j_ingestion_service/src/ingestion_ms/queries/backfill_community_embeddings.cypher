// Query communities without embeddings
MATCH (c:__Community__)-[:IN_PROJECT]->(p:__Project__ {id: $project_id})
WHERE (c.embedding IS NULL OR size(c.embedding) = 0)
  AND c.summary IS NOT NULL AND c.summary <> ''
RETURN c.community AS community_id, 
       c.project_id AS project_id,
       c.summary AS summary,
       c.level AS level
ORDER BY c.level DESC  // Process highest level first

