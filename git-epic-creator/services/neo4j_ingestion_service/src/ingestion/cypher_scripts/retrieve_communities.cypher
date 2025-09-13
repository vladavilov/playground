// Paginated, capped retrieval without variable-length expansions
// Page communities first to bound memory
MATCH (c:`__Community__`)
WHERE c.level >= 0 AND c.level <= $maxLevels
WITH c
ORDER BY c.level ASC, c.id ASC
SKIP $offset LIMIT $pageSize

// For each paged community, fetch a capped set of member entities
OPTIONAL MATCH (e:__Entity__)-[:IN_COMMUNITY]->(c)
WITH c, collect(e)[..$nodeCap] AS nodes
WITH c, nodes, size(nodes) AS communitySize
WHERE communitySize > 1

// Fetch relationships among the capped node set with a cap
WITH c, nodes,
     [n IN nodes | elementId(n)] AS nodeIds,
     communitySize
OPTIONAL MATCH (n)-[r]-(m)
WHERE elementId(n) IN nodeIds AND elementId(m) IN nodeIds
WITH c, nodes, collect(r)[..$relCap] AS rels, communitySize

RETURN c.id AS communityId,
  [n in nodes | {id: elementId(n), type: coalesce(properties(n)['type'], head(labels(n))), description: n.description}] AS nodes,
  [r in rels | {start: elementId(startNode(r)), type: type(r), end: elementId(endNode(r)), description: r.description}] AS rels,
  communitySize
ORDER BY communitySize DESC, communityId ASC
