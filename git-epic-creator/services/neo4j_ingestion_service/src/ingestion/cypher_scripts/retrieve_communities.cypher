MATCH (c:`__Community__`)<-[:IN_COMMUNITY*]-(e:__Entity__)
WHERE c.level IN range(0, $maxLevels)
WITH c, collect(e) AS nodes
WHERE size(nodes) > 1
CALL apoc.path.subgraphAll(nodes[0], {
  whitelistNodes:nodes
})
YIELD relationships
RETURN c.id AS communityId,
  [n in nodes | {id: id(n), type: properties(n).type, description: n.description}] AS nodes,
  [r in relationships | {start: id(startNode(r)), type: type(r), end: id(endNode(r)), description: r.description}] AS rels
