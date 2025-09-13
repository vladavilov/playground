WITH 'communities' AS g, gds.graph.exists('communities') AS exists
CALL {
  WITH g, exists
  WITH g, exists
  WHERE exists
  CALL gds.graph.drop(g) YIELD graphName
  RETURN graphName
  UNION
  WITH g, exists
  WITH g, exists
  WHERE NOT exists
  RETURN null AS graphName
}
MATCH (s:__Entity__)-[r]->(t:__Entity__)
WHERE ($projectId IS NULL OR (toString(s.project_id) = toString($projectId) AND toString(t.project_id) = toString($projectId)))
  AND ($relationshipTypes IS NULL OR type(r) IN $relationshipTypes)
  AND coalesce(r.weight, 1.0) >= $minWeight
WITH s, t, r
WITH gds.graph.project(
  'communities',
  s,
  t,
  {
    relationshipType: 'R',
    relationshipProperties: r { .weight },
    sourceNodeLabels: labels(s),
    targetNodeLabels: labels(t)
  },
  { undirectedRelationshipTypes: ['R'] }
) AS g
RETURN g.graphName AS graphName
