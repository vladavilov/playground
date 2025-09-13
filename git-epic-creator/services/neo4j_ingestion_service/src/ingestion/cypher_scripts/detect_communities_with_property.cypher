// Guard against empty graphs to prevent NPE in Leiden internals
CALL gds.graph.list($graph_name) YIELD graphName, relationshipCount
WITH graphName, relationshipCount
WHERE relationshipCount > 0
CALL gds.leiden.write(
  graphName,
  {
    writeProperty: $writeProperty,
    relationshipWeightProperty: 'weight',
    concurrency: $concurrency
  }
) YIELD ranLevels
RETURN ranLevels, relationshipCount
UNION
CALL gds.graph.list($graph_name) YIELD graphName, relationshipCount
WITH relationshipCount
WHERE relationshipCount = 0
RETURN 0 AS ranLevels, relationshipCount
