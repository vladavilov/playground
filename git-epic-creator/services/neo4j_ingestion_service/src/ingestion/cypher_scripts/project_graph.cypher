CALL gds.graph.project(
  "communities",
  "__Entity__",
  {
    _ALL_: {
      type: "*",
      orientation: "UNDIRECTED",
      properties: {weight: {property: "*", aggregation: "COUNT"}}
    }
  }
)
