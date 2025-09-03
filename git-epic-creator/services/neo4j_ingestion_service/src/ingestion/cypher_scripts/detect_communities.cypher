CALL gds.leiden.write(
  "communities",
  {
    writeProperty: "communities",
    includeIntermediateCommunities: True,
    relationshipWeightProperty: "weight"
  }
) YIELD ranLevels;
