MATCH (e:`__Entity__`)
WITH e, $community_property AS prop
WITH e, prop, apoc.convert.toList(e[prop]) AS comms
WITH e, prop, comms
WHERE size(comms) > 0
UNWIND range(0, size(comms) - 1 , 1) AS index
CALL {
  WITH e, comms, index
  WITH e, comms, index
  WHERE index = 0
  MERGE (c:`__Community__` {id: $id_prefix + toString(index) + '-' + toString(comms[index])})
  ON CREATE SET c.level = index
  MERGE (e)-[:IN_COMMUNITY]->(c)
  RETURN count(*) AS count_0
}
CALL {
  WITH e, comms, index
  WITH e, comms, index
  WHERE index > 0
  MERGE (current:`__Community__` {id: $id_prefix + toString(index) + '-' + toString(comms[index])})
  ON CREATE SET current.level = index
  MERGE (previous:`__Community__` {id: $id_prefix + toString(index - 1) + '-' + toString(comms[index - 1])})
  ON CREATE SET previous.level = index - 1
  MERGE (previous)-[:IN_COMMUNITY]->(current)
  RETURN count(*) AS count_1
}
RETURN count(*)


