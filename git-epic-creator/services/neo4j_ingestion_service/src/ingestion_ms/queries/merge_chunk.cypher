UNWIND $rows AS v
MERGE (c:__Chunk__ {id:v.id}) SET c += v
WITH c,v,coalesce(v.document_ids,[]) AS dids
UNWIND dids AS did
WITH c,did WHERE did IS NOT NULL
MATCH (d:__Document__ {id:did})
MERGE (d)-[:HAS_CHUNK]->(c)
WITH c WHERE c.text IS NOT NULL AND c.text <> ''
MATCH (x:__Chunk__ {text:c.text})
WITH c,x ORDER BY x.id
WITH head(collect(x)) AS canon, tail(collect(x)) AS dups
UNWIND dups AS dup
CALL {
  WITH dup,canon
  MATCH (a)-[r]->(dup)
  WITH a,canon,type(r) AS t,properties(r) AS p
  CALL apoc.create.relationship(a,t,p,canon) YIELD rel
  RETURN 0 AS moved_in_done
}
WITH dup,canon
CALL {
  WITH dup,canon
  MATCH (dup)-[r]->(b)
  WITH canon,b,type(r) AS t,properties(r) AS p
  CALL apoc.create.relationship(canon,t,p,b) YIELD rel
  RETURN 0 AS moved_out_done
}
WITH dup,canon
DETACH DELETE dup
WITH canon
CALL {
  WITH canon
  MATCH (canon)-[r]->(b)
  WITH canon,b,type(r) AS t,collect(r) AS rs
  WHERE size(rs) > 1
  FOREACH (x IN rs[1..] | DELETE x)
  RETURN 0 AS dedup_out_done
}
WITH canon
CALL {
  WITH canon
  MATCH (a)-[r]->(canon)
  WITH a,canon,type(r) AS t,collect(r) AS rs
  WHERE size(rs) > 1
  FOREACH (x IN rs[1..] | DELETE x)
  RETURN 0 AS dedup_in_done
}
RETURN 0