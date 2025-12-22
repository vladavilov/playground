UNWIND $rows AS v
MERGE (c:__Chunk__ {id:v.id}) 
SET c += v,
    c.text_hash = CASE 
      WHEN v.text IS NOT NULL AND v.text <> '' 
      THEN substring(apoc.util.sha256([v.text]), 0, 16)
      ELSE NULL 
    END
WITH c, v, v.project_id AS pid
MERGE (p:__Project__ {id: pid})
MERGE (c)-[:IN_PROJECT]->(p)
// Create HAS_CHUNK relationships WITH project scoping to prevent cross-project contamination
WITH c, p, pid, v, coalesce(v.document_ids,[]) AS dids
UNWIND dids AS did
WITH c, p, pid, did WHERE did IS NOT NULL
// Use OPTIONAL MATCH to preserve chunk even if document doesn't exist (should never happen in correct ingestion order)
OPTIONAL MATCH (d:__Document__)-[:IN_PROJECT]->(p)
WHERE d.id = did
// Only create relationship if document exists
FOREACH (ignored IN CASE WHEN d IS NOT NULL THEN [1] ELSE [] END |
  MERGE (d)-[:HAS_CHUNK]->(c)
)
// Ensure no duplicate HAS_CHUNK relationships exist for this chunk even if input repeats
WITH c, p, pid
OPTIONAL MATCH (a)-[r:HAS_CHUNK]->(c)
WITH a,c,p,pid,collect(r) AS rs
WHERE size(rs) > 1
FOREACH (x IN rs[1..] | DELETE x)
WITH c
RETURN count(DISTINCT c) AS chunks_created
