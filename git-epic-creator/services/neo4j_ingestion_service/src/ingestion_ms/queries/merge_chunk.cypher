UNWIND $rows AS v
MERGE (c:__Chunk__ {id:v.id}) 
SET c += v,
    c.text_hash = CASE 
      WHEN v.text IS NOT NULL AND v.text <> '' 
      THEN substring(sha256(v.text), 0, 16)
      ELSE NULL 
    END
WITH c, v, v.project_id AS pid
MERGE (p:__Project__ {id: pid})
MERGE (c)-[:IN_PROJECT]->(p)
// Create HAS_CHUNK relationships WITH project scoping to prevent cross-project contamination
WITH c, p, pid, v, coalesce(v.document_ids,[]) AS dids
UNWIND dids AS did
WITH c, p, pid, did WHERE did IS NOT NULL
// Add project scoping to document MATCH
MATCH (d:__Document__)-[:IN_PROJECT]->(p)
WHERE d.id = did
MERGE (d)-[:HAS_CHUNK]->(c)
// Ensure no duplicate HAS_CHUNK relationships exist for this chunk even if input repeats
WITH c, p, pid
MATCH (a)-[r:HAS_CHUNK]->(c)
WITH a,c,p,pid,collect(r) AS rs
WHERE size(rs) > 1
FOREACH (x IN rs[1..] | DELETE x)
// Hash-based deduplication WITH project scoping to prevent cross-project contamination
WITH c, p, pid WHERE c.text_hash IS NOT NULL
MATCH (dup:__Chunk__)-[:IN_PROJECT]->(p)
WHERE dup.text_hash = c.text_hash 
  AND dup.id < c.id
  AND dup.id <> c.id
WITH c, dup, p, pid
LIMIT 1000
// Keep newer node (c) as primary to preserve fresh properties including project_id
CALL apoc.refactor.mergeNodes([c, dup], {properties: 'combine', mergeRels: true}) 
YIELD node
// Ensure project_id is set correctly after merge (defensive programming)
SET node.project_id = pid
RETURN count(node) AS deduplicated