// Update community embeddings, scoped to project.
// Backward-compatible input rows:
// - Preferred: { community, embedding } (matched by (project_id, community))
// - Also supported: { id, embedding } (matched by id)
// - Also supported: { text, embedding } where text = full_content (legacy)
MATCH (p:__Project__ {id: $project_id})
UNWIND $rows AS r
WITH
  p,
  r,
  toString(coalesce(r.id, '')) AS cid,
  r.community AS community,
  toString(coalesce(r.text, '')) AS full_content

OPTIONAL MATCH (c_by_key:__Community__ {project_id: $project_id, community: community})-[:IN_PROJECT]->(p)
WHERE community IS NOT NULL
WITH p, r, cid, full_content, c_by_key
OPTIONAL MATCH (c_by_id:__Community__ {id: cid})-[:IN_PROJECT]->(p)
WHERE c_by_key IS NULL AND cid <> ''
WITH p, r, full_content, coalesce(c_by_key, c_by_id) AS c0
OPTIONAL MATCH (c_by_text:__Community__)-[:IN_PROJECT]->(p)
WHERE c0 IS NULL AND full_content <> '' AND c_by_text.full_content = full_content
WITH r, coalesce(c0, head(collect(c_by_text))) AS c

FOREACH (_ IN CASE WHEN c IS NOT NULL THEN [1] ELSE [] END |
  SET c.embedding = r.embedding
)

RETURN count(CASE WHEN c IS NOT NULL THEN 1 END) AS updated


