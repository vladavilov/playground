// Cleanup query to remove duplicate RELATED relationships for a given project
// Parameters: $project_id (string)
// Keeps the first relationship found and deletes duplicates

MATCH (p:__Project__ {id: $project_id})
MATCH (s:__Entity__)-[r:RELATED]->(t:__Entity__)
WHERE (s)-[:IN_PROJECT]->(p) AND (t)-[:IN_PROJECT]->(p)
WITH s, t, collect(r) AS rels
WHERE size(rels) > 1
WITH s, t, rels[0] AS keep_rel, rels[1..] AS delete_rels, size(rels) - 1 AS dup_count
UNWIND delete_rels AS del_rel
DELETE del_rel
WITH sum(dup_count) AS total_duplicates_removed
RETURN total_duplicates_removed
