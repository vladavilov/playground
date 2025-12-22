// Validation query to check relationship health for a given project
// Parameters: $project_id (string)

MATCH (p:__Project__ {id: $project_id})
OPTIONAL MATCH (e:__Entity__)-[:IN_PROJECT]->(p)
WITH p, count(DISTINCT e) AS entity_count

OPTIONAL MATCH (s:__Entity__)-[r:RELATED]->(t:__Entity__)
WHERE (s)-[:IN_PROJECT]->(p) AND (t)-[:IN_PROJECT]->(p)
WITH p, entity_count,
     count(DISTINCT r) AS unique_rel_count,
     count(r) AS total_rel_count,
     count(DISTINCT s) AS source_entity_count,
     count(DISTINCT t) AS target_entity_count

RETURN 
  p.id AS project_id,
  entity_count,
  source_entity_count,
  target_entity_count,
  unique_rel_count,
  total_rel_count,
  total_rel_count - unique_rel_count AS duplicate_count,
  CASE 
    WHEN entity_count > 0 THEN toFloat(unique_rel_count) / entity_count 
    ELSE 0.0
  END AS avg_relationships_per_entity,
  CASE
    WHEN unique_rel_count > 0 THEN toFloat(total_rel_count) / unique_rel_count
    ELSE 0.0
  END AS duplication_ratio

