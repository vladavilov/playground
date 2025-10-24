UNWIND $rows AS value

// Normalize source and target once for reuse
WITH value, toUpper(coalesce(value.source, '')) AS source_key, toUpper(coalesce(value.target, '')) AS target_key
WHERE source_key <> '' AND target_key <> ''

// Extract project context from first row for scoping (all rows in batch should have same project_id)
WITH value, source_key, target_key, value.project_id AS project_id
LIMIT 1
WITH source_key, target_key, project_id
MATCH (p:__Project__ {id: project_id})

// Continue processing all rows with project context
MATCH (p)
UNWIND $rows AS value
WITH p, value, toUpper(coalesce(value.source, '')) AS source_key, toUpper(coalesce(value.target, '')) AS target_key
WHERE source_key <> '' AND target_key <> ''

// Find source entity WITH project scoping to prevent cross-project matching
CALL (source_key, p) {
  MATCH (s:__Entity__)-[:IN_PROJECT]->(p)
  WHERE toUpper(coalesce(s.norm_title, s.title, '')) = source_key
  RETURN s
  ORDER BY s.id
  LIMIT 1
}

// Find target entity WITH project scoping to prevent cross-project matching
CALL (target_key, p) {
  MATCH (t:__Entity__)-[:IN_PROJECT]->(p)
  WHERE toUpper(coalesce(t.norm_title, t.title, '')) = target_key
  RETURN t
  ORDER BY t.id
  LIMIT 1
}

// Filter out cases where either entity doesn't exist
WITH value, s, t
WHERE s IS NOT NULL AND t IS NOT NULL

// Prepare text_unit_ids for merge
WITH value, s, t, coalesce(value.text_unit_ids, []) AS new_text_units

// Create or update the relationship
MERGE (s)-[rel:RELATED]->(t)
ON CREATE SET
  rel.id = coalesce(value.id, s.id + '|' + t.id),
  rel.source = value.source,
  rel.target = value.target,
  rel.source_id = s.id,
  rel.target_id = t.id,
  rel.description = value.description,
  rel.weight = value.weight,
  rel.combined_degree = value.combined_degree,
  rel.text_unit_ids = new_text_units
ON MATCH SET
  rel.description = coalesce(value.description, rel.description),
  rel.source = coalesce(value.source, rel.source),
  rel.target = coalesce(value.target, rel.target),
  rel.source_id = s.id,
  rel.target_id = t.id,
  rel.weight = coalesce(value.weight, rel.weight),
  rel.combined_degree = coalesce(value.combined_degree, rel.combined_degree)

// Deduplicate text_unit_ids after merge using pure Cypher
WITH rel, coalesce(rel.text_unit_ids, []) + new_text_units AS combined_ids
WHERE size(combined_ids) > 0
WITH rel, [id IN combined_ids WHERE id IS NOT NULL] AS filtered_ids
UNWIND filtered_ids AS text_unit_id
WITH rel, collect(DISTINCT text_unit_id) AS dedup_ids
SET rel.text_unit_ids = dedup_ids

RETURN count(DISTINCT rel) AS relationships_processed
