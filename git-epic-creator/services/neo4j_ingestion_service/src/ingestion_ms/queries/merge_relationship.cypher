UNWIND $rows AS value

// Isolated subquery to find exactly ONE source entity deterministically
CALL (value) {
  WITH value
  MATCH (s:__Entity__)
  WHERE toUpper(coalesce(s.norm_title, s.title)) = toUpper(value.source)
  WITH s ORDER BY s.id LIMIT 1
  RETURN s
}

// Isolated subquery to find exactly ONE target entity deterministically
CALL (value) {
  WITH value
  MATCH (t:__Entity__)
  WHERE toUpper(coalesce(t.norm_title, t.title)) = toUpper(value.target)
  WITH t ORDER BY t.id LIMIT 1
  RETURN t
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

// Deduplicate text_unit_ids without APOC, preserving existing + new
WITH rel, new_text_units, coalesce(rel.text_unit_ids, []) + new_text_units AS combined_ids
WHERE size(combined_ids) > 0
UNWIND combined_ids AS id
WITH rel, collect(DISTINCT id) AS dedup_ids
SET rel.text_unit_ids = dedup_ids
RETURN count(rel) AS relationships_processed
