// Backfill entity.relationship_ids with outgoing and incoming RELATED relationship IDs
// SCOPED to specific project for multi-tenant safety
// Parameters: $project_id (string)

MATCH (p:__Project__ {id: $project_id})
MATCH (e:__Entity__)-[:IN_PROJECT]->(p)

// Collect outgoing relationship IDs
CALL (e) {
  OPTIONAL MATCH (e)-[r:RELATED]->()
  RETURN collect(DISTINCT r.id) AS out_ids
}

// Collect incoming relationship IDs
CALL (e) {
  OPTIONAL MATCH ()-[r:RELATED]->(e)
  RETURN collect(DISTINCT r.id) AS in_ids
}

// Combine and filter nulls
WITH e, [id IN (out_ids + in_ids) WHERE id IS NOT NULL] AS all_rel_ids
SET e.relationship_ids = all_rel_ids
