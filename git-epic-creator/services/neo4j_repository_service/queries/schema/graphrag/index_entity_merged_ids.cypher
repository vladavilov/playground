// Supports `WHERE $id IN e.merged_ids` lookups used during community ingestion.
// This avoids scanning all entities in a project when resolving community membership.
CREATE INDEX entity_merged_ids_index IF NOT EXISTS
FOR (e:`__Entity__`)
ON (e.merged_ids)

