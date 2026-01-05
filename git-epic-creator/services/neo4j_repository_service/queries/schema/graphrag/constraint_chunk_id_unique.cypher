CREATE CONSTRAINT chunk_id_unique_underscored IF NOT EXISTS
FOR (c:`__Chunk__`)
REQUIRE (c.project_id, c.id) IS UNIQUE
