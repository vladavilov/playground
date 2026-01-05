CREATE CONSTRAINT entity_id_unique_underscored IF NOT EXISTS
FOR (e:`__Entity__`)
REQUIRE (e.project_id, e.id) IS UNIQUE
