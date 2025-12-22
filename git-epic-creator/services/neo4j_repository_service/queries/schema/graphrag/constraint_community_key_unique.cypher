CREATE CONSTRAINT community_key_unique_underscored IF NOT EXISTS
FOR (c:`__Community__`)
REQUIRE (c.project_id, c.community) IS UNIQUE
