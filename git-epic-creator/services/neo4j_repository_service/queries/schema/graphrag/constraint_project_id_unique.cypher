CREATE CONSTRAINT project_id_unique_underscored IF NOT EXISTS
FOR (p:`__Project__`)
REQUIRE p.id IS UNIQUE
