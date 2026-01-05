CREATE CONSTRAINT document_id_unique_underscored IF NOT EXISTS
FOR (d:`__Document__`)
REQUIRE (d.project_id, d.id) IS UNIQUE
