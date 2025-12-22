CREATE CONSTRAINT document_id_unique_underscored IF NOT EXISTS
FOR (d:`__Document__`)
REQUIRE d.id IS UNIQUE
