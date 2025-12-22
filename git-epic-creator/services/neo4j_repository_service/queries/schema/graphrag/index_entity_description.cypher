CREATE INDEX entity_description_index IF NOT EXISTS
FOR (e:`__Entity__`)
ON (e.description)
