CREATE INDEX entity_title_index IF NOT EXISTS
FOR (e:`__Entity__`)
ON (e.title)
