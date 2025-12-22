CREATE INDEX entity_norm_title_index IF NOT EXISTS
FOR (e:`__Entity__`)
ON (e.norm_title)
