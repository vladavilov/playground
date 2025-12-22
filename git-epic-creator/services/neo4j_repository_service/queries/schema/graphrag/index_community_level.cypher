CREATE INDEX community_level IF NOT EXISTS
FOR (c:`__Community__`)
ON (c.level)
