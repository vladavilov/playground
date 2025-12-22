CREATE FULLTEXT INDEX community_summary_fts IF NOT EXISTS
FOR (c:`__Community__`)
ON EACH [c.summary]
