// Backfill community.id for existing data
MATCH (c:__Community__)
WHERE c.id IS NULL OR c.id = ''
SET c.id = toString(c.community) + '_' + c.project_id
RETURN count(c) AS communities_updated
