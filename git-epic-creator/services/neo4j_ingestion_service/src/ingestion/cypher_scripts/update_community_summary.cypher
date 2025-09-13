MATCH (c:__Community__ {id: $community_id})
SET c.summary = $summary, c.summarized_at = datetime()
RETURN c


