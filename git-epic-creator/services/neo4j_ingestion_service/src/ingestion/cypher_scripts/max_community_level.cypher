MATCH (c:__Community__) RETURN coalesce(max(c.level), 0) AS max_level


