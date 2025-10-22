// Update single community embedding
MATCH (c:__Community__ {community: $community_id, project_id: $project_id})
SET c.embedding = $embedding
RETURN c.community AS updated_id

