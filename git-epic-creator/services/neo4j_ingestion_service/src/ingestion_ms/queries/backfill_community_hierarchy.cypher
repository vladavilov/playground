// Backfill missing community hierarchy relationships
MATCH (p:__Project__ {id: $project_id})
MATCH (child:__Community__)-[:IN_PROJECT]->(p)
WHERE child.level IS NOT NULL AND child.level > 0
WITH p, child, child.level AS child_level
MATCH (parent:__Community__)-[:IN_PROJECT]->(p)
WHERE parent.level = child_level - 1
  AND parent.project_id = child.project_id
WITH child, parent, child.entity_ids AS child_eids, parent.entity_ids AS parent_eids
WHERE size([eid IN child_eids WHERE eid IN parent_eids | 1]) > 0
MERGE (child)-[:IN_COMMUNITY]->(parent)
RETURN count(*) AS hierarchy_links_created

