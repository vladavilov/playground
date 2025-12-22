// Backfill missing community hierarchy relationships
// Uses entity_ids overlap as primary matching, relationship_ids as fallback
MATCH (p:__Project__ {id: $project_id})
MATCH (child:__Community__)-[:IN_PROJECT]->(p)
WHERE child.level IS NOT NULL AND child.level > 0
WITH p, child, child.level AS child_level
MATCH (parent:__Community__)-[:IN_PROJECT]->(p)
WHERE parent.level = child_level - 1
  AND parent.project_id = child.project_id

// Collect all potential matching criteria
WITH child, parent, 
     coalesce(child.entity_ids, []) AS child_eids, 
     coalesce(parent.entity_ids, []) AS parent_eids,
     coalesce(child.relationship_ids, []) AS child_rids,
     coalesce(parent.relationship_ids, []) AS parent_rids

// Match if entities overlap OR relationships overlap (robust fallback)
WHERE (size(child_eids) > 0 AND size(parent_eids) > 0 AND 
       size([eid IN child_eids WHERE eid IN parent_eids | 1]) > 0)
   OR (size(child_rids) > 0 AND size(parent_rids) > 0 AND 
       size([rid IN child_rids WHERE rid IN parent_rids | 1]) > 0)

MERGE (child)-[:IN_COMMUNITY]->(parent)
RETURN count(*) AS hierarchy_links_created
