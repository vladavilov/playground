UNWIND $rows AS value
WITH value, value.project_id AS pid
MERGE (p:__Project__ {id: pid})
MERGE (c:__Community__ {community:value.community, project_id: pid}) 
SET c.id = toString(value.community) + '_' + pid,
    c.level = value.level,
    c.title = value.title,
    c.rank = value.rank,
    c.rating_explanation = value.rating_explanation,
    c.full_content = value.full_content,
    c.summary = value.summary,
    c.full_content_json = value.full_content_json
MERGE (c)-[:IN_PROJECT]->(p)

