WITH $rows AS rows
UNWIND rows AS value
WITH value
MERGE (c:__Community__ {community:value.community}) 
SET c += value {.level, .title, .rank, .rating_explanation, .full_content, .summary, .full_content_json} 
WITH c, value, value.project_id AS pid 
MERGE (p:__Project__ {id: pid})
MERGE (c)-[:IN_PROJECT]->(p)

