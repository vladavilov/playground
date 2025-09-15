UNWIND $rows AS value
WITH value
MERGE (c:__Community__ {community:value.community}) 
SET c += value {.level, .title, .rank, .rating_explanation, .full_content, .summary, .full_content_json} 
WITH c, value 
WITH c, value, range(0, coalesce(size(value.findings), 0)-1) AS idxs 
UNWIND idxs AS finding_idx 
WITH c, value, finding_idx, value.findings[finding_idx] AS finding 
MERGE (c)-[:HAS_FINDING]->(f:Finding {id:finding_idx}) 
SET f += finding
