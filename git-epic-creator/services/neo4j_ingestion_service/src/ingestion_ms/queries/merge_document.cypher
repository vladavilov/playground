UNWIND $rows AS value
MERGE (d:__Document__ {id:value.id}) 
SET d += value
// Ensure title is populated: fallback to metadata.file_name if title is empty
WITH d, value
WHERE d.title IS NULL OR d.title = ''
SET d.title = coalesce(
    CASE WHEN value.metadata.file_name IS NOT NULL AND value.metadata.file_name <> '' 
         THEN value.metadata.file_name 
         ELSE NULL END,
    d.id
)
WITH d, value.project_id AS pid
MERGE (p:__Project__ {id: pid})
MERGE (d)-[:IN_PROJECT]->(p)
