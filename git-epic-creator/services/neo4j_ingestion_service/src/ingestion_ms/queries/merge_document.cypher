UNWIND $rows AS value
WITH value
MERGE (d:__Document__ {id:value.id}) 
SET d += value
// Ensure title is populated: fallback to metadata.file_name if title is empty
// Use FOREACH to conditionally set title without filtering rows
FOREACH (ignored IN CASE WHEN d.title IS NULL OR d.title = '' THEN [1] ELSE [] END |
  SET d.title = coalesce(
    CASE WHEN value.metadata.file_name IS NOT NULL AND value.metadata.file_name <> '' 
         THEN value.metadata.file_name 
         ELSE NULL END,
    d.id
  )
)
WITH d, value.project_id AS pid
MERGE (p:__Project__ {id: pid})
MERGE (d)-[:IN_PROJECT]->(p)
