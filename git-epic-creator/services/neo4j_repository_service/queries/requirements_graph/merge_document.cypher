UNWIND $rows AS value
MERGE (d:__Document__ {id:value.id}) 
SET d += value
// Ensure title is populated: fallback to metadata.file_name if title is empty
WITH d, value
SET d.title = CASE 
  WHEN d.title IS NULL OR d.title = '' 
  THEN coalesce(
    CASE WHEN value.metadata.file_name IS NOT NULL AND value.metadata.file_name <> '' 
         THEN value.metadata.file_name 
         ELSE NULL END,
    d.id
  )
  ELSE d.title
END
WITH d, value.project_id AS pid
MERGE (p:__Project__ {id: pid})
MERGE (d)-[:IN_PROJECT]->(p)
WITH d
RETURN count(DISTINCT d) AS documents_created
