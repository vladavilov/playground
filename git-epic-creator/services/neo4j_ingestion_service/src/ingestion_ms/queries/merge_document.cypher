UNWIND $rows AS value
WITH value
MERGE (d:__Document__ {id:value.id}) SET d += value
WITH d, value.project_id AS pid
MERGE (p:__Project__ {id: pid})
MERGE (d)-[:IN_PROJECT]->(p)
