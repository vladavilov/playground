UNWIND $rows AS value
WITH value
MERGE (d:__Document__ {id:value.id}) SET d += value
