MATCH (e:__Entity__) 
OPTIONAL MATCH (e)-[r1:RELATED]->() 
WITH e, [x IN collect(DISTINCT r1.id) WHERE x IS NOT NULL] AS out_ids 
OPTIONAL MATCH ()-[r2:RELATED]->(e) 
WITH e, out_ids, [x IN collect(DISTINCT r2.id) WHERE x IS NOT NULL] AS in_ids 
SET e.relationship_ids = out_ids + in_ids
