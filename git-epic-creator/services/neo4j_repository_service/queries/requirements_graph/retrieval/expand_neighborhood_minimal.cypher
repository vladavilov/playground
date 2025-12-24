UNWIND $chunkIds AS cid
MATCH (p:__Project__ {id: $projectId})
MATCH (ch:__Chunk__)-[:IN_PROJECT]->(p) WHERE ch.id = cid
// Get document information for the chunk
OPTIONAL MATCH (d:__Document__)-[:HAS_CHUNK]->(ch)
// Get entities from current chunk
OPTIONAL MATCH (ch)-[:HAS_ENTITY]->(e:__Entity__)-[:IN_PROJECT]->(p)
// Get related entities and their relationships
OPTIONAL MATCH (e)-[r:RELATED]->(re:__Entity__)-[:IN_PROJECT]->(p)
// Get chunks containing related entities
OPTIONAL MATCH (ch2:__Chunk__)-[:HAS_ENTITY]->(re)
WHERE ch2 <> ch AND (ch2)-[:IN_PROJECT]->(p)
WITH cid, ch, d,
collect(DISTINCT { _id: e.id, properties: {
title: e.title, description: e.description, type: coalesce(e.type, ''),
text_unit_ids: e.text_unit_ids } })[0..2] AS neighbours,
collect(DISTINCT { _id: re.id, properties: {
title: re.title, description: re.description, type: coalesce(re.type, ''),
text_unit_ids: re.text_unit_ids } })[0..3] AS related_entities,
collect(DISTINCT { type: type(r), description: r.description })[0..5] AS relationships,
collect(DISTINCT ch2.id)[0..3] AS neighbor_chunk_ids
RETURN cid AS chunk_id,
substring(ch.text, 0, $maxChunkLen) AS text,
coalesce(d.title, d.id, 'unknown') AS document_name,
neighbours, related_entities, relationships, neighbor_chunk_ids



