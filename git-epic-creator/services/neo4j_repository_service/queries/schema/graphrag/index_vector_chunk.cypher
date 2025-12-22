CREATE VECTOR INDEX graphrag_chunk_index IF NOT EXISTS
FOR (c:`__Chunk__`)
ON (c.embedding)
OPTIONS {indexConfig: {`vector.dimensions`: 3072, `vector.similarity_function`: 'cosine'}}
