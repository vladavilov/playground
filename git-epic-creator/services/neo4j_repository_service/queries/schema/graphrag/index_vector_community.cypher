CREATE VECTOR INDEX graphrag_comm_index IF NOT EXISTS
FOR (c:`__Community__`)
ON (c.embedding)
OPTIONS {indexConfig: {`vector.dimensions`: 3072, `vector.similarity_function`: 'cosine'}}
