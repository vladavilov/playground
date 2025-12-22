CREATE VECTOR INDEX graphrag_entity_index IF NOT EXISTS
FOR (e:`__Entity__`)
ON (e.embedding)
OPTIONS {indexConfig: {`vector.dimensions`: 3072, `vector.similarity_function`: 'cosine'}}
