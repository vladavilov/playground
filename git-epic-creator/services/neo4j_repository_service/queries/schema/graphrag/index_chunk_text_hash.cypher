CREATE INDEX chunk_text_hash_index IF NOT EXISTS
FOR (c:`__Chunk__`)
ON (c.text_hash)
