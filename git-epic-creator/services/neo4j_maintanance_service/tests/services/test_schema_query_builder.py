from services.schema_query_builder import SchemaQueryBuilder

# --- Initialization ---
def test_query_builder_initialization():
    builder = SchemaQueryBuilder()
    assert builder is not None

# --- Constraint Queries ---
def test_get_constraint_queries_returns_list():
    builder = SchemaQueryBuilder()
    constraints = builder.get_constraint_queries()
    assert isinstance(constraints, list)
    assert len(constraints) > 0

def test_get_constraint_queries_content_and_structure():
    builder = SchemaQueryBuilder()
    constraints = builder.get_constraint_queries()
    constraint_text = " ".join(constraints)
    expected_node_types = ["Entity", "Requirement", "Document", "JiraTicket"]
    for node_type in expected_node_types:
        assert f"FOR (e:{node_type})" in constraint_text or \
               f"FOR (r:{node_type})" in constraint_text or \
               f"FOR (d:{node_type})" in constraint_text or \
               f"FOR (j:{node_type})" in constraint_text
    for constraint in constraints:
        assert constraint.startswith("CREATE CONSTRAINT")
        assert "IF NOT EXISTS" in constraint
        assert "REQUIRE" in constraint
        assert "IS UNIQUE" in constraint

# --- Index Queries ---
def test_get_index_queries_returns_list():
    builder = SchemaQueryBuilder()
    indexes = builder.get_index_queries()
    assert isinstance(indexes, list)
    assert len(indexes) > 0

def test_get_index_queries_content_and_structure():
    builder = SchemaQueryBuilder()
    indexes = builder.get_index_queries()
    index_text = " ".join(indexes)
    assert "requirement_embeddings" in index_text
    assert "entity_embeddings" in index_text
    assert "requirement_text_index" in index_text
    assert "entity_name_index" in index_text
    assert "document_title_index" in index_text
    for index in indexes:
        assert index.startswith("CREATE")
        assert "INDEX" in index
        assert "IF NOT EXISTS" in index

# --- Vector and Text Indexes ---
def test_get_vector_and_text_index_queries():
    builder = SchemaQueryBuilder()
    indexes = builder.get_index_queries()
    vector_indexes = [idx for idx in indexes if "VECTOR INDEX" in idx]
    text_indexes = [idx for idx in indexes if "TEXT INDEX" in idx]
    assert len(vector_indexes) >= 2
    for vector_index in vector_indexes:
        assert "vector.dimensions" in vector_index
        assert "1536" in vector_index
        assert "vector.similarity_function" in vector_index
        assert "cosine" in vector_index
    assert len(text_indexes) >= 3
    for text_index in text_indexes:
        assert "ON (" in text_index
        assert ")" in text_index

# --- All Queries ---
def test_get_all_queries_combines_constraints_and_indexes():
    builder = SchemaQueryBuilder()
    constraints = builder.get_constraint_queries()
    indexes = builder.get_index_queries()
    relationship_types = builder.get_relationship_type_queries()
    all_queries = builder.get_all_queries()
    assert len(all_queries) == len(constraints) + len(indexes) + len(relationship_types)
    for i, constraint in enumerate(constraints):
        assert all_queries[i] == constraint
    for i, index in enumerate(indexes):
        assert all_queries[len(constraints) + i] == index
    for i, rel_type in enumerate(relationship_types):
        assert all_queries[len(constraints) + len(indexes) + i] == rel_type

# --- Node and Relationship Types ---
def test_get_node_and_relationship_types():
    builder = SchemaQueryBuilder()
    node_types = builder.get_node_types()
    expected_types = ["Entity", "Requirement", "Document", "JiraTicket"]
    assert set(node_types) == set(expected_types)
    relationships = builder.get_relationship_types()
    expected_relationships = ["REFERENCED_BY", "EVIDENCED_BY", "MERGED_FROM", "RELATED_TO", "DESCRIBED_IN"]
    assert set(relationships) == set(expected_relationships)

# --- Schema Info ---
def test_get_schema_info_comprehensive():
    builder = SchemaQueryBuilder()
    schema_info = builder.get_schema_info()
    assert "constraints" in schema_info
    assert "indexes" in schema_info
    assert "node_types" in schema_info
    assert "relationships" in schema_info
    assert "vector_dimensions" in schema_info
    assert "similarity_function" in schema_info
    assert schema_info["vector_dimensions"] == 1536
    assert schema_info["similarity_function"] == "cosine"