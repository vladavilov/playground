from persistence.neo4j_writer import (
    build_merge_code_nodes_query,
    build_merge_edges_query,
    build_merge_files_query,
    build_merge_project_query,
    build_merge_repo_query,
)


def test_code_node_query_has_required_merge_keys_and_props() -> None:
    q = build_merge_code_nodes_query()
    assert "MERGE (n:__CodeNode__" in q
    assert "project_id: row.project_id" in q
    assert "repo_fingerprint: row.repo_fingerprint" in q
    assert "node_id: row.node_id" in q
    assert "SET n +=" in q
    assert "row.props" in q
    assert "IN_PROJECT" in q


def test_edge_query_includes_required_properties() -> None:
    q = build_merge_edges_query("CALLS")
    assert "MERGE (src)-[r:CALLS" in q
    assert "project_id: row.project_id" in q
    assert "repo_fingerprint: row.repo_fingerprint" in q
    assert "SET r.confidence = row.confidence" in q
    assert "r.metadata = row.metadata" in q


def test_includes_edge_targets_file_nodes() -> None:
    q = build_merge_edges_query("INCLUDES")
    assert "MATCH (dst:__File__" in q
    assert "file_path: row.dst_node_id" in q


def test_file_and_repo_queries_have_project_scoping() -> None:
    assert "MERGE (p:__Project__" in build_merge_project_query()
    assert "IN_PROJECT" in build_merge_repo_query()
    assert "IN_PROJECT" in build_merge_files_query()


