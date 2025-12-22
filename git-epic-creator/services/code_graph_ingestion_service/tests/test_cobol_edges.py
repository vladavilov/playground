from plugins.cobol.edges import extract_cobol_edges


def test_edges_extracts_literal_and_dynamic_calls_and_perform() -> None:
    lines = [
        "       IDENTIFICATION DIVISION.",
        "       PROGRAM-ID. HELLO.",
        '       CALL "FOO".',
        "       CALL WS-PGM.",
        "       PERFORM INIT-ROUTINE.",
    ]

    res = extract_cobol_edges(
        project_id="p",
        repo_fingerprint="r",
        file_path="x.cbl",
        program_node_id="prog",
        physical_lines=lines,
    )
    rels = {e.rel_type for e in res.edges}
    assert "CALLS" in rels
    assert "PERFORMS" in rels
    # unresolved nodes created
    assert any(n.kind == "unresolved" for n in res.nodes)
    assert any("__UnresolvedCall__" in n.extra_labels for n in res.nodes)



