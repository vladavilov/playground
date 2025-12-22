from core.chunking import chunk_oversized_nodes
from core.records import CodeNodeRecord


def test_chunking_splits_large_node_and_adds_pipe_edges() -> None:
    text = "".join(f"line{i}\n" for i in range(1205))
    node = CodeNodeRecord(
        project_id="p",
        repo_fingerprint="r",
        node_id="N",
        language="cobol",
        kind="program",
        symbol="X",
        file_path="x.cbl",
        start_line=1,
        end_line=1205,
        snippet_hash="h",
        text=text,
    )
    res = chunk_oversized_nodes([node], max_lines=1000, chunk_size=250, overlap=25)
    assert len(res.nodes) > 1
    assert all(e.rel_type == "CALLS" for e in res.edges)
    assert all(e.metadata.get("pipe") for e in res.edges)

