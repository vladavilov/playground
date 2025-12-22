from core.stable_ids import snippet_hash, stable_node_id


def test_stable_node_id_is_deterministic() -> None:
    a = stable_node_id("p", "r", "file.cbl", "program", "HELLO", "1", "10")
    b = stable_node_id("p", "r", "file.cbl", "program", "HELLO", "1", "10")
    c = stable_node_id("p", "r", "file.cbl", "program", "HELLO", "1", "11")
    assert a == b
    assert a != c
    assert len(a) == 24


def test_snippet_hash_normalizes_newlines_and_trailing_whitespace() -> None:
    h1 = snippet_hash(file_path="a.cbl", start_line=1, end_line=2, text="A  \r\nB\t\r\n")
    h2 = snippet_hash(file_path="a.cbl", start_line=1, end_line=2, text="A\nB\n")
    assert h1 == h2


def test_snippet_hash_changes_on_content_change() -> None:
    h1 = snippet_hash(file_path="a.cbl", start_line=1, end_line=1, text="A\n")
    h2 = snippet_hash(file_path="a.cbl", start_line=1, end_line=1, text="B\n")
    assert h1 != h2


