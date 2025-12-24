use code_graph_ingestion_service::core::stable_ids::{snippet_hash, stable_node_id};

#[test]
fn stable_node_id_is_deterministic() {
    let a = stable_node_id(["p", "r", "file.cbl", "program", "HELLO", "1", "10"]);
    let b = stable_node_id(["p", "r", "file.cbl", "program", "HELLO", "1", "10"]);
    let c = stable_node_id(["p", "r", "file.cbl", "program", "HELLO", "1", "11"]);
    assert_eq!(a, b);
    assert_ne!(a, c);
    assert_eq!(a.len(), 24);
}

#[test]
fn snippet_hash_normalizes_newlines_and_trailing_whitespace() {
    let h1 = snippet_hash("a.cbl", 1, 2, "A  \r\nB\t\r\n");
    let h2 = snippet_hash("a.cbl", 1, 2, "A\nB\n");
    assert_eq!(h1, h2);
}

#[test]
fn snippet_hash_changes_on_content_change() {
    let h1 = snippet_hash("a.cbl", 1, 1, "A\n");
    let h2 = snippet_hash("a.cbl", 1, 1, "B\n");
    assert_ne!(h1, h2);
}


