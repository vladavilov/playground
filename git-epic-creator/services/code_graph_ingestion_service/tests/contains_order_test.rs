use code_graph_ingestion_service::core::records::CodeNodeRecord;
use code_graph_ingestion_service::core::structure::build_contains_edges;
use code_graph_ingestion_service::core::types::{CodeLanguage, CodeRelType};

#[test]
fn contains_edges_include_deterministic_sibling_order() {
    let parent = CodeNodeRecord {
        project_id: "p".to_string(),
        repo_fingerprint: "r".to_string(),
        node_id: "parent".to_string(),
        language: CodeLanguage::Cobol,
        kind: "program".to_string(),
        symbol: None,
        file_path: "a.cbl".to_string(),
        start_line: 1,
        end_line: 10,
        snippet_hash: "h1".to_string(),
        text: "x\n".to_string(),
        extra_labels: vec![],
    };
    let child1 = CodeNodeRecord {
        project_id: "p".to_string(),
        repo_fingerprint: "r".to_string(),
        node_id: "c1".to_string(),
        language: CodeLanguage::Cobol,
        kind: "paragraph".to_string(),
        symbol: None,
        file_path: "a.cbl".to_string(),
        start_line: 2,
        end_line: 3,
        snippet_hash: "h2".to_string(),
        text: "x\n".to_string(),
        extra_labels: vec![],
    };
    let child2 = CodeNodeRecord {
        project_id: "p".to_string(),
        repo_fingerprint: "r".to_string(),
        node_id: "c2".to_string(),
        language: CodeLanguage::Cobol,
        kind: "paragraph".to_string(),
        symbol: None,
        file_path: "a.cbl".to_string(),
        start_line: 4,
        end_line: 6,
        snippet_hash: "h3".to_string(),
        text: "x\n".to_string(),
        extra_labels: vec![],
    };
    let grandchild = CodeNodeRecord {
        project_id: "p".to_string(),
        repo_fingerprint: "r".to_string(),
        node_id: "gc".to_string(),
        language: CodeLanguage::Cobol,
        kind: "statement".to_string(),
        symbol: None,
        file_path: "a.cbl".to_string(),
        start_line: 4,
        end_line: 4,
        snippet_hash: "h4".to_string(),
        text: "x\n".to_string(),
        extra_labels: vec![],
    };

    let edges = build_contains_edges(&[child2.clone(), grandchild.clone(), child1.clone(), parent.clone()]);
    let mut got: Vec<(String, String, i64)> = edges
        .into_iter()
        .filter(|e| e.rel_type == CodeRelType::Contains)
        .map(|e| {
            let order = e.metadata.get("order").and_then(|v| v.as_i64()).unwrap_or(-1);
            (e.src_node_id, e.dst_node_id, order)
        })
        .collect();
    got.sort();

    // parent has two direct children ordered by source position (start_line, -end_line, node_id)
    // child2 has one direct child
    assert_eq!(
        got,
        vec![
            ("c2".to_string(), "gc".to_string(), 1),
            ("parent".to_string(), "c1".to_string(), 1),
            ("parent".to_string(), "c2".to_string(), 2),
        ]
    );
}


