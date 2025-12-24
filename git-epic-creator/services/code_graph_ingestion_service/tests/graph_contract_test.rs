use code_graph_ingestion_service::core::graph_contract::validate_graph_contract;
use code_graph_ingestion_service::core::inventory::InventoryEntry;
use code_graph_ingestion_service::core::records::{CodeNodeRecord, EdgeRecord};
use code_graph_ingestion_service::core::types::{CodeLanguage, CodeRelType};

#[test]
fn graph_contract_rejects_includes_to_missing_file() {
    let repo_root = tempfile::tempdir().unwrap();
    let files = vec![InventoryEntry {
        path: "a.cpy".to_string(),
        sha256: "x".to_string(),
        language: CodeLanguage::Cobol,
        line_count: 1,
    }, InventoryEntry {
        path: "a.cbl".to_string(),
        sha256: "x".to_string(),
        language: CodeLanguage::Cobol,
        line_count: 1,
    }];
    let nodes = vec![CodeNodeRecord {
        project_id: "p".to_string(),
        repo_fingerprint: "r".to_string(),
        node_id: "n1".to_string(),
        language: CodeLanguage::Cobol,
        kind: "program".to_string(),
        symbol: None,
        file_path: "a.cbl".to_string(),
        start_line: 1,
        end_line: 1,
        snippet_hash: "h".to_string(),
        text: "x\n".to_string(),
        extra_labels: vec![],
    }];
    let edges = vec![EdgeRecord {
        project_id: "p".to_string(),
        repo_fingerprint: "r".to_string(),
        rel_type: CodeRelType::Includes,
        src_node_id: "n1".to_string(),
        dst_node_id: "missing.cpy".to_string(),
        confidence: 1.0,
        metadata: serde_json::Map::new(),
    }];

    let err = validate_graph_contract("p", "r", repo_root.path(), &files, &nodes, &edges).unwrap_err();
    assert!(err
        .to_string()
        .contains("INCLUDES dst must exist in inventory or on disk"));
}

#[test]
fn graph_contract_rejects_edge_with_missing_endpoint_node() {
    let repo_root = tempfile::tempdir().unwrap();
    let files = vec![InventoryEntry {
        path: "a.js".to_string(),
        sha256: "x".to_string(),
        language: CodeLanguage::Javascript,
        line_count: 1,
    }];
    let nodes = vec![CodeNodeRecord {
        project_id: "p".to_string(),
        repo_fingerprint: "r".to_string(),
        node_id: "n1".to_string(),
        language: CodeLanguage::Javascript,
        kind: "module".to_string(),
        symbol: None,
        file_path: "a.js".to_string(),
        start_line: 1,
        end_line: 1,
        snippet_hash: "h".to_string(),
        text: "x\n".to_string(),
        extra_labels: vec![],
    }];
    let edges = vec![EdgeRecord {
        project_id: "p".to_string(),
        repo_fingerprint: "r".to_string(),
        rel_type: CodeRelType::Calls,
        src_node_id: "n1".to_string(),
        dst_node_id: "missing".to_string(),
        confidence: 0.5,
        metadata: serde_json::Map::new(),
    }];
    let err = validate_graph_contract("p", "r", repo_root.path(), &files, &nodes, &edges).unwrap_err();
    assert!(err.to_string().contains("Edge dst_node_id not found"));
}

#[test]
fn graph_contract_rejects_node_span_out_of_bounds() {
    let repo_root = tempfile::tempdir().unwrap();
    let files = vec![InventoryEntry {
        path: "a.java".to_string(),
        sha256: "x".to_string(),
        language: CodeLanguage::Java,
        line_count: 3,
    }];
    let nodes = vec![CodeNodeRecord {
        project_id: "p".to_string(),
        repo_fingerprint: "r".to_string(),
        node_id: "n1".to_string(),
        language: CodeLanguage::Java,
        kind: "module".to_string(),
        symbol: None,
        file_path: "a.java".to_string(),
        start_line: 1,
        end_line: 5,
        snippet_hash: "h".to_string(),
        text: "x\n".to_string(),
        extra_labels: vec![],
    }];
    let edges: Vec<EdgeRecord> = vec![];
    let err = validate_graph_contract("p", "r", repo_root.path(), &files, &nodes, &edges).unwrap_err();
    assert!(err.to_string().contains("Node end_line out of bounds"));
}


