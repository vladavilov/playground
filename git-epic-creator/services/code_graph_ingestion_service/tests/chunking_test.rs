use code_graph_ingestion_service::core::chunking::chunk_oversized_nodes;
use code_graph_ingestion_service::core::records::CodeNodeRecord;
use code_graph_ingestion_service::core::types::{CodeLanguage, CodeRelType};

#[test]
fn chunking_splits_large_node_and_adds_pipe_edges() {
    let mut text = String::new();
    for i in 0..1205 {
        text.push_str(&format!("line{i}\n"));
    }
    let node = CodeNodeRecord {
        project_id: "p".to_string(),
        repo_fingerprint: "r".to_string(),
        node_id: "N".to_string(),
        language: CodeLanguage::Cobol,
        kind: "program".to_string(),
        symbol: Some("X".to_string()),
        file_path: "x.cbl".to_string(),
        start_line: 1,
        end_line: 1205,
        snippet_hash: "h".to_string(),
        text,
        extra_labels: vec![],
    };

    let res = chunk_oversized_nodes(&[node], 1000, 250, 25);
    assert!(res.nodes.len() > 1);
    assert!(res.edges.iter().all(|e| e.rel_type == CodeRelType::NextChunk));
    assert!(res.edges.iter().all(|e| e.metadata.get("synthetic").and_then(|v| v.as_bool()) == Some(true)));
}


