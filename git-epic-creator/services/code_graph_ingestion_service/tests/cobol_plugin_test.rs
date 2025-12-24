use code_graph_ingestion_service::plugins::base::IngestionContext;
use code_graph_ingestion_service::plugins::cobol::plugin::CobolPlugin;
use code_graph_ingestion_service::plugins::base::LanguagePlugin;
use code_graph_ingestion_service::core::types::{CodeLanguage, CodeRelType};

#[test]
fn cobol_plugin_emits_program_node_and_includes_edge() {
    let tmp = tempfile::tempdir().unwrap();
    let repo = tmp.path().join("repo");
    std::fs::create_dir_all(&repo).unwrap();
    std::fs::write(
        repo.join("MAIN.cbl"),
        concat!(
            "       IDENTIFICATION DIVISION.\n",
            "       PROGRAM-ID. HELLO.\n",
            "       DATA DIVISION.\n",
            "       WORKING-STORAGE SECTION.\n",
            "       COPY FOO.\n",
            "       PROCEDURE DIVISION.\n",
            "       CALL \"X\".\n",
            "       STOP RUN.\n",
        ),
    )
    .unwrap();
    std::fs::write(repo.join("FOO.cpy"), "01 X PIC 9.\n").unwrap();

    let ctx = IngestionContext {
        project_id: "p".to_string(),
        repo_fingerprint: "r".to_string(),
        repo_root: repo.clone(),
        source_language: CodeLanguage::Cobol,
    };
    let plugin = CobolPlugin::new();
    let files = plugin.iter_files(&ctx).unwrap();
    let (nodes, edges, facts) = plugin.ingest(&ctx, &files).unwrap();

    assert!(nodes.iter().any(|n| n.kind == "program"));
    assert!(nodes.iter().any(|n| n.kind == "copybook"), "expected copybook unit node from COPY expansion provenance");
    assert!(edges.iter().any(|e| e.rel_type == CodeRelType::Includes));
    assert_eq!(facts.get("includes_count").and_then(|v| v.as_i64()), Some(1));
}

#[test]
fn cobol_plugin_resolves_literal_calls_to_program_nodes() {
    let tmp = tempfile::tempdir().unwrap();
    let repo = tmp.path().join("repo");
    std::fs::create_dir_all(&repo).unwrap();

    std::fs::write(
        repo.join("MAIN.cbl"),
        concat!(
            "       IDENTIFICATION DIVISION.\n",
            "       PROGRAM-ID. MAIN.\n",
            "       PROCEDURE DIVISION.\n",
            "       CALL \"CALLEE\".\n",
            "       STOP RUN.\n",
        ),
    )
    .unwrap();
    std::fs::write(
        repo.join("CALLEE.cbl"),
        concat!(
            "       IDENTIFICATION DIVISION.\n",
            "       PROGRAM-ID. CALLEE.\n",
            "       PROCEDURE DIVISION.\n",
            "       STOP RUN.\n",
        ),
    )
    .unwrap();

    let ctx = IngestionContext {
        project_id: "p".to_string(),
        repo_fingerprint: "r".to_string(),
        repo_root: repo.clone(),
        source_language: CodeLanguage::Cobol,
    };
    let plugin = CobolPlugin::new();
    let files = plugin.iter_files(&ctx).unwrap();
    let (nodes, edges, _facts) = plugin.ingest(&ctx, &files).unwrap();

    let callee_program = nodes
        .iter()
        .find(|n| n.kind == "program" && n.symbol.as_deref() == Some("CALLEE"))
        .expect("callee program node exists");

    let resolved_calls: Vec<_> = edges
        .iter()
        .filter(|e| {
            e.rel_type == CodeRelType::Calls
                && e.dst_node_id == callee_program.node_id
                && e.metadata.get("resolved").and_then(|v| v.as_bool()) == Some(true)
                && e.metadata.get("strategy").and_then(|v| v.as_str()) == Some("program-id")
        })
        .collect();

    assert!(!resolved_calls.is_empty(), "Expected literal CALL to be resolved to program node");
}


