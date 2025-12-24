use code_graph_ingestion_service::plugins::base::{IngestionContext, LanguagePlugin};
use code_graph_ingestion_service::plugins::javascript::plugin::JavaScriptPlugin;
use code_graph_ingestion_service::core::types::{CodeLanguage, CodeRelType};

#[test]
fn javascript_plugin_emits_imports_edges() {
    let tmp = tempfile::tempdir().unwrap();
    let repo = tmp.path().join("repo");
    std::fs::create_dir_all(&repo).unwrap();
    std::fs::write(
        repo.join("a.js"),
        concat!(
            "import x from 'lib-x';\n",
            "const y = require('lib-y');\n",
            "function f() { return 1 }\n",
        ),
    )
    .unwrap();

    let ctx = IngestionContext {
        project_id: "p".to_string(),
        repo_fingerprint: "r".to_string(),
        repo_root: repo.clone(),
        source_language: CodeLanguage::Javascript,
    };
    let plugin = JavaScriptPlugin::new();
    let files = plugin.iter_files(&ctx).unwrap();
    let (nodes, edges, facts) = plugin.ingest(&ctx, &files).unwrap();

    assert!(edges.iter().any(|e| e.rel_type == CodeRelType::Imports));
    assert_eq!(facts.get("js_file_count").and_then(|v| v.as_i64()), Some(1));
    assert!(nodes.iter().any(|n| n.kind == "unresolved" && n.symbol.as_deref() == Some("lib-x")));
    assert!(nodes.iter().any(|n| n.kind == "unresolved" && n.symbol.as_deref() == Some("lib-y")));
}

#[test]
fn javascript_plugin_resolves_relative_import_to_module_node() {
    let tmp = tempfile::tempdir().unwrap();
    let repo = tmp.path().join("repo");
    std::fs::create_dir_all(&repo).unwrap();
    std::fs::write(repo.join("a.js"), "import x from './b';\n").unwrap();
    std::fs::write(repo.join("b.js"), "export const y = 1;\n").unwrap();

    let ctx = IngestionContext {
        project_id: "p".to_string(),
        repo_fingerprint: "r".to_string(),
        repo_root: repo.clone(),
        source_language: CodeLanguage::Javascript,
    };
    let plugin = JavaScriptPlugin::new();
    let files = plugin.iter_files(&ctx).unwrap();
    let (nodes, edges, _facts) = plugin.ingest(&ctx, &files).unwrap();

    let b_module = nodes
        .iter()
        .find(|n| n.kind == "module" && n.file_path == "b.js")
        .expect("b.js module node exists");

    let resolved: Vec<_> = edges
        .iter()
        .filter(|e| {
            e.rel_type == CodeRelType::Imports
                && e.dst_node_id == b_module.node_id
                && e.metadata.get("resolved").and_then(|v| v.as_bool()) == Some(true)
                && e.metadata.get("strategy").and_then(|v| v.as_str()) == Some("import-specifier")
        })
        .collect();

    assert!(!resolved.is_empty(), "Expected relative import './b' to resolve to b.js module node");
}


