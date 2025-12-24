use code_graph_ingestion_service::plugins::base::{IngestionContext, LanguagePlugin};
use code_graph_ingestion_service::plugins::java::plugin::JavaPlugin;
use code_graph_ingestion_service::plugins::javascript::plugin::JavaScriptPlugin;
use code_graph_ingestion_service::core::types::{CodeLanguage, CodeRelType};

#[test]
fn javascript_tsg_extracts_export_from_and_dynamic_import() {
    let tmp = tempfile::tempdir().unwrap();
    let repo = tmp.path().join("repo");
    std::fs::create_dir_all(&repo).unwrap();
    std::fs::write(
        repo.join("a.js"),
        concat!(
            "export { x } from 'lib-e';\n",
            "const m = import('lib-d');\n",
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
    let (nodes, edges, _facts) = plugin.ingest(&ctx, &files).unwrap();

    assert!(nodes.iter().any(|n| n.kind == "unresolved" && n.symbol.as_deref() == Some("lib-e")));
    assert!(nodes.iter().any(|n| n.kind == "unresolved" && n.symbol.as_deref() == Some("lib-d")));

    assert!(edges.iter().any(|e| {
        e.rel_type == CodeRelType::Imports
            && e.metadata.get("kind").and_then(|v| v.as_str()) == Some("export_from")
            && e.metadata.get("specifier").and_then(|v| v.as_str()) == Some("lib-e")
    }));
    assert!(edges.iter().any(|e| {
        e.rel_type == CodeRelType::Imports
            && e.metadata.get("kind").and_then(|v| v.as_str()) == Some("dynamic_import")
            && e.metadata.get("specifier").and_then(|v| v.as_str()) == Some("lib-d")
    }));
}

#[test]
fn java_tsg_extracts_static_and_wildcard_imports() {
    let tmp = tempfile::tempdir().unwrap();
    let repo = tmp.path().join("repo");
    std::fs::create_dir_all(&repo).unwrap();
    std::fs::write(
        repo.join("A.java"),
        concat!(
            "package p;\n",
            "import static p.B.C;\n",
            "import p.util.*;\n",
            "public class A {}\n",
        ),
    )
    .unwrap();

    let ctx = IngestionContext {
        project_id: "p".to_string(),
        repo_fingerprint: "r".to_string(),
        repo_root: repo.clone(),
        source_language: CodeLanguage::Java,
    };
    let plugin = JavaPlugin::new();
    let files = plugin.iter_files(&ctx).unwrap();
    let (nodes, edges, _facts) = plugin.ingest(&ctx, &files).unwrap();

    // Static import should drop trailing member, matching previous behavior.
    assert!(nodes
        .iter()
        .any(|n| n.kind == "unresolved" && n.symbol.as_deref() == Some("p.B")));

    // Wildcard import should remain as-is.
    assert!(nodes
        .iter()
        .any(|n| n.kind == "unresolved" && n.symbol.as_deref() == Some("p.util.*")));

    assert!(edges.iter().any(|e| {
        e.rel_type == CodeRelType::Imports
            && e.metadata.get("import").and_then(|v| v.as_str()) == Some("p.B")
    }));
    assert!(edges.iter().any(|e| {
        e.rel_type == CodeRelType::Imports
            && e.metadata.get("import").and_then(|v| v.as_str()) == Some("p.util.*")
    }));
}


