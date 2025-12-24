use code_graph_ingestion_service::plugins::base::{IngestionContext, LanguagePlugin};
use code_graph_ingestion_service::plugins::java::plugin::JavaPlugin;
use code_graph_ingestion_service::core::types::{CodeLanguage, CodeRelType};

#[test]
fn java_plugin_emits_class_and_method_and_config_wires() {
    let tmp = tempfile::tempdir().unwrap();
    let repo = tmp.path().join("repo");
    std::fs::create_dir_all(&repo).unwrap();
    std::fs::write(
        repo.join("A.java"),
        concat!(
            "package p;\n",
            "public class A {\n",
            "  void m() {}\n",
            "}\n",
        ),
    )
    .unwrap();
    std::fs::write(repo.join("beans.xml"), "<bean id=\"a\" class=\"p.A\"/>\n").unwrap();

    let ctx = IngestionContext {
        project_id: "p".to_string(),
        repo_fingerprint: "r".to_string(),
        repo_root: repo.clone(),
        source_language: CodeLanguage::Java,
    };
    let plugin = JavaPlugin::new();
    let files = plugin.iter_files(&ctx).unwrap();
    let (nodes, edges, facts) = plugin.ingest(&ctx, &files).unwrap();

    assert!(nodes.iter().any(|n| n.kind == "class" && n.symbol.as_deref() == Some("A")));
    assert!(nodes.iter().any(|n| n.kind == "method" && n.symbol.as_deref() == Some("m")));
    assert!(edges.iter().any(|e| e.rel_type == CodeRelType::ConfigWires));
    assert_eq!(facts.get("java_file_count").and_then(|v| v.as_i64()), Some(1));
    assert_eq!(facts.get("xml_file_count").and_then(|v| v.as_i64()), Some(1));
}

#[test]
fn java_plugin_resolves_imports_to_class_nodes() {
    let tmp = tempfile::tempdir().unwrap();
    let repo = tmp.path().join("repo");
    std::fs::create_dir_all(&repo).unwrap();
    std::fs::write(
        repo.join("A.java"),
        concat!(
            "package p;\n",
            "import p.B;\n",
            "public class A {\n",
            "  void m() {}\n",
            "}\n",
        ),
    )
    .unwrap();
    std::fs::write(repo.join("B.java"), "package p;\npublic class B {}\n").unwrap();

    let ctx = IngestionContext {
        project_id: "p".to_string(),
        repo_fingerprint: "r".to_string(),
        repo_root: repo.clone(),
        source_language: CodeLanguage::Java,
    };
    let plugin = JavaPlugin::new();
    let files = plugin.iter_files(&ctx).unwrap();
    let (nodes, edges, _facts) = plugin.ingest(&ctx, &files).unwrap();

    let b_class = nodes
        .iter()
        .find(|n| n.kind == "class" && n.symbol.as_deref() == Some("B"))
        .expect("B class node exists");

    let resolved_imports: Vec<_> = edges
        .iter()
        .filter(|e| {
            e.rel_type == CodeRelType::Imports
                && e.dst_node_id == b_class.node_id
                && e.metadata.get("resolved").and_then(|v| v.as_bool()) == Some(true)
                && e.metadata.get("strategy").and_then(|v| v.as_str()) == Some("java-import-fqn")
        })
        .collect();

    assert!(!resolved_imports.is_empty(), "Expected import p.B to resolve to class B node");
}


