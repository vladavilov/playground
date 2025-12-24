use code_graph_ingestion_service::plugins::base::{IngestionContext, LanguagePlugin};
use code_graph_ingestion_service::plugins::registry::{run_selected_plugin, select_plugin};
use code_graph_ingestion_service::core::types::CodeLanguage;
use serde_json::json;
use std::path::PathBuf;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::Arc;

struct TestPlugin {
    name: String,
    ingest_calls: Arc<AtomicUsize>,
}

impl LanguagePlugin for TestPlugin {
    fn name(&self) -> &str {
        &self.name
    }

    fn iter_files(&self, _ctx: &IngestionContext) -> anyhow::Result<Vec<PathBuf>> {
        Ok(vec![])
    }

    fn ingest(
        &self,
        _ctx: &IngestionContext,
        _files: &[PathBuf],
    ) -> anyhow::Result<(Vec<code_graph_ingestion_service::core::records::CodeNodeRecord>, Vec<code_graph_ingestion_service::core::records::EdgeRecord>, serde_json::Value)>
    {
        self.ingest_calls.fetch_add(1, Ordering::SeqCst);
        Ok((vec![], vec![], json!({"ok": true})))
    }
}

#[test]
fn select_plugin_supported_and_unsupported() {
    let ctx = IngestionContext {
        project_id: "p".to_string(),
        repo_fingerprint: "r".to_string(),
        repo_root: PathBuf::from("."),
        source_language: CodeLanguage::Java,
    };

    let p_java: Box<dyn LanguagePlugin> = Box::new(TestPlugin {
        name: "java".to_string(),
        ingest_calls: Arc::new(AtomicUsize::new(0)),
    });
    let p_cobol: Box<dyn LanguagePlugin> = Box::new(TestPlugin {
        name: "cobol".to_string(),
        ingest_calls: Arc::new(AtomicUsize::new(0)),
    });

    let plugins = vec![p_java, p_cobol];
    let sel = select_plugin(&ctx, &plugins).unwrap();
    assert_eq!(sel.name(), "java");

    let bad = IngestionContext {
        source_language: CodeLanguage::Other,
        ..ctx
    };
    assert!(select_plugin(&bad, &plugins).is_err());
}

#[test]
fn run_selected_plugin_executes_exactly_one_plugin() {
    let cobol_calls = Arc::new(AtomicUsize::new(0));
    let java_calls = Arc::new(AtomicUsize::new(0));

    let ctx = IngestionContext {
        project_id: "p".to_string(),
        repo_fingerprint: "r".to_string(),
        repo_root: PathBuf::from("."),
        source_language: CodeLanguage::Cobol,
    };

    let p_java: Box<dyn LanguagePlugin> = Box::new(TestPlugin {
        name: "java".to_string(),
        ingest_calls: java_calls.clone(),
    });
    let p_cobol: Box<dyn LanguagePlugin> = Box::new(TestPlugin {
        name: "cobol".to_string(),
        ingest_calls: cobol_calls.clone(),
    });

    let (nodes, edges, facts) = run_selected_plugin(&ctx, &[p_java, p_cobol]).unwrap();
    assert!(nodes.is_empty());
    assert!(edges.is_empty());
    assert_eq!(facts, json!({"cobol": {"ok": true}}));

    assert_eq!(cobol_calls.load(Ordering::SeqCst), 1);
    assert_eq!(java_calls.load(Ordering::SeqCst), 0);
}


