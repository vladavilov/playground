use crate::core::chunking::chunk_oversized_nodes;
use crate::core::fingerprint::{fingerprint_for_git_head_commit, fingerprint_for_zip_bytes};
use crate::core::graph_contract::validate_graph_contract;
use crate::core::ignore_rules::build_ignore_rules;
use crate::core::inventory::build_inventory;
use crate::core::repo_materializer::{materialize_git, materialize_zip_bytes};
use crate::core::structure::build_contains_edges;
use crate::core::types::CodeLanguage;
use crate::core::workspace::Workspace;
use crate::persistence::neo4j_repository_service_client::persist_code_graph;
use crate::persistence::repo_index_store::upsert_repo_index_sync;
use crate::plugins::base::{IngestionContext, LanguagePlugin};
use crate::plugins::cobol::plugin::CobolPlugin;
use crate::plugins::java::plugin::JavaPlugin;
use crate::plugins::javascript::plugin::JavaScriptPlugin;
use crate::plugins::registry::run_selected_plugin;
use bytes::Bytes;
use chrono::Utc;
use serde_json::json;
use uuid::Uuid;

#[derive(Debug, Clone)]
pub struct IngestOutcome {
    pub project_id: Uuid,
    pub repo_fingerprint: String,
}

#[derive(Debug, Clone)]
pub struct OrchestratorConfig {
    pub workspace_root: std::path::PathBuf,
}

impl Default for OrchestratorConfig {
    fn default() -> Self {
        let root = std::env::var("WORKSPACE_ROOT").unwrap_or_else(|_| "workspace".to_string());
        Self {
            workspace_root: std::path::PathBuf::from(root),
        }
    }
}

fn default_plugins() -> Vec<Box<dyn LanguagePlugin>> {
    vec![
        Box::new(CobolPlugin::new()),
        Box::new(JavaPlugin::new()),
        Box::new(JavaScriptPlugin::new()),
    ]
}

fn run_ingestion_pipeline(
    project_id: Uuid,
    source_language: CodeLanguage,
    repo_root: std::path::PathBuf,
    repo_fingerprint: String,
    repo_auth_header: Option<String>,
) -> anyhow::Result<IngestOutcome> {
    let ignore = build_ignore_rules(&repo_root, None)?;
    let files = build_inventory(&repo_root, Some(&ignore))?;

    let ctx = IngestionContext {
        project_id: project_id.to_string(),
        repo_fingerprint: repo_fingerprint.clone(),
        repo_root: repo_root.clone(),
        source_language,
    };

    let plugins = default_plugins();
    let (nodes, mut edges, facts) = run_selected_plugin(&ctx, &plugins)?;

    let chunked = chunk_oversized_nodes(&nodes, 1000, 250, 25);
    let nodes = chunked.nodes;
    edges.extend(chunked.edges);
    edges.extend(build_contains_edges(&nodes));

    validate_graph_contract(
        &ctx.project_id,
        &ctx.repo_fingerprint,
        &ctx.repo_root,
        &files,
        &nodes,
        &edges,
    )?;

    persist_code_graph(
        &ctx.project_id,
        &ctx.repo_fingerprint,
        &files,
        &nodes,
        &edges,
        repo_auth_header.as_deref(),
    )?;
    let repo_index_json = json!({
        "project_id": ctx.project_id,
        "repo_fingerprint": ctx.repo_fingerprint,
        "created_at": Utc::now().to_rfc3339(),
        "files": files,
        "facts": facts,
    });
    let _ = upsert_repo_index_sync(
        &database_url,
        project_id,
        &repo_fingerprint,
        &repo_index_json,
    )?;
    
    Ok(IngestOutcome {
        project_id,
        repo_fingerprint,
    })
}

pub fn ingest_zip(
    cfg: &OrchestratorConfig,
    project_id: Uuid,
    source_language: CodeLanguage,
    zip_bytes: Bytes,
    repo_auth_header: Option<String>,
) -> anyhow::Result<IngestOutcome> {
    let ws = Workspace::new(&cfg.workspace_root);
    let repo_root = ws.zip_repo_dir(&project_id.to_string());
    materialize_zip_bytes(zip_bytes.as_ref(), &repo_root).map_err(|e| anyhow::anyhow!(e))?;

    let fp = fingerprint_for_zip_bytes(zip_bytes.as_ref())?;
    run_ingestion_pipeline(
        project_id,
        source_language,
        repo_root,
        fp.value,
        repo_auth_header,
    )
}

pub fn ingest_git(
    cfg: &OrchestratorConfig,
    project_id: Uuid,
    source_language: CodeLanguage,
    git_url: &str,
    ref_name: Option<&str>,
    repo_auth_header: Option<String>,
) -> anyhow::Result<IngestOutcome> {
    let ws = Workspace::new(&cfg.workspace_root);
    let repo_root = ws.git_repo_dir(&project_id.to_string(), git_url, ref_name);
    let res = materialize_git(git_url, ref_name, &repo_root).map_err(|e| anyhow::anyhow!(e))?;
    let head = res.head_commit.ok_or_else(|| {
        anyhow::anyhow!("Could not determine HEAD commit for git materialization")
    })?;

    let fp = fingerprint_for_git_head_commit(&head);
    run_ingestion_pipeline(
        project_id,
        source_language,
        repo_root,
        fp.value,
        repo_auth_header,
    )
}
