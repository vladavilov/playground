use crate::core::inventory::InventoryEntry;
use crate::core::records::{CodeNodeRecord, EdgeRecord};
use crate::core::types::CodeRelType;
use reqwest::blocking::Client;
use serde::Serialize;
use std::collections::BTreeMap;

fn base_url() -> String {
    std::env::var("NEO4J_REPOSITORY_SERVICE_URL")
        .unwrap_or_else(|_| "http://neo4j-repository-service:8080".to_string())
        .trim_end_matches('/')
        .to_string()
}

fn client() -> Client {
    Client::builder()
        .timeout(std::time::Duration::from_secs(
            std::env::var("NEO4J_REPOSITORY_TIMEOUT_S")
                .ok()
                .and_then(|s| s.parse::<u64>().ok())
                .unwrap_or(60),
        ))
        .build()
        .expect("reqwest client")
}

fn post_json<T: Serialize>(path: &str, payload: &T) -> anyhow::Result<()> {
    let url = format!("{}{}", base_url(), path);
    let resp = client().post(url).json(payload).send()?;
    if !resp.status().is_success() {
        anyhow::bail!("neo4j-repository-service returned {}", resp.status());
    }
    Ok(())
}

#[derive(Debug, Serialize)]
struct MergeProjectRequest<'a> {
    project_id: &'a str,
}

#[derive(Debug, Serialize)]
struct MergeRepoRequest<'a> {
    project_id: &'a str,
    repo_fingerprint: &'a str,
}

#[derive(Debug, Serialize)]
struct MergeRowsRequest<T> {
    rows: Vec<T>,
}

#[derive(Debug, Serialize)]
struct FileRow<'a> {
    project_id: &'a str,
    repo_fingerprint: &'a str,
    file_path: &'a str,
    sha256: &'a str,
    language: &'a str,
    line_count: i64,
}

#[derive(Debug, Serialize)]
struct CodeNodeRow<'a> {
    project_id: &'a str,
    repo_fingerprint: &'a str,
    node_id: &'a str,
    props: serde_json::Value,
    extra_labels: Vec<&'a str>,
}

#[derive(Debug, Serialize)]
struct EdgeRow<'a> {
    project_id: &'a str,
    repo_fingerprint: &'a str,
    src_node_id: &'a str,
    dst_node_id: &'a str,
    confidence: f64,
    metadata: serde_json::Value,
}

#[derive(Debug, Serialize)]
struct MergeCodeGraphRequest<'a> {
    nodes: Vec<CodeNodeRow<'a>>,
    edges: BTreeMap<CodeRelType, Vec<EdgeRow<'a>>>,
}

pub fn persist_code_graph(
    project_id: &str,
    repo_fingerprint: &str,
    files: &[InventoryEntry],
    nodes: &[CodeNodeRecord],
    edges: &[EdgeRecord],
) -> anyhow::Result<()> {
    post_json("/v1/code-graph/merge-project", &MergeProjectRequest { project_id })?;
    post_json(
        "/v1/code-graph/merge-repo",
        &MergeRepoRequest {
            project_id,
            repo_fingerprint,
        },
    )?;

    let file_rows: Vec<FileRow<'_>> = files
        .iter()
        .map(|f| FileRow {
            project_id,
            repo_fingerprint,
            file_path: &f.path,
            sha256: &f.sha256,
            language: f.language.as_str(),
            line_count: f.line_count,
        })
        .collect();
    post_json("/v1/code-graph/merge-files", &MergeRowsRequest { rows: file_rows })?;

    let node_rows: Vec<CodeNodeRow<'_>> = nodes
        .iter()
        .map(|n| CodeNodeRow {
            project_id,
            repo_fingerprint,
            node_id: &n.node_id,
            props: serde_json::json!({
                "language": n.language,
                "kind": n.kind,
                "symbol": n.symbol,
                "file_path": n.file_path,
                "start_line": n.start_line,
                "end_line": n.end_line,
                "snippet_hash": n.snippet_hash,
                "text": n.text,
            }),
            extra_labels: n.extra_labels.iter().map(|s| s.as_str()).collect(),
        })
        .collect();

    let mut edges_by_type: BTreeMap<CodeRelType, Vec<EdgeRow<'_>>> = BTreeMap::new();
    for e in edges {
        edges_by_type.entry(e.rel_type).or_default().push(EdgeRow {
            project_id,
            repo_fingerprint,
            src_node_id: &e.src_node_id,
            dst_node_id: &e.dst_node_id,
            confidence: e.confidence,
            metadata: serde_json::Value::Object(e.metadata.clone()),
        });
    }

    post_json(
        "/v1/code-graph/merge-code-graph",
        &MergeCodeGraphRequest {
            nodes: node_rows,
            edges: edges_by_type,
        },
    )?;

    Ok(())
}


