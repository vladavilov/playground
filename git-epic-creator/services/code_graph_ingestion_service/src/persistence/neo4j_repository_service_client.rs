use crate::core::inventory::InventoryEntry;
use crate::core::records::{CodeNodeRecord, EdgeRecord};
use crate::core::types::CodeRelType;
use reqwest::blocking::Client;
use reqwest::header::AUTHORIZATION;
use serde::Serialize;
use std::collections::BTreeMap;
use std::sync::OnceLock;

fn base_url() -> &'static str {
    static URL: OnceLock<String> = OnceLock::new();
    URL.get_or_init(|| {
        std::env::var("NEO4J_REPOSITORY_SERVICE_URL").to_string())
            .trim_end_matches('/')
            .to_string()
    })
}

fn client() -> anyhow::Result<&'static Client> {
    static CLIENT: OnceLock<anyhow::Result<Client>> = OnceLock::new();
    let res = CLIENT.get_or_init(|| {
        let timeout_s = std::env::var("NEO4J_REPOSITORY_TIMEOUT_S")
            .ok()
            .and_then(|s| s.parse::<u64>().ok())
            .unwrap_or(60);
        Ok(Client::builder()
            .timeout(std::time::Duration::from_secs(timeout_s))
            .build()?)
    });
    match res {
        Ok(c) => Ok(c),
        Err(e) => Err(anyhow::anyhow!(e.to_string())),
    }
}

fn post_json<T: Serialize>(
    path: &str,
    payload: &T,
    auth_header: Option<&str>,
) -> anyhow::Result<()> {
    let url = format!("{}{}", base_url(), path);
    let mut req = client()?.post(url).json(payload);
    if let Some(h) = auth_header {
        req = req.header(AUTHORIZATION, h);
    }
    let resp = req.send()?;
    if !resp.status().is_success() {
        anyhow::bail!("neo4j-repository-service returned {}", resp.status());
    }
    Ok(())
}

#[derive(Debug, Serialize)]
struct FileRow<'a> {
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
    metadata: &'a serde_json::Map<String, serde_json::Value>,
}

#[derive(Debug, Serialize)]
struct MergeCodeGraphRequest<'a> {
    project_id: &'a str,
    repo_fingerprint: &'a str,
    files: Vec<FileRow<'a>>,
    nodes: Vec<CodeNodeRow<'a>>,
    edges: BTreeMap<CodeRelType, Vec<EdgeRow<'a>>>,
}

pub fn persist_code_graph(
    project_id: &str,
    repo_fingerprint: &str,
    files: &[InventoryEntry],
    nodes: &[CodeNodeRecord],
    edges: &[EdgeRecord],
    auth_header: Option<&str>,
) -> anyhow::Result<()> {
    let auth_header = auth_header
        .map(|s| s.trim())
        .filter(|s| !s.is_empty())
        .ok_or_else(|| {
            anyhow::anyhow!("Missing Authorization header for neo4j-repository-service call")
        })?;

    let file_rows: Vec<FileRow<'_>> = files
        .iter()
        .map(|f| FileRow {
            file_path: &f.path,
            sha256: &f.sha256,
            language: f.language.as_str(),
            line_count: f.line_count,
        })
        .collect();

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
            metadata: &e.metadata,
        });
    }

    post_json(
        "/v1/code-graph/merge-code-graph",
        &MergeCodeGraphRequest {
            project_id,
            repo_fingerprint,
            files: file_rows,
            nodes: node_rows,
            edges: edges_by_type,
        },
        Some(auth_header),
    )?;

    Ok(())
}
