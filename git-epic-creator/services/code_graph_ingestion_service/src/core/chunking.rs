use crate::core::records::{CodeNodeRecord, EdgeRecord};
use crate::core::stable_ids::{snippet_hash, stable_node_id};
use crate::core::types::CodeRelType;
use serde_json::Value;

#[derive(Debug, Clone)]
pub struct ChunkingResult {
    pub nodes: Vec<CodeNodeRecord>,
    pub edges: Vec<EdgeRecord>,
}

pub fn chunk_oversized_nodes(
    nodes: &[CodeNodeRecord],
    max_lines: i64,
    chunk_size: i64,
    overlap: i64,
) -> ChunkingResult {
    let mut out_nodes: Vec<CodeNodeRecord> = Vec::new();
    let mut out_edges: Vec<EdgeRecord> = Vec::new();

    for n in nodes {
        let span_lines = (n.end_line - n.start_line + 1).max(0);
        if span_lines <= max_lines || n.text.is_empty() {
            out_nodes.push(n.clone());
            continue;
        }

        // Keep original span node (so the graph has a stable "container" span).
        out_nodes.push(n.clone());

        // Chunk deterministically by physical line windows.
        let normalized_text = n.text.replace("\r\n", "\n").replace('\r', "\n");
        let mut physical_lines: Vec<&str> = normalized_text.split('\n').collect();
        if physical_lines.last() == Some(&"") {
            physical_lines.pop();
        }

        let mut start = n.start_line;
        let mut idx: i64 = 0;
        let mut chunk_ids: Vec<String> = Vec::new();

        while start <= n.end_line {
            idx += 1;
            let end = (start + chunk_size - 1).min(n.end_line);
            let offset_start = (start - n.start_line).max(0) as usize;
            let offset_end = (end - n.start_line).max(0) as usize;
            let chunk_text = physical_lines[offset_start..=offset_end].join("\n") + "\n";

            let idx_s = idx.to_string();
            let cid = stable_node_id([
                n.project_id.as_str(),
                n.repo_fingerprint.as_str(),
                n.node_id.as_str(),
                "chunk",
                idx_s.as_str(),
            ]);
            chunk_ids.push(cid.clone());

            let ch = CodeNodeRecord {
                project_id: n.project_id.clone(),
                repo_fingerprint: n.repo_fingerprint.clone(),
                node_id: cid,
                language: n.language,
                kind: "chunk".to_string(),
                symbol: n.symbol.clone(),
                file_path: n.file_path.clone(),
                start_line: start,
                end_line: end,
                snippet_hash: snippet_hash(&n.file_path, start, end, &chunk_text),
                text: chunk_text,
                extra_labels: n.extra_labels.clone(),
            };
            out_nodes.push(ch);

            if end == n.end_line {
                break;
            }
            start = end - overlap + 1;
        }

        for i in 0..(chunk_ids.len().saturating_sub(1)) {
            let mut md = serde_json::Map::new();
            md.insert("synthetic".to_string(), Value::Bool(true));
            md.insert("order".to_string(), Value::Number(((i as i64) + 1).into()));
            out_edges.push(EdgeRecord {
                project_id: n.project_id.clone(),
                repo_fingerprint: n.repo_fingerprint.clone(),
                rel_type: CodeRelType::NextChunk,
                src_node_id: chunk_ids[i].clone(),
                dst_node_id: chunk_ids[i + 1].clone(),
                confidence: 1.0,
                metadata: md,
            });
        }
    }

    ChunkingResult {
        nodes: out_nodes,
        edges: out_edges,
    }
}


