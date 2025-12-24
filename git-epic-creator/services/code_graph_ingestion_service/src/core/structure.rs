use crate::core::records::{CodeNodeRecord, EdgeRecord};
use crate::core::types::CodeRelType;
use serde_json::Value;

pub fn build_contains_edges(nodes: &[CodeNodeRecord]) -> Vec<EdgeRecord> {
    let mut by_file: std::collections::BTreeMap<String, Vec<CodeNodeRecord>> = std::collections::BTreeMap::new();
    for n in nodes {
        by_file.entry(n.file_path.clone()).or_default().push(n.clone());
    }

    let mut out: Vec<EdgeRecord> = Vec::new();
    for (_file_path, mut file_nodes) in by_file {
        file_nodes.sort_by(|a, b| {
            (a.start_line, -a.end_line, a.node_id.clone()).cmp(&(b.start_line, -b.end_line, b.node_id.clone()))
        });

        let mut stack: Vec<CodeNodeRecord> = Vec::new();
        // Deterministic sibling ordering for direct children per parent, per file.
        // 1-based to match existing `NEXT_CHUNK.metadata.order` convention.
        let mut child_order_by_parent: std::collections::HashMap<String, i64> = std::collections::HashMap::new();
        for cur in file_nodes {
            while let Some(top) = stack.last() {
                if cur.end_line > top.end_line {
                    stack.pop();
                } else {
                    break;
                }
            }
            if let Some(parent) = stack.last() {
                if parent.start_line <= cur.start_line
                    && parent.end_line >= cur.end_line
                    && (parent.start_line, parent.end_line) != (cur.start_line, cur.end_line)
                {
                    let order = {
                        let e = child_order_by_parent.entry(parent.node_id.clone()).or_insert(0);
                        *e += 1;
                        *e
                    };
                    let mut md = serde_json::Map::new();
                    md.insert("strategy".to_string(), Value::String("span-nesting".to_string()));
                    md.insert("order".to_string(), Value::Number(order.into()));
                    out.push(EdgeRecord {
                        project_id: cur.project_id.clone(),
                        repo_fingerprint: cur.repo_fingerprint.clone(),
                        rel_type: CodeRelType::Contains,
                        src_node_id: parent.node_id.clone(),
                        dst_node_id: cur.node_id.clone(),
                        confidence: 1.0,
                        metadata: md,
                    });
                }
            }
            stack.push(cur);
        }
    }

    let mut uniq: std::collections::BTreeMap<(CodeRelType, String, String, String), EdgeRecord> =
        std::collections::BTreeMap::new();
    for e in out {
        let md_key = serde_json::to_string(&e.metadata).unwrap_or_default();
        uniq.insert((e.rel_type, e.src_node_id.clone(), e.dst_node_id.clone(), md_key), e);
    }
    uniq.into_values().collect()
}


