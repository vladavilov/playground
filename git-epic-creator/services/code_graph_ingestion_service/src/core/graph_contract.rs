use crate::core::inventory::InventoryEntry;
use crate::core::records::{CodeNodeRecord, EdgeRecord};
use crate::core::types::CodeRelType;
use std::path::Path;

#[derive(Debug, thiserror::Error)]
#[error("{message}")]
pub struct GraphContractViolation {
    pub message: String,
}

pub fn validate_graph_contract(
    project_id: &str,
    repo_fingerprint: &str,
    repo_root: &Path,
    files: &[InventoryEntry],
    nodes: &[CodeNodeRecord],
    edges: &[EdgeRecord],
) -> Result<(), GraphContractViolation> {
    fn is_valid_repo_rel_posix_path(p: &str) -> bool {
        if p.is_empty() {
            return false;
        }
        // repo-relative POSIX path only
        if p.contains('\\') {
            return false;
        }
        if p.starts_with('/') {
            return false;
        }
        // avoid Windows drive-like prefixes and path traversal
        if p.contains(':') {
            return false;
        }
        for part in p.split('/') {
            if part.is_empty() || part == "." || part == ".." {
                return false;
            }
        }
        true
    }

    fn exists_under_repo_root(repo_root: &Path, repo_rel_posix_path: &str) -> bool {
        if !is_valid_repo_rel_posix_path(repo_rel_posix_path) {
            return false;
        }
        let repo_root = match std::path::absolute(repo_root) {
            Ok(p) => p,
            Err(_) => return false,
        };
        let candidate = repo_root.join(repo_rel_posix_path);
        let candidate = match std::path::absolute(candidate) {
            Ok(p) => p,
            Err(_) => return false,
        };
        if candidate.strip_prefix(&repo_root).is_err() {
            return false;
        }
        candidate.is_file()
    }

    let file_paths: std::collections::HashSet<&str> =
        files.iter().map(|f| f.path.as_str()).collect();
    let file_max_line_by_path: std::collections::HashMap<&str, i64> = files
        .iter()
        .map(|f| (f.path.as_str(), f.line_count.max(1)))
        .collect();

    // Build node index and enforce deterministic node_id uniqueness.
    let mut node_by_id: std::collections::HashMap<&str, &CodeNodeRecord> =
        std::collections::HashMap::new();
    for n in nodes {
        if n.node_id.is_empty() {
            return Err(GraphContractViolation {
                message: "Node node_id is empty".to_string(),
            });
        }
        if node_by_id.insert(n.node_id.as_str(), n).is_some() {
            return Err(GraphContractViolation {
                message: format!("Duplicate node_id detected: {}", n.node_id),
            });
        }
    }

    for n in nodes {
        if n.project_id != project_id {
            return Err(GraphContractViolation {
                message: format!("Node project_id mismatch: {}", n.node_id),
            });
        }
        if n.repo_fingerprint != repo_fingerprint {
            return Err(GraphContractViolation {
                message: format!("Node repo_fingerprint mismatch: {}", n.node_id),
            });
        }
        if n.kind.trim().is_empty() {
            return Err(GraphContractViolation {
                message: format!("Node kind is empty: {}", n.node_id),
            });
        }
        if n.file_path.is_empty() {
            return Err(GraphContractViolation {
                message: format!("Node file_path is empty: {}", n.node_id),
            });
        }
        if !is_valid_repo_rel_posix_path(&n.file_path) {
            return Err(GraphContractViolation {
                message: format!("Node file_path is not repo-relative POSIX path: {}", n.node_id),
            });
        }
        if !file_paths.contains(n.file_path.as_str()) {
            return Err(GraphContractViolation {
                message: format!(
                    "Node file_path not in inventory: {} -> {}",
                    n.node_id, n.file_path
                ),
            });
        }
        if n.start_line < 1 {
            return Err(GraphContractViolation {
                message: format!("Node start_line < 1: {}", n.node_id),
            });
        }
        if n.end_line < n.start_line {
            return Err(GraphContractViolation {
                message: format!("Node end_line < start_line: {}", n.node_id),
            });
        }
        let max_line = file_max_line_by_path
            .get(n.file_path.as_str())
            .copied()
            .unwrap_or(1);
        if n.end_line > max_line {
            return Err(GraphContractViolation {
                message: format!(
                    "Node end_line out of bounds: {} (end_line={}, max_line={})",
                    n.node_id, n.end_line, max_line
                ),
            });
        }
        if n.snippet_hash.trim().is_empty() {
            return Err(GraphContractViolation {
                message: format!("Node snippet_hash is empty: {}", n.node_id),
            });
        }
    }

    for e in edges {
        if e.project_id != project_id {
            return Err(GraphContractViolation {
                message: format!("Edge project_id mismatch: {}", e.rel_type),
            });
        }
        if e.repo_fingerprint != repo_fingerprint {
            return Err(GraphContractViolation {
                message: format!("Edge repo_fingerprint mismatch: {}", e.rel_type),
            });
        }
        if e.src_node_id.trim().is_empty() {
            return Err(GraphContractViolation {
                message: format!("Edge src_node_id is empty: {}", e.rel_type),
            });
        }
        if e.dst_node_id.trim().is_empty() {
            return Err(GraphContractViolation {
                message: format!("Edge dst_node_id is empty: {}", e.rel_type),
            });
        }
        if !(0.0..=1.0).contains(&e.confidence) {
            return Err(GraphContractViolation {
                message: format!("Edge confidence out of range: {}", e.rel_type),
            });
        }
        // In Rust, metadata is always a map; no None state.
        if e.rel_type == CodeRelType::Includes {
            if !node_by_id.contains_key(e.src_node_id.as_str()) {
                return Err(GraphContractViolation {
                    message: format!(
                        "INCLUDES src_node_id must exist as node: {}",
                        e.src_node_id
                    ),
                });
            }
            // Convention: INCLUDES.dst_node_id is a repo-relative file path (not a node id).
            if !is_valid_repo_rel_posix_path(&e.dst_node_id) {
                return Err(GraphContractViolation {
                    message: format!("INCLUDES dst is not repo-relative POSIX path: {}", e.dst_node_id),
                });
            }
            let in_inventory = file_paths.contains(e.dst_node_id.as_str());
            let on_disk = exists_under_repo_root(repo_root, &e.dst_node_id);
            if !(in_inventory || on_disk) {
                return Err(GraphContractViolation {
                    message: format!(
                        "INCLUDES dst must exist in inventory or on disk: {}",
                        e.dst_node_id
                    ),
                });
            }
            continue;
        }

        let Some(src) = node_by_id.get(e.src_node_id.as_str()) else {
            return Err(GraphContractViolation {
                message: format!("Edge src_node_id not found: {}", e.src_node_id),
            });
        };
        let Some(dst) = node_by_id.get(e.dst_node_id.as_str()) else {
            return Err(GraphContractViolation {
                message: format!("Edge dst_node_id not found: {}", e.dst_node_id),
            });
        };

        match e.rel_type {
            CodeRelType::Contains => {
                if src.file_path != dst.file_path {
                    return Err(GraphContractViolation {
                        message: format!("CONTAINS must stay within a file: {}", e.src_node_id),
                    });
                }
                let spans_equal = (src.start_line, src.end_line) == (dst.start_line, dst.end_line);
                if spans_equal
                    || !(src.start_line <= dst.start_line && src.end_line >= dst.end_line)
                {
                    return Err(GraphContractViolation {
                        message: format!("CONTAINS span is not a proper container: {}", e.src_node_id),
                    });
                }
            }
            CodeRelType::NextChunk => {
                if e.src_node_id == e.dst_node_id {
                    return Err(GraphContractViolation {
                        message: "NEXT_CHUNK must not be a self-loop".to_string(),
                    });
                }
                if src.kind != "chunk" || dst.kind != "chunk" {
                    return Err(GraphContractViolation {
                        message: "NEXT_CHUNK endpoints must be chunk nodes".to_string(),
                    });
                }
                if src.file_path != dst.file_path {
                    return Err(GraphContractViolation {
                        message: "NEXT_CHUNK must stay within a file".to_string(),
                    });
                }
                if src.start_line > dst.start_line {
                    return Err(GraphContractViolation {
                        message: "NEXT_CHUNK must move forward".to_string(),
                    });
                }
            }
            _ => {}
        }
    }

    Ok(())
}


