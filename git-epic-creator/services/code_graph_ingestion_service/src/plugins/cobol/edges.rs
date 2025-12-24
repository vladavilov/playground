use crate::core::records::{CodeNodeRecord, EdgeRecord};
use crate::core::stable_ids::{snippet_hash, stable_node_id};
use crate::core::types::{CodeLanguage, CodeRelType};
use crate::plugins::cobol::semantic_linker;
use crate::plugins::tsg;
use regex::Regex;
use serde_json::Value;

#[derive(Debug, Clone)]
pub struct CobolEdgesResult {
    pub nodes: Vec<CodeNodeRecord>,
    pub edges: Vec<EdgeRecord>,
}

pub fn extract_cobol_edges(
    project_id: &str,
    repo_fingerprint: &str,
    file_path: &str,
    program_node_id: &str,
    physical_lines: &[String],
) -> CobolEdgesResult {
    let mut nodes: Vec<CodeNodeRecord> = Vec::new();
    let mut edges: Vec<EdgeRecord> = Vec::new();
    let end_line = (physical_lines.len() as i64).max(1);

    let parse_stream = physical_lines.join("\n") + "\n";
    let extracted = tsg::extract_cobol(&parse_stream).unwrap_or(tsg::CobolExtract {
        calls: vec![],
        performs: vec![],
        io: vec![],
    });

    // CALLS
    for c in extracted.calls {
        let callee = c.callee;
        let target_id = unresolved_node_id(project_id, repo_fingerprint, file_path, "call", &callee);
        nodes.push(unresolved_node(
            project_id,
            repo_fingerprint,
            file_path,
            1,
            end_line,
            &callee,
            &target_id,
            &["__UnresolvedCall__"],
        ));

        let mut md = serde_json::Map::new();
        md.insert("callee".to_string(), Value::String(callee));
        md.insert("call_type".to_string(), Value::String(c.call_type.to_string()));
        edges.push(EdgeRecord {
            project_id: project_id.to_string(),
            repo_fingerprint: repo_fingerprint.to_string(),
            rel_type: CodeRelType::Calls,
            src_node_id: program_node_id.to_string(),
            dst_node_id: target_id,
            confidence: if c.call_type == "literal" { 0.8 } else { 0.2 },
            metadata: md,
        });
    }

    // PERFORM targets
    for p in extracted.performs {
        let target = p.target;
        let target_id = unresolved_node_id(project_id, repo_fingerprint, file_path, "perform", &target);
        nodes.push(unresolved_node(
            project_id,
            repo_fingerprint,
            file_path,
            1,
            end_line,
            &target,
            &target_id,
            &[],
        ));

        let mut md = serde_json::Map::new();
        md.insert("target".to_string(), Value::String(target));
        md.insert(
            "thru".to_string(),
            p.thru.map(Value::String).unwrap_or(Value::Null),
        );
        edges.push(EdgeRecord {
            project_id: project_id.to_string(),
            repo_fingerprint: repo_fingerprint.to_string(),
            rel_type: CodeRelType::Performs,
            src_node_id: program_node_id.to_string(),
            dst_node_id: target_id,
            confidence: 0.6,
            metadata: md,
        });
    }

    // IO (READ/WRITE/REWRITE/DELETE) -> READS/WRITES edges (conservative).
    for io in extracted.io {
        let rel_type = match io.op {
            "READ" => CodeRelType::Reads,
            "WRITE" | "REWRITE" | "DELETE" => CodeRelType::Writes,
            _ => continue,
        };

        // Include op in the unresolved identity so READ/WRITE of same symbol don't collide.
        let target_id = unresolved_node_id(
            project_id,
            repo_fingerprint,
            file_path,
            "io",
            &format!("{}:{}", io.op, io.target),
        );
        nodes.push(unresolved_node(
            project_id,
            repo_fingerprint,
            file_path,
            1,
            end_line,
            &io.target,
            &target_id,
            &[if io.op == "READ" || io.op == "DELETE" {
                "__UnresolvedFile__"
            } else {
                "__UnresolvedRecord__"
            }],
        ));

        let mut md = serde_json::Map::new();
        md.insert("op".to_string(), Value::String(io.op.to_string()));
        md.insert("target".to_string(), Value::String(io.target));
        edges.push(EdgeRecord {
            project_id: project_id.to_string(),
            repo_fingerprint: repo_fingerprint.to_string(),
            rel_type,
            src_node_id: program_node_id.to_string(),
            dst_node_id: target_id,
            confidence: 0.6,
            metadata: md,
        });
    }

    // EXEC-derived edges (fallback path): scan physical lines for EXEC...END-EXEC blocks.
    for (s, e, text) in exec_blocks_from_physical(physical_lines) {
        if let Some(cap) = re_exec_header().captures(&text) {
            let kind = cap.get(1).unwrap().as_str().to_ascii_uppercase();
            if kind == "SQL" {
                for (sql_op, table, access) in semantic_linker::exec::extract_exec_sql_table_ops(&text) {
                    let table_up = table.trim().to_ascii_uppercase();
                    if table_up.is_empty() {
                        continue;
                    }
                    let nid = stable_node_id([
                        project_id,
                        repo_fingerprint,
                        file_path,
                        "sql_table",
                        &table_up,
                        &s.to_string(),
                        &e.to_string(),
                        &sql_op,
                    ]);
                    nodes.push(CodeNodeRecord {
                        project_id: project_id.to_string(),
                        repo_fingerprint: repo_fingerprint.to_string(),
                        node_id: nid.clone(),
                        language: CodeLanguage::Other,
                        kind: "sql_table".to_string(),
                        symbol: Some(table_up.clone()),
                        file_path: file_path.to_string(),
                        start_line: s,
                        end_line: e,
                        snippet_hash: snippet_hash(file_path, s, e, ""),
                        text: String::new(),
                        extra_labels: vec!["__ExecSqlTable__".to_string()],
                    });

                    let mut md = serde_json::Map::new();
                    md.insert("via".to_string(), Value::String("exec_sql".to_string()));
                    md.insert("sql_op".to_string(), Value::String(sql_op));
                    md.insert("table".to_string(), Value::String(table_up));
                    edges.push(EdgeRecord {
                        project_id: project_id.to_string(),
                        repo_fingerprint: repo_fingerprint.to_string(),
                        rel_type: access,
                        src_node_id: program_node_id.to_string(),
                        dst_node_id: nid,
                        confidence: 0.4,
                        metadata: md,
                    });
                }
            } else if kind == "CICS" {
                if let Some(cap) = re_exec_cics_command().captures(&text) {
                    let cmd = cap.get(1).unwrap().as_str().trim().to_ascii_uppercase();
                    if !cmd.is_empty() {
                        let unresolved_id = unresolved_node_id(project_id, repo_fingerprint, file_path, "cics_cmd", &cmd);
                        nodes.push(unresolved_node(
                            project_id,
                            repo_fingerprint,
                            file_path,
                            1,
                            end_line,
                            &format!("CICS:{cmd}"),
                            &unresolved_id,
                            &["__UnresolvedCicsCommand__"],
                        ));
                        let mut md = serde_json::Map::new();
                        md.insert("via".to_string(), Value::String("exec_cics".to_string()));
                        md.insert("command".to_string(), Value::String(cmd));
                        edges.push(EdgeRecord {
                            project_id: project_id.to_string(),
                            repo_fingerprint: repo_fingerprint.to_string(),
                            rel_type: CodeRelType::Calls,
                            src_node_id: program_node_id.to_string(),
                            dst_node_id: unresolved_id,
                            confidence: 0.4,
                            metadata: md,
                        });
                    }
                }
            }
        }
    }

    // Deterministic de-dup
    let mut uniq_nodes: std::collections::BTreeMap<String, CodeNodeRecord> = std::collections::BTreeMap::new();
    for n in nodes {
        uniq_nodes.insert(n.node_id.clone(), n);
    }
    let mut uniq_edges: std::collections::BTreeMap<(CodeRelType, String, String, String), EdgeRecord> =
        std::collections::BTreeMap::new();
    for e in edges {
        let md_key = serde_json::to_string(&e.metadata).unwrap_or_default();
        uniq_edges.insert((e.rel_type, e.src_node_id.clone(), e.dst_node_id.clone(), md_key), e);
    }

    CobolEdgesResult {
        nodes: uniq_nodes.into_values().collect(),
        edges: uniq_edges.into_values().collect(),
    }
}

fn unresolved_node_id(project_id: &str, repo_fingerprint: &str, file_path: &str, kind: &str, symbol: &str) -> String {
    stable_node_id([project_id, repo_fingerprint, file_path, "unresolved", kind, symbol])
}

fn unresolved_node(
    project_id: &str,
    repo_fingerprint: &str,
    file_path: &str,
    start_line: i64,
    end_line: i64,
    symbol: &str,
    node_id: &str,
    extra_labels: &[&str],
) -> CodeNodeRecord {
    let text = "";
    let sh = snippet_hash(file_path, start_line, end_line, text);
    CodeNodeRecord {
        project_id: project_id.to_string(),
        repo_fingerprint: repo_fingerprint.to_string(),
        node_id: node_id.to_string(),
        language: CodeLanguage::Cobol,
        kind: "unresolved".to_string(),
        symbol: Some(symbol.to_string()),
        file_path: file_path.to_string(),
        start_line,
        end_line,
        snippet_hash: sh,
        text: text.to_string(),
        extra_labels: extra_labels.iter().map(|s| s.to_string()).collect(),
    }
}

fn re_exec_header() -> &'static Regex {
    static RE: std::sync::OnceLock<Regex> = std::sync::OnceLock::new();
    RE.get_or_init(|| Regex::new(r"(?i)\bEXEC\s+(SQL|CICS)\b").unwrap())
}
fn re_exec_cics_command() -> &'static Regex {
    static RE: std::sync::OnceLock<Regex> = std::sync::OnceLock::new();
    RE.get_or_init(|| Regex::new(r"(?i)\bEXEC\s+CICS\s+([A-Z0-9-]+)\b").unwrap())
}

fn exec_blocks_from_physical(lines: &[String]) -> Vec<(i64, i64, String)> {
    let mut out: Vec<(i64, i64, String)> = Vec::new();
    let mut i = 0usize;
    while i < lines.len() {
        let line = &lines[i];
        if line.to_ascii_uppercase().contains("EXEC ") {
            let start = (i as i64) + 1;
            let mut end = start;
            let mut text = String::new();
            text.push_str(line);
            text.push('\n');
            i += 1;
            while i < lines.len() {
                end = (i as i64) + 1;
                text.push_str(&lines[i]);
                text.push('\n');
                if lines[i].to_ascii_uppercase().contains("END-EXEC") {
                    i += 1;
                    break;
                }
                i += 1;
            }
            out.push((start, end, text));
            continue;
        }
        i += 1;
    }
    out
}


