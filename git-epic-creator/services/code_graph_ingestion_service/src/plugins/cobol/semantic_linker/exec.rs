use crate::core::records::{CodeNodeRecord, EdgeRecord};
use crate::core::stable_ids::stable_node_id;
use crate::core::types::CodeRelType;
use crate::plugins::cobol::normalizer::PreprocessResult;
use serde_json::Value;
use tree_sitter::{Node, Parser};

use super::regexes;
use super::resolution;
use super::text;

pub(crate) fn iter_exec_blocks(preprocess: &PreprocessResult) -> Vec<(i64, i64, String)> {
    let mut out: Vec<(i64, i64, String)> = Vec::new();
    for (idx, ln) in preprocess.logical_lines.iter().enumerate() {
        if ln.trim().eq_ignore_ascii_case("EXEC_BLOCK.") {
            let (s, e) = preprocess.logical_spans[idx];
            let text = text::slice_text(&preprocess.physical_lines, s, e);
            out.push((s, e, text));
        }
    }
    out
}

pub(crate) fn extract_exec_sql_table_ops(exec_text: &str) -> Vec<(String, String, CodeRelType)> {
    // Returns (sql_op, table, edge_type)
    // Prefer SQL grammar parsing for precision; fall back to regex if parsing yields no table refs.
    let parsed = extract_exec_sql_table_ops_treesitter(exec_text);
    if !parsed.is_empty() {
        return parsed;
    }
    extract_exec_sql_table_ops_regex(exec_text)
}

fn extract_exec_sql_table_ops_regex(exec_text: &str) -> Vec<(String, String, CodeRelType)> {
    let up = exec_text
        .replace("\r\n", "\n")
        .replace('\r', "\n")
        .to_ascii_uppercase();
    let mut out: Vec<(String, String, CodeRelType)> = Vec::new();

    for cap in regexes::re_sql_from_join().captures_iter(&up) {
        let t = cap.get(1).unwrap().as_str().to_string();
        out.push(("SELECT".to_string(), t, CodeRelType::Reads));
    }
    for cap in regexes::re_sql_insert_into().captures_iter(&up) {
        let t = cap.get(1).unwrap().as_str().to_string();
        out.push(("INSERT".to_string(), t, CodeRelType::Writes));
    }
    for cap in regexes::re_sql_update().captures_iter(&up) {
        let t = cap.get(1).unwrap().as_str().to_string();
        out.push(("UPDATE".to_string(), t, CodeRelType::Writes));
    }
    for cap in regexes::re_sql_delete_from().captures_iter(&up) {
        let t = cap.get(1).unwrap().as_str().to_string();
        out.push(("DELETE".to_string(), t, CodeRelType::Writes));
    }
    for cap in regexes::re_sql_merge_into().captures_iter(&up) {
        let t = cap.get(1).unwrap().as_str().to_string();
        out.push(("MERGE".to_string(), t, CodeRelType::Writes));
    }

    out.sort_by(|a, b| a.0.cmp(&b.0).then_with(|| a.1.cmp(&b.1)).then_with(|| a.2.cmp(&b.2)));
    out.dedup_by(|a, b| a.0 == b.0 && a.1 == b.1 && a.2 == b.2);
    out
}

fn extract_exec_sql_table_ops_treesitter(exec_text: &str) -> Vec<(String, String, CodeRelType)> {
    let sql = normalize_exec_sql_for_parsing(exec_text);
    if sql.trim().is_empty() {
        return vec![];
    }

    let mut parser = Parser::new();
    let lang: tree_sitter::Language = tree_sitter_sequel::LANGUAGE.into();
    if parser.set_language(&lang).is_err() {
        return vec![];
    }
    let Some(tree) = parser.parse(&sql, None) else {
        return vec![];
    };
    let root = tree.root_node();

    let mut out: Vec<(String, String, CodeRelType)> = Vec::new();
    let mut stack: Vec<Node<'_>> = vec![root];
    while let Some(n) = stack.pop() {
        let kind = n.kind();
        let (sql_op, edge_type) = match kind {
            "select" => ("SELECT", CodeRelType::Reads),
            "insert" => ("INSERT", CodeRelType::Writes),
            "update" => ("UPDATE", CodeRelType::Writes),
            "delete" => ("DELETE", CodeRelType::Writes),
            "merge" => ("MERGE", CodeRelType::Writes),
            "replace" => ("REPLACE", CodeRelType::Writes),
            _ => {
                let mut c = n.walk();
                for ch in n.children(&mut c) {
                    stack.push(ch);
                }
                continue;
            }
        };

        for t in extract_tables_from_sql_stmt(&sql, n) {
            out.push((sql_op.to_string(), t, edge_type));
        }

        let mut c = n.walk();
        for ch in n.children(&mut c) {
            stack.push(ch);
        }
    }

    out.sort_by(|a, b| a.0.cmp(&b.0).then_with(|| a.1.cmp(&b.1)).then_with(|| a.2.cmp(&b.2)));
    out.dedup_by(|a, b| a.0 == b.0 && a.1 == b.1 && a.2 == b.2);
    out
}

fn normalize_exec_sql_for_parsing(exec_text: &str) -> String {
    // Turn the COBOL physical span (with FIXED/FREE formatting and hostvars) into a best-effort SQL program.
    // We keep this conservative so we don't break parsing on real code.
    let mut sql_lines: Vec<String> = Vec::new();
    for raw_line in exec_text
        .replace("\r\n", "\n")
        .replace('\r', "\n")
        .lines()
    {
        let mut line = strip_cobol_fixed_prefix(raw_line);
        let up = line.trim_start().to_ascii_uppercase();
        if up.starts_with("EXEC SQL") {
            continue;
        }
        if up.contains("END-EXEC") {
            break;
        }
        // Trim COBOL sentence period, but keep SQL punctuation.
        line = line.trim().trim_end_matches('.').trim().to_string();
        if line.is_empty() {
            continue;
        }
        sql_lines.push(line);
    }
    let joined = sql_lines.join(" ");
    // Normalize hostvars `:WS-NAME` into `WS_NAME`.
    let joined = regexes::re_sql_hostvar().replace_all(&joined, |caps: &regex::Captures<'_>| {
        let raw = caps.get(1).map(|m| m.as_str()).unwrap_or("");
        let cleaned = raw.replace('-', "_");
        cleaned
    });
    joined.to_string()
}

fn strip_cobol_fixed_prefix(line: &str) -> String {
    // Best-effort: if this looks like FIXED-format (sequence area + indicator area), drop cols 1-7.
    // We can't perfectly know FREE vs FIXED at this stage (physical evidence is verbatim), so we keep it heuristic.
    if line.len() >= 7 {
        let (prefix, rest) = line.split_at(7);
        let looks_like_seq = prefix[..6]
            .bytes()
            .all(|b| b == b' ' || (b'0'..=b'9').contains(&b));
        let indicator = prefix.as_bytes().get(6).copied().unwrap_or(b' ') as char;
        if looks_like_seq && (indicator == ' ' || indicator == '-') {
            return rest.to_string();
        }
    }
    line.to_string()
}

fn extract_tables_from_sql_stmt(source: &str, stmt: Node<'_>) -> Vec<String> {
    // Conservative: only accept object_reference nodes that appear under FROM or RELATION contexts.
    let mut out: Vec<String> = Vec::new();

    // Insert has a direct `name` field in this grammar; use it if present.
    if stmt.kind() == "insert" {
        if let Some(name) = stmt.child_by_field_name("name") {
            if let Some(t) = sql_ident_text(source, name) {
                out.push(t);
            }
        }
    }

    let mut stack: Vec<Node<'_>> = vec![stmt];
    while let Some(n) = stack.pop() {
        match n.kind() {
            "from" | "relation" => {
                // Pull direct object_reference children (table refs).
                let mut c = n.walk();
                for ch in n.children(&mut c) {
                    if ch.kind() == "object_reference" {
                        if let Some(t) = sql_ident_text(source, ch) {
                            out.push(t);
                        }
                    }
                }
            }
            _ => {}
        }

        let mut c = n.walk();
        for ch in n.children(&mut c) {
            stack.push(ch);
        }
    }

    out.sort();
    out.dedup();
    out
}

fn sql_ident_text(source: &str, node: Node<'_>) -> Option<String> {
    let raw = node.utf8_text(source.as_bytes()).ok()?.trim().to_string();
    let t = raw
        .trim_matches('"')
        .trim_matches('\'')
        .trim_matches('`')
        .trim()
        .to_string();
    if t.is_empty() {
        None
    } else {
        Some(t)
    }
}

pub(crate) fn push_exec_cics_edges(
    project_id: &str,
    repo_fingerprint: &str,
    file_path: &str,
    exec_text: &str,
    src_node: &str,
    nodes: &mut std::collections::BTreeMap<String, CodeNodeRecord>,
    edges: &mut Vec<EdgeRecord>,
) {
    // Always model the CICS command itself as a conservative CALL target.
    if let Some(cap) = regexes::re_exec_cics_command().captures(exec_text) {
        let cmd = cap.get(1).unwrap().as_str().trim().to_ascii_uppercase();
        if cmd.is_empty() {
            return;
        }
        let unresolved_id = stable_node_id([project_id, repo_fingerprint, file_path, "unresolved", "cics_cmd", &cmd]);
        nodes.entry(unresolved_id.clone()).or_insert_with(|| {
            resolution::unresolved_node(
                project_id,
                repo_fingerprint,
                file_path,
                &format!("CICS:{cmd}"),
                &unresolved_id,
                "__UnresolvedCicsCommand__",
            )
        });
        let mut md = serde_json::Map::new();
        md.insert("via".to_string(), Value::String("exec_cics".to_string()));
        md.insert("command".to_string(), Value::String(cmd.clone()));
        edges.push(EdgeRecord {
            project_id: project_id.to_string(),
            repo_fingerprint: repo_fingerprint.to_string(),
            rel_type: CodeRelType::Calls,
            src_node_id: src_node.to_string(),
            dst_node_id: unresolved_id,
            confidence: 0.7,
            metadata: md,
        });

        // If this looks like a program control command, also emit a CALL to PROGRAM(...)
        // so repo-global literal resolution can connect it to a PROGRAM-ID node.
        if matches!(cmd.as_str(), "LINK" | "XCTL" | "LOAD" | "START") {
            if let Some(pcap) = regexes::re_exec_cics_program_arg().captures(exec_text) {
                let prog = pcap
                    .get(1)
                    .or_else(|| pcap.get(2))
                    .or_else(|| pcap.get(3))
                    .map(|m| m.as_str().trim().to_string())
                    .unwrap_or_default()
                    .trim()
                    .to_ascii_uppercase();
                if prog.is_empty() {
                    return;
                }
                let call_type = if pcap.get(1).is_some() || pcap.get(2).is_some() {
                    "literal"
                } else {
                    "dynamic"
                };
                let prog_unresolved_id = stable_node_id([project_id, repo_fingerprint, file_path, "unresolved", "call", &prog]);
                nodes.entry(prog_unresolved_id.clone()).or_insert_with(|| {
                    resolution::unresolved_node(
                        project_id,
                        repo_fingerprint,
                        file_path,
                        &prog,
                        &prog_unresolved_id,
                        "__UnresolvedCall__",
                    )
                });
                let mut md = serde_json::Map::new();
                md.insert("callee".to_string(), Value::String(prog.clone()));
                md.insert("call_type".to_string(), Value::String(call_type.to_string()));
                md.insert("via".to_string(), Value::String("exec_cics".to_string()));
                md.insert("command".to_string(), Value::String(cmd));
                edges.push(EdgeRecord {
                    project_id: project_id.to_string(),
                    repo_fingerprint: repo_fingerprint.to_string(),
                    rel_type: CodeRelType::Calls,
                    src_node_id: src_node.to_string(),
                    dst_node_id: prog_unresolved_id,
                    confidence: if call_type == "literal" { 0.8 } else { 0.2 },
                    metadata: md,
                });
            }
        }
    }
}


