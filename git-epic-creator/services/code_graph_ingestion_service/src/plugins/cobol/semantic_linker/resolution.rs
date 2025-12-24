use crate::core::records::CodeNodeRecord;
use crate::core::stable_ids::snippet_hash;
use crate::core::types::CodeLanguage;

pub(crate) fn build_label_indexes(
    nodes: &std::collections::BTreeMap<String, CodeNodeRecord>,
) -> (
    std::collections::BTreeMap<String, Vec<String>>,
    std::collections::BTreeMap<String, Vec<String>>,
) {
    let mut paragraph_index: std::collections::BTreeMap<String, Vec<String>> = std::collections::BTreeMap::new();
    let mut section_index: std::collections::BTreeMap<String, Vec<String>> = std::collections::BTreeMap::new();
    for n in nodes.values() {
        if n.kind == "paragraph" {
            if let Some(sym) = &n.symbol {
                paragraph_index
                    .entry(sym.trim().to_string())
                    .or_default()
                    .push(n.node_id.clone());
            }
        }
        if n.kind == "section" {
            if let Some(sym) = &n.symbol {
                section_index
                    .entry(sym.trim().to_string())
                    .or_default()
                    .push(n.node_id.clone());
            }
        }
    }
    for v in paragraph_index.values_mut() {
        v.sort();
        v.dedup();
    }
    for v in section_index.values_mut() {
        v.sort();
        v.dedup();
    }
    (paragraph_index, section_index)
}

pub(crate) fn resolve_perform_target(
    target: &str,
    paragraph_index: &std::collections::BTreeMap<String, Vec<String>>,
    section_index: &std::collections::BTreeMap<String, Vec<String>>,
) -> (Option<String>, &'static str) {
    let t = target.trim().to_ascii_uppercase();
    if let Some(paras) = paragraph_index.get(&t) {
        if paras.len() == 1 {
            return (Some(paras[0].clone()), "paragraph");
        }
    }
    if let Some(secs) = section_index.get(&t) {
        if secs.len() == 1 {
            return (Some(secs[0].clone()), "section");
        }
    }
    (None, "unknown")
}

fn resolve_label_in_nodes(
    label: &str,
    nodes: &std::collections::BTreeMap<String, CodeNodeRecord>,
) -> Option<(String, &'static str)> {
    let t = label.trim().to_ascii_uppercase();
    if t.is_empty() {
        return None;
    }
    let mut para: Vec<String> = nodes
        .values()
        .filter(|n| {
            n.kind == "paragraph"
                && n.symbol
                    .as_deref()
                    .is_some_and(|s| s.trim().eq_ignore_ascii_case(&t))
        })
        .map(|n| n.node_id.clone())
        .collect();
    para.sort();
    para.dedup();
    if para.len() == 1 {
        return Some((para[0].clone(), "paragraph"));
    }

    let mut sec: Vec<String> = nodes
        .values()
        .filter(|n| {
            n.kind == "section"
                && n.symbol
                    .as_deref()
                    .is_some_and(|s| s.trim().eq_ignore_ascii_case(&t))
        })
        .map(|n| n.node_id.clone())
        .collect();
    sec.sort();
    sec.dedup();
    if sec.len() == 1 {
        return Some((sec[0].clone(), "section"));
    }

    None
}

pub(crate) fn resolve_thru_target(
    thru: &str,
    paragraph_index: &std::collections::BTreeMap<String, Vec<String>>,
    section_index: &std::collections::BTreeMap<String, Vec<String>>,
    nodes: &std::collections::BTreeMap<String, CodeNodeRecord>,
) -> Option<(String, &'static str)> {
    let (thru_node, thru_kind) = resolve_perform_target(thru, paragraph_index, section_index);
    thru_node
        .map(|id| (id, thru_kind))
        .or_else(|| resolve_label_in_nodes(thru, nodes))
}

pub(crate) fn parse_perform_thru_from_logical_line(line: &str) -> Option<String> {
    // Conservative lexer: extract `THRU <label>` or `THROUGH <label>` from the same logical line.
    // This is only used as a fallback when tree-sitter-graph capture does not provide `thru`.
    let up = line
        .replace("\r\n", "\n")
        .replace('\r', "\n")
        .to_ascii_uppercase();
    let mut toks = up.split_whitespace();
    while let Some(tok) = toks.next() {
        if tok == "THRU" || tok == "THROUGH" {
            return toks
                .next()
                .map(|s| s.trim_end_matches('.').to_string())
                .filter(|s| !s.is_empty());
        }
    }
    None
}

pub(crate) fn find_container_span_node_id(spans: &[(i64, i64, String)], line_no: i64) -> Option<String> {
    let mut best: Option<(i64, i64, String)> = None;
    for (s, e, nid) in spans {
        if *s <= line_no && line_no <= *e {
            match &best {
                None => best = Some((*s, *e, nid.clone())),
                Some((bs, be, _)) => {
                    if (e - s) < (be - bs) {
                        best = Some((*s, *e, nid.clone()));
                    }
                }
            }
        }
    }
    best.map(|(_, _, nid)| nid)
}

pub(crate) fn unresolved_node(
    project_id: &str,
    repo_fingerprint: &str,
    file_path: &str,
    symbol: &str,
    node_id: &str,
    extra_label: &str,
) -> CodeNodeRecord {
    let sh = snippet_hash(file_path, 1, 1, "");
    CodeNodeRecord {
        project_id: project_id.to_string(),
        repo_fingerprint: repo_fingerprint.to_string(),
        node_id: node_id.to_string(),
        language: CodeLanguage::Cobol,
        kind: "unresolved".to_string(),
        symbol: Some(symbol.to_string()),
        file_path: file_path.to_string(),
        start_line: 1,
        end_line: 1,
        snippet_hash: sh,
        text: String::new(),
        extra_labels: vec![extra_label.to_string()],
    }
}


