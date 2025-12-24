use crate::core::records::CodeNodeRecord;
use crate::core::stable_ids::{snippet_hash, stable_node_id};
use crate::core::types::CodeLanguage;
use crate::plugins::cobol::copybooks::CopybookInsertProvenance;
use crate::plugins::cobol::normalizer::PreprocessResult;
use crate::plugins::tsg::cobol_prepare_source;
use regex::Regex;
use tree_sitter::{Node, Parser};

#[derive(Debug, Clone)]
pub struct UnitizeResult {
    pub nodes: Vec<CodeNodeRecord>,
    pub used_treesitter: bool,
}

fn re_program_id() -> &'static Regex {
    static RE: std::sync::OnceLock<Regex> = std::sync::OnceLock::new();
    RE.get_or_init(|| Regex::new(r"(?i)\bPROGRAM-ID\.\s*([A-Za-z0-9_-]+)\b").unwrap())
}

pub fn unitize_cobol_file(
    project_id: &str,
    repo_fingerprint: &str,
    file_path: &str,
    preprocess: &PreprocessResult,
    copybook_expansion_map: &[CopybookInsertProvenance],
    _max_error_ratio: f64,
) -> UnitizeResult {
    let start_line = 1i64;
    let end_line = (preprocess.physical_lines.len() as i64).max(1);

    let parsed = parse_cobol_tree(preprocess);
    let symbol = parsed
        .as_ref()
        .and_then(|(prepared, tree)| find_first_program_name(tree.root_node(), prepared))
        .map(|s| s.trim().to_ascii_uppercase())
        .filter(|s| !s.is_empty())
        .or_else(|| extract_program_id(&preprocess.physical_lines));
    let used_treesitter = parsed.is_some();

    let text = slice_text(&preprocess.physical_lines, start_line, end_line);
    let node_id = stable_node_id([
        project_id,
        repo_fingerprint,
        file_path,
        "program",
        symbol.as_deref().unwrap_or(""),
        &start_line.to_string(),
        &end_line.to_string(),
    ]);
    let sh = snippet_hash(file_path, start_line, end_line, &text);

    let mut nodes = vec![CodeNodeRecord {
        project_id: project_id.to_string(),
        repo_fingerprint: repo_fingerprint.to_string(),
        node_id: node_id.clone(),
        language: CodeLanguage::Cobol,
        kind: "program".to_string(),
        symbol,
        file_path: file_path.to_string(),
        start_line,
        end_line,
        snippet_hash: sh,
        text,
        extra_labels: vec![],
    }];

    // Emit exec_block nodes from preprocessing markers.
    for (idx, ln) in preprocess.logical_lines.iter().enumerate() {
        if ln.trim().eq_ignore_ascii_case("EXEC_BLOCK.") {
            let (s, e) = preprocess.logical_spans[idx];
            let exec_text = slice_text(&preprocess.physical_lines, s, e);
            let exec_id = stable_node_id([
                project_id,
                repo_fingerprint,
                file_path,
                "exec_block",
                "",
                &s.to_string(),
                &e.to_string(),
            ]);
            let exec_sh = snippet_hash(file_path, s, e, &exec_text);
            nodes.push(CodeNodeRecord {
                project_id: project_id.to_string(),
                repo_fingerprint: repo_fingerprint.to_string(),
                node_id: exec_id,
                language: CodeLanguage::Cobol,
                kind: "exec_block".to_string(),
                symbol: None,
                file_path: file_path.to_string(),
                start_line: s,
                end_line: e,
                snippet_hash: exec_sh,
                text: exec_text,
                extra_labels: vec![],
            });
        }
    }

    // Emit copybook nodes from COPY expansion provenance (evidence points to the COPY statement span).
    nodes.extend(extract_copybook_nodes(
        project_id,
        repo_fingerprint,
        file_path,
        preprocess,
        copybook_expansion_map,
    ));

    // Tree-sitter-backed extraction for statements and data_description entries when parse is available.
    if let Some((prepared, tree)) = parsed.as_ref() {
        nodes.extend(extract_procedure_statements(
            project_id,
            repo_fingerprint,
            file_path,
            preprocess,
            prepared,
            tree.root_node(),
        ));
        nodes.extend(extract_data_items_from_ast(
            project_id,
            repo_fingerprint,
            file_path,
            preprocess,
            tree.root_node(),
        ));

        // Sentence segmentation is period-based and uses the logical→physical span map, scoped to PROCEDURE DIVISION.
        nodes.extend(extract_sentences(
            project_id,
            repo_fingerprint,
            file_path,
            preprocess,
            tree.root_node(),
        ));
    } else {
        // Minimal fallback: still attempt sentence segmentation from the logical stream.
        nodes.extend(extract_sentences_fallback(
            project_id,
            repo_fingerprint,
            file_path,
            preprocess,
        ));
        // Minimal fallback data items: regex scan in DATA DIVISION range.
        nodes.extend(extract_data_items_fallback(
            project_id,
            repo_fingerprint,
            file_path,
            preprocess,
        ));
    }

    // Deterministic de-dup by node_id.
    let mut uniq: std::collections::BTreeMap<String, CodeNodeRecord> = std::collections::BTreeMap::new();
    for n in nodes {
        uniq.insert(n.node_id.clone(), n);
    }

    UnitizeResult {
        nodes: uniq.into_values().collect(),
        used_treesitter,
    }
}

fn parse_cobol_tree(preprocess: &PreprocessResult) -> Option<(String, tree_sitter::Tree)> {
    let parse_stream = String::from_utf8_lossy(&preprocess.parse_bytes).to_string();
    let prepared = cobol_prepare_source(&parse_stream);

    let mut parser = Parser::new();
    parser.set_language(&tree_sitter_cobol::LANGUAGE.into()).ok()?;
    let tree = parser.parse(&prepared, None)?;
    Some((prepared, tree))
}

fn find_first_program_name(root: Node<'_>, source: &str) -> Option<String> {
    let mut stack: Vec<Node<'_>> = vec![root];
    while let Some(n) = stack.pop() {
        if n.kind() == "program_name" {
            return n.utf8_text(source.as_bytes()).ok().map(|s| s.to_string());
        }
        let mut c = n.walk();
        for ch in n.children(&mut c) {
            stack.push(ch);
        }
    }
    None
}

fn extract_program_id(physical_lines: &[String]) -> Option<String> {
    for line in physical_lines {
        if let Some(cap) = re_program_id().captures(line) {
            if let Some(m) = cap.get(1) {
                return Some(m.as_str().to_string());
            }
        }
    }
    None
}

fn slice_text(lines: &[String], start_line: i64, end_line: i64) -> String {
    let s = start_line.max(1);
    let e = end_line.min(lines.len() as i64);
    if e < s {
        return String::new();
    }
    let start_idx = (s - 1) as usize;
    let end_idx = (e - 1) as usize;
    lines[start_idx..=end_idx].join("\n") + "\n"
}

fn reserved_para_names() -> &'static std::collections::HashSet<&'static str> {
    static SET: std::sync::OnceLock<std::collections::HashSet<&'static str>> = std::sync::OnceLock::new();
    SET.get_or_init(|| {
        [
            "ACCEPT", "ADD", "CALL", "CANCEL", "CLOSE", "COMPUTE", "CONTINUE", "DELETE", "DIVIDE", "DISPLAY", "ELSE",
            "END", "END-IF", "END-READ", "END-WRITE", "EVALUATE", "EXEC", "EXIT", "GOBACK", "GO", "IF", "INITIALIZE",
            "INSPECT", "MERGE", "MOVE", "MULTIPLY", "OPEN", "PERFORM", "READ", "RETURN", "REWRITE", "SEARCH", "SET",
            "SORT", "START", "STOP", "STRING", "SUBTRACT", "UNSTRING", "WHEN", "WRITE",
        ]
        .into_iter()
        .collect()
    })
}

fn normalize_end_row_for_node(n: Node<'_>) -> usize {
    let start = n.start_position().row;
    let mut end = n.end_position().row;
    if n.end_position().column == 0 && end > start {
        end = end.saturating_sub(1);
    }
    end
}

fn physical_span_for_node(preprocess: &PreprocessResult, n: Node<'_>) -> Option<(i64, i64)> {
    let start_row = n.start_position().row;
    let end_row = normalize_end_row_for_node(n);
    if preprocess.logical_spans.is_empty() {
        return None;
    }
    let end_row = end_row.min(preprocess.logical_spans.len().saturating_sub(1));
    let start_row = start_row.min(end_row);

    let mut s = i64::MAX;
    let mut e = i64::MIN;
    for row in start_row..=end_row {
        let (ps, pe) = preprocess.logical_spans.get(row).copied()?;
        s = s.min(ps);
        e = e.max(pe);
    }
    if s == i64::MAX || e == i64::MIN {
        None
    } else {
        Some((s, e))
    }
}

fn find_first_node_kind<'a>(root: Node<'a>, kind: &str) -> Option<Node<'a>> {
    let mut stack: Vec<Node<'a>> = vec![root];
    while let Some(n) = stack.pop() {
        if n.kind() == kind {
            return Some(n);
        }
        let mut c = n.walk();
        for ch in n.children(&mut c) {
            stack.push(ch);
        }
    }
    None
}

fn extract_copybook_nodes(
    project_id: &str,
    repo_fingerprint: &str,
    file_path: &str,
    preprocess: &PreprocessResult,
    expansion_map: &[CopybookInsertProvenance],
) -> Vec<CodeNodeRecord> {
    let mut out: Vec<CodeNodeRecord> = Vec::new();
    for ins in expansion_map {
        let idx0 = ins.inserted_at_logical_line.saturating_sub(1) as usize;
        let Some((s, e)) = preprocess.logical_spans.get(idx0).copied() else {
            continue;
        };
        let text = slice_text(&preprocess.physical_lines, s, e);
        let nid = stable_node_id([
            project_id,
            repo_fingerprint,
            file_path,
            "copybook",
            ins.copybook_path.as_str(),
            &s.to_string(),
            &e.to_string(),
        ]);
        out.push(CodeNodeRecord {
            project_id: project_id.to_string(),
            repo_fingerprint: repo_fingerprint.to_string(),
            node_id: nid,
            language: CodeLanguage::Cobol,
            kind: "copybook".to_string(),
            symbol: Some(ins.copybook_path.clone()),
            file_path: file_path.to_string(),
            start_line: s,
            end_line: e,
            snippet_hash: snippet_hash(file_path, s, e, &text),
            text,
            extra_labels: vec![],
        });
    }
    out
}

fn extract_procedure_statements(
    project_id: &str,
    repo_fingerprint: &str,
    file_path: &str,
    preprocess: &PreprocessResult,
    source: &str,
    root: Node<'_>,
) -> Vec<CodeNodeRecord> {
    let Some(proc) = find_first_node_kind(root, "procedure_division") else {
        return vec![];
    };

    let mut out: Vec<CodeNodeRecord> = Vec::new();

    // Traverse the whole PROCEDURE DIVISION subtree and collect all concrete `*_statement` nodes.
    // This is more robust than only looking at direct children (many statements are nested).
    let mut candidates: Vec<(i64, i64, String)> = Vec::new(); // (start_line, end_line, kind)
    let mut stack: Vec<Node<'_>> = vec![proc];
    while let Some(n) = stack.pop() {
        let k = n.kind();
        if k == "paragraph_header" || k == "_end_statement" {
            // Not a statement unit.
        } else if k.ends_with("_statement") {
            if let Some((s, e)) = physical_span_for_node(preprocess, n) {
                candidates.push((s, e, k.to_string()));
            }
        }
        let mut c = n.walk();
        for ch in n.children(&mut c) {
            stack.push(ch);
        }
    }

    candidates.sort_by(|a, b| (a.0, a.1, a.2.clone()).cmp(&(b.0, b.1, b.2.clone())));
    candidates.dedup();

    for (s, e, stmt_kind) in candidates {
        let text = slice_text(&preprocess.physical_lines, s, e);
        let stmt_symbol = stmt_kind.clone();
        let nid = stable_node_id([
            project_id,
            repo_fingerprint,
            file_path,
            "statement",
            stmt_symbol.as_str(),
            &s.to_string(),
            &e.to_string(),
        ]);
        let _ = source; // reserved for future: verb extraction from `source` + AST span
        out.push(CodeNodeRecord {
            project_id: project_id.to_string(),
            repo_fingerprint: repo_fingerprint.to_string(),
            node_id: nid,
            language: CodeLanguage::Cobol,
            kind: "statement".to_string(),
            symbol: Some(stmt_symbol),
            file_path: file_path.to_string(),
            start_line: s,
            end_line: e,
            snippet_hash: snippet_hash(file_path, s, e, &text),
            text,
            extra_labels: vec![],
        });
    }
    out
}

fn re_data_item_line() -> &'static Regex {
    static RE: std::sync::OnceLock<Regex> = std::sync::OnceLock::new();
    RE.get_or_init(|| Regex::new(r"(?i)^\s*(\d{2}|66|77|88)\s+([A-Za-z0-9_-]+)\b").unwrap())
}

fn extract_data_items_from_ast(
    project_id: &str,
    repo_fingerprint: &str,
    file_path: &str,
    preprocess: &PreprocessResult,
    root: Node<'_>,
) -> Vec<CodeNodeRecord> {
    let mut out: Vec<CodeNodeRecord> = Vec::new();
    let mut stack: Vec<Node<'_>> = vec![root];
    while let Some(n) = stack.pop() {
        if n.kind() == "data_description" {
            if let Some((s, e)) = physical_span_for_node(preprocess, n) {
                let text = slice_text(&preprocess.physical_lines, s, e);
                let symbol = text
                    .lines()
                    .next()
                    .and_then(|ln| re_data_item_line().captures(ln).and_then(|c| c.get(2)).map(|m| m.as_str().to_ascii_uppercase()));
                if let Some(symbol) = symbol {
                    let nid = stable_node_id([
                        project_id,
                        repo_fingerprint,
                        file_path,
                        "data_item",
                        symbol.as_str(),
                        &s.to_string(),
                        &e.to_string(),
                    ]);
                    out.push(CodeNodeRecord {
                        project_id: project_id.to_string(),
                        repo_fingerprint: repo_fingerprint.to_string(),
                        node_id: nid,
                        language: CodeLanguage::Cobol,
                        kind: "data_item".to_string(),
                        symbol: Some(symbol),
                        file_path: file_path.to_string(),
                        start_line: s,
                        end_line: e,
                        snippet_hash: snippet_hash(file_path, s, e, &text),
                        text,
                        extra_labels: vec![],
                    });
                }
            }
        }
        let mut c = n.walk();
        for ch in n.children(&mut c) {
            stack.push(ch);
        }
    }
    out
}

fn extract_data_items_fallback(
    project_id: &str,
    repo_fingerprint: &str,
    file_path: &str,
    preprocess: &PreprocessResult,
) -> Vec<CodeNodeRecord> {
    // Conservative: only inside DATA DIVISION .. PROCEDURE DIVISION.
    let mut data_start: Option<i64> = None;
    let mut proc_start: Option<i64> = None;
    for (i, line) in preprocess.physical_lines.iter().enumerate() {
        let up = line.to_ascii_uppercase();
        if data_start.is_none() && up.contains("DATA DIVISION") {
            data_start = Some((i as i64) + 1);
        }
        if proc_start.is_none() && up.contains("PROCEDURE DIVISION") {
            proc_start = Some((i as i64) + 1);
        }
    }
    let Some(ds) = data_start else { return vec![] };
    let eof = (preprocess.physical_lines.len() as i64).max(1);
    let de = proc_start.map(|p| p - 1).unwrap_or(eof);
    if de < ds {
        return vec![];
    }

    let mut out: Vec<CodeNodeRecord> = Vec::new();
    for line_no in ds..=de {
        let line = &preprocess.physical_lines[(line_no - 1) as usize];
        let Some(cap) = re_data_item_line().captures(line) else { continue };
        let name = cap.get(2).map(|m| m.as_str().to_ascii_uppercase()).unwrap_or_default();
        if name.is_empty() {
            continue;
        }
        let text = slice_text(&preprocess.physical_lines, line_no, line_no);
        let nid = stable_node_id([
            project_id,
            repo_fingerprint,
            file_path,
            "data_item",
            name.as_str(),
            &line_no.to_string(),
            &line_no.to_string(),
        ]);
        out.push(CodeNodeRecord {
            project_id: project_id.to_string(),
            repo_fingerprint: repo_fingerprint.to_string(),
            node_id: nid,
            language: CodeLanguage::Cobol,
            kind: "data_item".to_string(),
            symbol: Some(name),
            file_path: file_path.to_string(),
            start_line: line_no,
            end_line: line_no,
            snippet_hash: snippet_hash(file_path, line_no, line_no, &text),
            text,
            extra_labels: vec![],
        });
    }
    out
}

fn is_paragraph_header_candidate(line: &str) -> bool {
    let stripped = line.trim();
    if !stripped.ends_with('.') {
        return false;
    }
    let token = stripped.trim_end_matches('.').trim();
    if token.is_empty() || token.contains(' ') {
        return false;
    }
    let up = token.to_ascii_uppercase();
    if reserved_para_names().contains(up.as_str()) {
        return false;
    }
    if up.ends_with("DIVISION") || up.ends_with("SECTION") || up == "PROGRAM-ID" {
        return false;
    }
    true
}

fn physical_span_for_logical_range(preprocess: &PreprocessResult, start_idx: usize, end_idx: usize) -> Option<(i64, i64)> {
    if start_idx > end_idx || preprocess.logical_spans.is_empty() {
        return None;
    }
    let end_idx = end_idx.min(preprocess.logical_spans.len().saturating_sub(1));
    let start_idx = start_idx.min(end_idx);
    let mut s = i64::MAX;
    let mut e = i64::MIN;
    for idx in start_idx..=end_idx {
        let (ps, pe) = preprocess.logical_spans.get(idx).copied()?;
        s = s.min(ps);
        e = e.max(pe);
    }
    if s == i64::MAX || e == i64::MIN {
        None
    } else {
        Some((s, e))
    }
}

fn extract_sentences(
    project_id: &str,
    repo_fingerprint: &str,
    file_path: &str,
    preprocess: &PreprocessResult,
    root: Node<'_>,
) -> Vec<CodeNodeRecord> {
    let Some(proc) = find_first_node_kind(root, "procedure_division") else {
        return vec![];
    };
    let Some((proc_s, proc_e)) = physical_span_for_node(preprocess, proc) else {
        return vec![];
    };

    // Prefer Tree-sitter’s own `*_end_statement` punctuation nodes as sentence terminators, if present.
    // This avoids treating arbitrary dots in non-procedure text as sentence ends.
    let mut end_lines: Vec<i64> = Vec::new();
    let mut stack: Vec<Node<'_>> = vec![proc];
    while let Some(n) = stack.pop() {
        let k = n.kind();
        if k == "_end_statement" || k == "end_statement" {
            if let Some((s, e)) = physical_span_for_node(preprocess, n) {
                // end_statement should be a point; keep a stable end line.
                end_lines.push(e.max(s));
            }
        }
        let mut c = n.walk();
        for ch in n.children(&mut c) {
            stack.push(ch);
        }
    }
    end_lines.sort();
    end_lines.dedup();

    if !end_lines.is_empty() {
        let mut out: Vec<CodeNodeRecord> = Vec::new();
        let mut cur_start: Option<i64> = None;
        for end_line in end_lines {
            if !(proc_s <= end_line && end_line <= proc_e) {
                continue;
            }
            if cur_start.is_none() {
                cur_start = Some(proc_s);
            }
            let s = cur_start.unwrap();
            let e = end_line;
            if e < s {
                continue;
            }
            let text = slice_text(&preprocess.physical_lines, s, e);
            let nid = stable_node_id([
                project_id,
                repo_fingerprint,
                file_path,
                "sentence",
                "",
                &s.to_string(),
                &e.to_string(),
            ]);
            out.push(CodeNodeRecord {
                project_id: project_id.to_string(),
                repo_fingerprint: repo_fingerprint.to_string(),
                node_id: nid,
                language: CodeLanguage::Cobol,
                kind: "sentence".to_string(),
                symbol: None,
                file_path: file_path.to_string(),
                start_line: s,
                end_line: e,
                snippet_hash: snippet_hash(file_path, s, e, &text),
                text,
                extra_labels: vec![],
            });
            cur_start = Some((e + 1).min(proc_e));
        }
        return out;
    }

    let mut logical_idxs: Vec<usize> = Vec::new();
    for (idx, (s, e)) in preprocess.logical_spans.iter().enumerate() {
        if *s >= proc_s && *e <= proc_e {
            logical_idxs.push(idx);
        }
    }
    if logical_idxs.is_empty() {
        return vec![];
    }

    let mut out: Vec<CodeNodeRecord> = Vec::new();
    let mut cur_start: Option<usize> = None;
    for idx in logical_idxs {
        let line = preprocess.logical_lines.get(idx).map(|s| s.as_str()).unwrap_or("");
        let trimmed = line.trim();
        if trimmed.is_empty() {
            continue;
        }
        if trimmed.eq_ignore_ascii_case("EXEC_BLOCK.") {
            // Keep exec_block as its own node; do not force it into statement/sentence boundaries.
            continue;
        }
        if is_paragraph_header_candidate(trimmed) {
            // Paragraph boundary: flush any open sentence.
            if let Some(sidx) = cur_start.take() {
                let eidx = idx.saturating_sub(1);
                if let Some((ps, pe)) = physical_span_for_logical_range(preprocess, sidx, eidx) {
                    let text = slice_text(&preprocess.physical_lines, ps, pe);
                    let nid = stable_node_id([
                        project_id,
                        repo_fingerprint,
                        file_path,
                        "sentence",
                        "",
                        &ps.to_string(),
                        &pe.to_string(),
                    ]);
                    out.push(CodeNodeRecord {
                        project_id: project_id.to_string(),
                        repo_fingerprint: repo_fingerprint.to_string(),
                        node_id: nid,
                        language: CodeLanguage::Cobol,
                        kind: "sentence".to_string(),
                        symbol: None,
                        file_path: file_path.to_string(),
                        start_line: ps,
                        end_line: pe,
                        snippet_hash: snippet_hash(file_path, ps, pe, &text),
                        text,
                        extra_labels: vec![],
                    });
                }
            }
            continue;
        }
        if cur_start.is_none() {
            cur_start = Some(idx);
        }
        if trimmed.ends_with('.') {
            // Period terminates a COBOL sentence (conservative heuristic).
            if let Some(sidx) = cur_start.take() {
                if let Some((ps, pe)) = physical_span_for_logical_range(preprocess, sidx, idx) {
                    let text = slice_text(&preprocess.physical_lines, ps, pe);
                    let nid = stable_node_id([
                        project_id,
                        repo_fingerprint,
                        file_path,
                        "sentence",
                        "",
                        &ps.to_string(),
                        &pe.to_string(),
                    ]);
                    out.push(CodeNodeRecord {
                        project_id: project_id.to_string(),
                        repo_fingerprint: repo_fingerprint.to_string(),
                        node_id: nid,
                        language: CodeLanguage::Cobol,
                        kind: "sentence".to_string(),
                        symbol: None,
                        file_path: file_path.to_string(),
                        start_line: ps,
                        end_line: pe,
                        snippet_hash: snippet_hash(file_path, ps, pe, &text),
                        text,
                        extra_labels: vec![],
                    });
                }
            }
        }
    }

    out
}

fn extract_sentences_fallback(
    project_id: &str,
    repo_fingerprint: &str,
    file_path: &str,
    preprocess: &PreprocessResult,
) -> Vec<CodeNodeRecord> {
    // Fallback: treat everything after first "PROCEDURE DIVISION" logical line as procedure text.
    let mut proc_idx: Option<usize> = None;
    for (i, ln) in preprocess.logical_lines.iter().enumerate() {
        if ln.to_ascii_uppercase().contains("PROCEDURE DIVISION") {
            proc_idx = Some(i);
            break;
        }
    }
    let Some(start_idx) = proc_idx else { return vec![] };

    let mut out: Vec<CodeNodeRecord> = Vec::new();
    let mut cur_start: Option<usize> = None;
    for idx in start_idx..preprocess.logical_lines.len() {
        let line = preprocess.logical_lines.get(idx).map(|s| s.as_str()).unwrap_or("");
        let trimmed = line.trim();
        if trimmed.is_empty() || trimmed.eq_ignore_ascii_case("EXEC_BLOCK.") {
            continue;
        }
        if is_paragraph_header_candidate(trimmed) {
            cur_start = None;
            continue;
        }
        if cur_start.is_none() {
            cur_start = Some(idx);
        }
        if trimmed.ends_with('.') {
            if let Some(sidx) = cur_start.take() {
                if let Some((ps, pe)) = physical_span_for_logical_range(preprocess, sidx, idx) {
                    let text = slice_text(&preprocess.physical_lines, ps, pe);
                    let nid = stable_node_id([
                        project_id,
                        repo_fingerprint,
                        file_path,
                        "sentence",
                        "",
                        &ps.to_string(),
                        &pe.to_string(),
                    ]);
                    out.push(CodeNodeRecord {
                        project_id: project_id.to_string(),
                        repo_fingerprint: repo_fingerprint.to_string(),
                        node_id: nid,
                        language: CodeLanguage::Cobol,
                        kind: "sentence".to_string(),
                        symbol: None,
                        file_path: file_path.to_string(),
                        start_line: ps,
                        end_line: pe,
                        snippet_hash: snippet_hash(file_path, ps, pe, &text),
                        text,
                        extra_labels: vec![],
                    });
                }
            }
        }
    }
    out
}


