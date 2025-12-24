use crate::plugins::cobol::normalizer::PreprocessResult;
use crate::plugins::tsg::cobol_prepare_source;
use tree_sitter::{Node, Parser};

use super::regexes;

pub(crate) fn parse_cobol_tree(preprocess: &PreprocessResult) -> Option<(String, tree_sitter::Tree)> {
    let parse_stream = String::from_utf8_lossy(&preprocess.parse_bytes).to_string();
    let prepared = cobol_prepare_source(&parse_stream);

    let mut parser = Parser::new();
    parser.set_language(&tree_sitter_cobol::LANGUAGE.into()).ok()?;
    let tree = parser.parse(&prepared, None)?;
    Some((prepared, tree))
}

pub(crate) fn extract_divisions_treesitter(
    preprocess: &PreprocessResult,
    root: Node<'_>,
) -> Option<Vec<(String, String, i64, i64)>> {
    let eof = (preprocess.physical_lines.len() as i64).max(1);
    let mut starts: Vec<(String, i64)> = Vec::new(); // (DIVISION_NAME, physical_start_line)

    let mut stack: Vec<Node<'_>> = vec![root];
    while let Some(n) = stack.pop() {
        let sym = match n.kind() {
            "identification_division" => Some("IDENTIFICATION"),
            "environment_division" => Some("ENVIRONMENT"),
            "data_division" => Some("DATA"),
            "procedure_division" => Some("PROCEDURE"),
            _ => None,
        };
        if let Some(sym) = sym {
            let row = n.start_position().row;
            let start_line = preprocess.logical_spans.get(row)?.0;
            starts.push((sym.to_string(), start_line));
        }

        let mut c = n.walk();
        for ch in n.children(&mut c) {
            stack.push(ch);
        }
    }

    if starts.is_empty() {
        return None;
    }
    starts.sort_by(|a, b| a.1.cmp(&b.1).then_with(|| a.0.cmp(&b.0)));
    starts.dedup();

    let mut out: Vec<(String, String, i64, i64)> = Vec::new();
    for i in 0..starts.len() {
        let (sym, start) = &starts[i];
        let end = if i + 1 < starts.len() {
            (starts[i + 1].1 - 1).max(*start)
        } else {
            eof
        };
        out.push(("division".to_string(), sym.clone(), *start, end));
    }
    Some(out)
}

pub(crate) fn extract_paragraphs_treesitter(
    preprocess: &PreprocessResult,
    root: Node<'_>,
    source: &str,
    divisions: &[(String, String, i64, i64)],
) -> Option<Vec<(String, String, i64, i64)>> {
    let proc = divisions.iter().find(|(_, sym, _, _)| sym == "PROCEDURE");
    let Some((_k, _sym, _proc_start, proc_end)) = proc else {
        return None;
    };

    let mut hits: Vec<(String, i64)> = Vec::new(); // (PARA_NAME, physical_start_line)

    let mut stack: Vec<Node<'_>> = vec![root];
    while let Some(n) = stack.pop() {
        if n.kind() == "paragraph_header" {
            let name_node = n.child_by_field_name("name");
            let Some(name_node) = name_node else {
                continue;
            };
            let raw = name_node.utf8_text(source.as_bytes()).unwrap_or("").trim();
            if raw.is_empty() {
                continue;
            }
            let up = raw.to_ascii_uppercase();
            if regexes::reserved_para_names().contains(up.as_str()) {
                continue;
            }
            if up.ends_with("DIVISION") || up.ends_with("SECTION") || up == "PROGRAM-ID" {
                continue;
            }

            let row = n.start_position().row;
            let start_line = preprocess.logical_spans.get(row)?.0;
            hits.push((up, start_line));
        }

        let mut c = n.walk();
        for ch in n.children(&mut c) {
            stack.push(ch);
        }
    }

    if hits.is_empty() {
        return None;
    }
    hits.sort_by(|a, b| a.1.cmp(&b.1).then_with(|| a.0.cmp(&b.0)));
    hits.dedup();

    let mut out: Vec<(String, String, i64, i64)> = Vec::new();
    for i in 0..hits.len() {
        let (sym, start) = &hits[i];
        let end = if i + 1 < hits.len() {
            (hits[i + 1].1 - 1).max(*start)
        } else {
            *proc_end
        };
        out.push(("paragraph".to_string(), sym.clone(), *start, end));
    }
    Some(out)
}


