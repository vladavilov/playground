use crate::core::records::{CodeNodeRecord, EdgeRecord};
use crate::core::stable_ids::{snippet_hash, stable_node_id};
use crate::core::types::{CodeLanguage, CodeRelType};
use crate::plugins::cobol::normalizer::PreprocessResult;
use crate::plugins::tsg;
use serde_json::Value;

mod cobol_tree;
mod data_defs;
mod divisions;
pub(crate) mod exec;
mod regexes;
mod resolution;
mod text;

#[derive(Debug, Clone)]
pub struct CobolSemanticResult {
    pub nodes: Vec<CodeNodeRecord>,
    pub edges: Vec<EdgeRecord>,
    pub facts: serde_json::Map<String, Value>,
}

pub fn link_cobol_semantics(
    project_id: &str,
    repo_fingerprint: &str,
    file_path: &str,
    preprocess: &PreprocessResult,
    program_node_id: &str,
) -> CobolSemanticResult {
    let physical_lines = &preprocess.physical_lines;

    // Prefer tree-sitter-cobol for high-fidelity structure (divisions + paragraph headers),
    // but keep regex-based fallback to remain robust for partial / malformed inputs.
    let parsed = cobol_tree::parse_cobol_tree(preprocess);
    let divisions_ts = parsed
        .as_ref()
        .and_then(|(_prepared, tree)| {
            cobol_tree::extract_divisions_treesitter(preprocess, tree.root_node())
        });
    let used_treesitter_divisions = divisions_ts.is_some();
    let divisions = divisions_ts.unwrap_or_else(|| divisions::extract_divisions_from_logical(preprocess));

    // Sections are only used for data storage scoping; keep them in the same "logical regex" lane
    // so FIXED-format physical prefixes don't break recognition.
    let sections = divisions::extract_sections_from_logical(preprocess, &divisions);

    let paragraphs_ts = parsed
        .as_ref()
        .and_then(|(prepared, tree)| {
            cobol_tree::extract_paragraphs_treesitter(
                preprocess,
                tree.root_node(),
                prepared,
                &divisions,
            )
        });
    let used_treesitter_paragraphs = paragraphs_ts.is_some();
    let paragraphs =
        paragraphs_ts.unwrap_or_else(|| divisions::extract_paragraphs_from_logical(preprocess, &divisions));

    let structure_extraction = match (used_treesitter_divisions, used_treesitter_paragraphs) {
        (true, true) => "treesitter",
        (false, false) => "regex",
        _ => "mixed",
    };

    let mut nodes: std::collections::BTreeMap<String, CodeNodeRecord> = std::collections::BTreeMap::new();

    for (kind, sym, start, end) in divisions.iter().chain(sections.iter()).chain(paragraphs.iter())
    {
        let text = text::slice_text(physical_lines, *start, *end);
        let nid = stable_node_id([
            project_id,
            repo_fingerprint,
            file_path,
            kind,
            sym,
            &start.to_string(),
            &end.to_string(),
        ]);
        nodes.insert(
            nid.clone(),
            CodeNodeRecord {
                project_id: project_id.to_string(),
                repo_fingerprint: repo_fingerprint.to_string(),
                node_id: nid,
                language: CodeLanguage::Cobol,
                kind: kind.to_string(),
                symbol: Some(sym.to_string()),
                file_path: file_path.to_string(),
                start_line: *start,
                end_line: *end,
                snippet_hash: snippet_hash(file_path, *start, *end, &text),
                text,
                extra_labels: vec![],
            },
        );
    }

    let (data_nodes, defs_by_name) =
        data_defs::extract_data_defs(project_id, repo_fingerprint, file_path, physical_lines, &divisions, &sections);
    for n in data_nodes {
        nodes.insert(n.node_id.clone(), n);
    }

    let para_spans: Vec<(i64, i64, String)> = nodes
        .values()
        .filter(|n| n.kind == "paragraph")
        .map(|n| (n.start_line, n.end_line, n.node_id.clone()))
        .collect();

    let mut edges: Vec<EdgeRecord> = Vec::new();

    // Pass 2: PERFORMS
    let (paragraph_index, section_index) = resolution::build_label_indexes(&nodes);

    let parse_stream = String::from_utf8_lossy(&preprocess.parse_bytes).to_string();
    let extracted = tsg::extract_cobol(&parse_stream).unwrap_or(tsg::CobolExtract {
        calls: vec![],
        performs: vec![],
        io: vec![],
    });

    for p in extracted.performs.iter() {
        let line_no = preprocess
            .logical_spans
            .get(p.logical_row)
            .map(|(s, _e)| *s)
            .unwrap_or(1);
        let src_node = resolution::find_container_span_node_id(&para_spans, line_no)
            .unwrap_or_else(|| program_node_id.to_string());

        // If tree-sitter-graph didn't provide THRU/THROUGH, recover it from the logical line text.
        let thru_fallback = p.thru.clone().or_else(|| {
            resolution::parse_perform_thru_from_logical_line(
                preprocess
                    .logical_lines
                    .get(p.logical_row)
                    .map(|s| s.as_str())
                    .unwrap_or(""),
            )
        });

        let (dst_node, dst_kind) =
            resolution::resolve_perform_target(&p.target, &paragraph_index, &section_index);
        if let Some(dst) = dst_node {
            let mut md = serde_json::Map::new();
            md.insert("target".to_string(), Value::String(p.target.clone()));
            md.insert(
                "thru".to_string(),
                thru_fallback.clone().map(Value::String).unwrap_or(Value::Null),
            );
            md.insert("kind".to_string(), Value::String(dst_kind.to_string()));

            // Minimal fine-graining: if a THRU/THROUGH label exists, attempt to resolve it as well
            // and attach the resolved node id/kind for downstream consumers (no extra edges/nodes).
            if let Some(thru) = thru_fallback.as_deref() {
                if let Some((tnid, tk)) = resolution::resolve_thru_target(
                    thru,
                    &paragraph_index,
                    &section_index,
                    &nodes,
                ) {
                    md.insert("thru_kind".to_string(), Value::String(tk.to_string()));
                    md.insert("thru_node_id".to_string(), Value::String(tnid));
                }
            }
            edges.push(EdgeRecord {
                project_id: project_id.to_string(),
                repo_fingerprint: repo_fingerprint.to_string(),
                rel_type: CodeRelType::Performs,
                src_node_id: src_node,
                dst_node_id: dst,
                confidence: 1.0,
                metadata: md,
            });
        } else {
            let unresolved_id = stable_node_id([
                project_id,
                repo_fingerprint,
                file_path,
                "unresolved",
                "perform",
                &p.target,
            ]);
            nodes.entry(unresolved_id.clone()).or_insert_with(|| {
                resolution::unresolved_node(
                    project_id,
                    repo_fingerprint,
                    file_path,
                    &p.target,
                    &unresolved_id,
                    "__UnresolvedPerform__",
                )
            });
            let mut md = serde_json::Map::new();
            md.insert("target".to_string(), Value::String(p.target.clone()));
            md.insert(
                "thru".to_string(),
                thru_fallback.clone().map(Value::String).unwrap_or(Value::Null),
            );
            md.insert("kind".to_string(), Value::String("unknown".to_string()));
            if let Some(thru) = thru_fallback.as_deref() {
                if let Some((tnid, tk)) = resolution::resolve_thru_target(
                    thru,
                    &paragraph_index,
                    &section_index,
                    &nodes,
                ) {
                    md.insert("thru_kind".to_string(), Value::String(tk.to_string()));
                    md.insert("thru_node_id".to_string(), Value::String(tnid));
                }
            }
            edges.push(EdgeRecord {
                project_id: project_id.to_string(),
                repo_fingerprint: repo_fingerprint.to_string(),
                rel_type: CodeRelType::Performs,
                src_node_id: src_node,
                dst_node_id: unresolved_id,
                confidence: 0.2,
                metadata: md,
            });
        }
    }

    // Pass 2: CALLS
    for c in extracted.calls.iter() {
        let line_no = preprocess
            .logical_spans
            .get(c.logical_row)
            .map(|(s, _e)| *s)
            .unwrap_or(1);
        let src_node = resolution::find_container_span_node_id(&para_spans, line_no)
            .unwrap_or_else(|| program_node_id.to_string());

        let unresolved_id = stable_node_id([
            project_id,
            repo_fingerprint,
            file_path,
            "unresolved",
            "call",
            &c.callee,
        ]);
        nodes.entry(unresolved_id.clone()).or_insert_with(|| {
            resolution::unresolved_node(
                project_id,
                repo_fingerprint,
                file_path,
                &c.callee,
                &unresolved_id,
                "__UnresolvedCall__",
            )
        });
        let mut md = serde_json::Map::new();
        md.insert("callee".to_string(), Value::String(c.callee.clone()));
        md.insert("call_type".to_string(), Value::String(c.call_type.to_string()));
        edges.push(EdgeRecord {
            project_id: project_id.to_string(),
            repo_fingerprint: repo_fingerprint.to_string(),
            rel_type: CodeRelType::Calls,
            src_node_id: src_node,
            dst_node_id: unresolved_id,
            confidence: 0.2,
            metadata: md,
        });
    }

    // Pass 2: READS / WRITES (file/record I/O)
    for io in extracted.io.iter() {
        let line_no = preprocess
            .logical_spans
            .get(io.logical_row)
            .map(|(s, _e)| *s)
            .unwrap_or(1);
        let src_node = resolution::find_container_span_node_id(&para_spans, line_no)
            .unwrap_or_else(|| program_node_id.to_string());

        let target_sym = io.target.trim().to_ascii_uppercase();
        if target_sym.is_empty() {
            continue;
        }
        let unresolved_id = stable_node_id([
            project_id,
            repo_fingerprint,
            file_path,
            "unresolved",
            "io",
            io.op,
            &target_sym,
        ]);
        let label = if io.op == "READ" || io.op == "DELETE" {
            "__UnresolvedFile__"
        } else {
            "__UnresolvedRecord__"
        };
        nodes.entry(unresolved_id.clone()).or_insert_with(|| {
            resolution::unresolved_node(
                project_id,
                repo_fingerprint,
                file_path,
                &target_sym,
                &unresolved_id,
                label,
            )
        });

        let mut md = serde_json::Map::new();
        md.insert("op".to_string(), Value::String(io.op.to_string()));
        md.insert("target".to_string(), Value::String(target_sym.clone()));

        let (rel, conf) = if io.op == "READ" {
            (CodeRelType::Reads, 0.8)
        } else {
            (CodeRelType::Writes, 0.7)
        };
        edges.push(EdgeRecord {
            project_id: project_id.to_string(),
            repo_fingerprint: repo_fingerprint.to_string(),
            rel_type: rel,
            src_node_id: src_node,
            dst_node_id: unresolved_id,
            confidence: conf,
            metadata: md,
        });
    }

    // Pass 2: EXEC-derived edges (SQL table IO, CICS call modeling)
    for (s, e, exec_text) in exec::iter_exec_blocks(preprocess) {
        let header = regexes::re_exec_header()
            .captures(&exec_text)
            .and_then(|c| c.get(1))
            .map(|m| m.as_str().to_ascii_uppercase());
        let Some(kind) = header else {
            continue;
        };

        let src_node =
            resolution::find_container_span_node_id(&para_spans, s).unwrap_or_else(|| program_node_id.to_string());

        if kind == "SQL" {
            for (sql_op, table, access) in exec::extract_exec_sql_table_ops(&exec_text) {
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
                nodes.entry(nid.clone()).or_insert_with(|| CodeNodeRecord {
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
                md.insert("span".to_string(), Value::String(format!("{s}:{e}")));

                edges.push(EdgeRecord {
                    project_id: project_id.to_string(),
                    repo_fingerprint: repo_fingerprint.to_string(),
                    rel_type: access,
                    src_node_id: src_node.clone(),
                    dst_node_id: nid,
                    confidence: 0.6,
                    metadata: md,
                });
            }
            continue;
        }

        if kind == "CICS" {
            exec::push_exec_cics_edges(
                project_id,
                repo_fingerprint,
                file_path,
                &exec_text,
                &src_node,
                &mut nodes,
                &mut edges,
            );
        }
    }

    // Pass 2: REFERENCES (qualified-only MVP)
    for (i, line) in physical_lines.iter().enumerate() {
        let line_no = (i as i64) + 1;
        for m in regexes::re_qualified_ref().find_iter(line) {
            let (ref_name, qualifiers) = data_defs::parse_qualified_ref(m.as_str());
            if ref_name.is_empty() || qualifiers.is_empty() {
                continue;
            }
            let src_node = resolution::find_container_span_node_id(&para_spans, line_no)
                .unwrap_or_else(|| program_node_id.to_string());
            let resolved = data_defs::resolve_data_ref(&ref_name, &qualifiers, &defs_by_name);
            if let Some(dst) = resolved {
                let mut md = serde_json::Map::new();
                md.insert("name".to_string(), Value::String(ref_name.clone()));
                md.insert(
                    "qualifiers".to_string(),
                    Value::Array(qualifiers.iter().map(|q| Value::String(q.clone())).collect()),
                );
                edges.push(EdgeRecord {
                    project_id: project_id.to_string(),
                    repo_fingerprint: repo_fingerprint.to_string(),
                    rel_type: CodeRelType::References,
                    src_node_id: src_node,
                    dst_node_id: dst,
                    confidence: 1.0,
                    metadata: md,
                });
            } else {
                let unresolved_symbol = format!("{} OF {}", ref_name, qualifiers.join(" OF "));
                let unresolved_id = stable_node_id([
                    project_id,
                    repo_fingerprint,
                    file_path,
                    "unresolved",
                    "data_ref",
                    &unresolved_symbol,
                ]);
                nodes.entry(unresolved_id.clone()).or_insert_with(|| {
                    resolution::unresolved_node(
                        project_id,
                        repo_fingerprint,
                        file_path,
                        &unresolved_symbol,
                        &unresolved_id,
                        "__UnresolvedDataRef__",
                    )
                });
                let mut md = serde_json::Map::new();
                md.insert("name".to_string(), Value::String(ref_name.clone()));
                md.insert(
                    "qualifiers".to_string(),
                    Value::Array(qualifiers.iter().map(|q| Value::String(q.clone())).collect()),
                );
                md.insert(
                    "candidate_count".to_string(),
                    Value::Number((data_defs::candidate_count(&ref_name, &defs_by_name) as i64).into()),
                );
                edges.push(EdgeRecord {
                    project_id: project_id.to_string(),
                    repo_fingerprint: repo_fingerprint.to_string(),
                    rel_type: CodeRelType::References,
                    src_node_id: src_node,
                    dst_node_id: unresolved_id,
                    confidence: 0.2,
                    metadata: md,
                });
            }
        }
    }

    // De-dup deterministically.
    let mut uniq_edges: std::collections::BTreeMap<(CodeRelType, String, String, String), EdgeRecord> =
        std::collections::BTreeMap::new();
    for e in edges {
        let md_key = serde_json::to_string(&e.metadata).unwrap_or_default();
        uniq_edges.insert((e.rel_type, e.src_node_id.clone(), e.dst_node_id.clone(), md_key), e);
    }

    let mut facts = serde_json::Map::new();
    facts.insert(
        "structure_extraction".to_string(),
        Value::String(structure_extraction.to_string()),
    );
    facts.insert(
        "paragraph_count".to_string(),
        Value::Number(
            (nodes.values().filter(|n| n.kind == "paragraph").count() as i64).into(),
        ),
    );
    facts.insert(
        "data_def_count".to_string(),
        Value::Number((nodes.values().filter(|n| n.kind == "data_def").count() as i64).into()),
    );
    facts.insert(
        "performs_count".to_string(),
        Value::Number(
            (uniq_edges
                .values()
                .filter(|e| e.rel_type == CodeRelType::Performs)
                .count() as i64)
                .into(),
        ),
    );
    facts.insert(
        "references_count".to_string(),
        Value::Number(
            (uniq_edges
                .values()
                .filter(|e| e.rel_type == CodeRelType::References)
                .count() as i64)
                .into(),
        ),
    );
    facts.insert(
        "reads_count".to_string(),
        Value::Number(
            (uniq_edges
                .values()
                .filter(|e| e.rel_type == CodeRelType::Reads)
                .count() as i64)
                .into(),
        ),
    );
    facts.insert(
        "writes_count".to_string(),
        Value::Number(
            (uniq_edges
                .values()
                .filter(|e| e.rel_type == CodeRelType::Writes)
                .count() as i64)
                .into(),
        ),
    );

    CobolSemanticResult {
        nodes: nodes.into_values().collect(),
        edges: uniq_edges.into_values().collect(),
        facts,
    }
}


