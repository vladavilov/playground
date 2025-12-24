use crate::core::records::{CodeNodeRecord, EdgeRecord};
use crate::core::types::{CodeLanguage, CodeRelType};
use crate::plugins::base::{IngestionContext, LanguagePlugin};
use crate::plugins::cobol::copybooks::expand_copybooks;
use crate::plugins::cobol::edges::extract_cobol_edges;
use crate::plugins::cobol::normalizer::{preprocess_cobol_bytes, PreprocessResult};
use crate::plugins::cobol::semantic_linker::link_cobol_semantics;
use crate::plugins::cobol::unitizer::unitize_cobol_file;
use serde_json::{json, Value};
use std::path::PathBuf;

#[derive(Debug, Default)]
pub struct CobolPlugin;

impl CobolPlugin {
    pub fn new() -> Self {
        Self
    }
}

impl LanguagePlugin for CobolPlugin {
    fn name(&self) -> &str {
        "cobol"
    }

    fn iter_files(&self, ctx: &IngestionContext) -> anyhow::Result<Vec<PathBuf>> {
        let mut files: Vec<PathBuf> = Vec::new();
        for entry in walkdir::WalkDir::new(&ctx.repo_root).follow_links(false) {
            let entry = entry?;
            if !entry.file_type().is_file() {
                continue;
            }
            let p = entry.path();
            let rel = p.strip_prefix(&ctx.repo_root).unwrap_or(p).to_string_lossy().replace('\\', "/").to_ascii_lowercase();
            if rel.ends_with(".cbl") || rel.ends_with(".cob") || rel.ends_with(".cpy") {
                files.push(p.to_path_buf());
            }
        }
        files.sort_by_key(|p| p.strip_prefix(&ctx.repo_root).unwrap_or(p).to_string_lossy().replace('\\', "/"));
        Ok(files)
    }

    fn ingest(
        &self,
        ctx: &IngestionContext,
        files: &[PathBuf],
    ) -> anyhow::Result<(Vec<CodeNodeRecord>, Vec<EdgeRecord>, Value)> {
        let mut all_nodes: std::collections::BTreeMap<String, CodeNodeRecord> = std::collections::BTreeMap::new();
        let mut all_edges: Vec<EdgeRecord> = Vec::new();
        let mut includes_edges: Vec<EdgeRecord> = Vec::new();
        let mut copybook_includes: Vec<String> = Vec::new();

        for path in files {
            let rel_path = path.strip_prefix(&ctx.repo_root).unwrap_or(path).to_string_lossy().replace('\\', "/");
            let raw = std::fs::read(path)?;

            let prep = preprocess_cobol_bytes(&raw);
            let expanded = expand_copybooks(
                &ctx.repo_root,
                &prep.logical_lines,
                &prep.logical_spans,
                Some(&[ctx.repo_root.clone()]),
            )?;
            copybook_includes.extend(expanded.includes.iter().cloned());

            let prep2 = PreprocessResult {
                physical_lines: prep.physical_lines,
                logical_lines: expanded.logical_lines.clone(),
                logical_spans: expanded.logical_spans.clone(),
                parse_bytes: (expanded.logical_lines.join("\n") + "\n").into_bytes(),
            };

            let unit = unitize_cobol_file(
                &ctx.project_id,
                &ctx.repo_fingerprint,
                &rel_path,
                &prep2,
                &expanded.expansion_map,
                0.02,
            );
            for n in &unit.nodes {
                all_nodes.insert(n.node_id.clone(), n.clone());
            }

            let program_node_id = unit
                .nodes
                .iter()
                .find(|n| n.kind == "program" && n.file_path == rel_path)
                .map(|n| n.node_id.clone())
                .unwrap_or_else(|| unit.nodes[0].node_id.clone());

            match std::panic::catch_unwind(|| {
                link_cobol_semantics(
                    &ctx.project_id,
                    &ctx.repo_fingerprint,
                    &rel_path,
                    &prep2,
                    &program_node_id,
                )
            }) {
                Ok(sem) => {
                    for n in sem.nodes {
                        all_nodes.insert(n.node_id.clone(), n);
                    }
                    all_edges.extend(sem.edges);
                }
                Err(_) => {
                    let ed = extract_cobol_edges(
                        &ctx.project_id,
                        &ctx.repo_fingerprint,
                        &rel_path,
                        &program_node_id,
                        &prep2.physical_lines,
                    );
                    for n in ed.nodes {
                        all_nodes.insert(n.node_id.clone(), n);
                    }
                    all_edges.extend(ed.edges);
                }
            }

            // Emit INCLUDES edges to copybook files (dst_node_id uses file_path).
            for inc in expanded.includes {
                includes_edges.push(EdgeRecord {
                    project_id: ctx.project_id.clone(),
                    repo_fingerprint: ctx.repo_fingerprint.clone(),
                    rel_type: CodeRelType::Includes,
                    src_node_id: program_node_id.clone(),
                    dst_node_id: inc,
                    confidence: 1.0,
                    metadata: serde_json::Map::new(),
                });
            }
        }

        all_edges.extend(includes_edges);
        let all_edges = resolve_literal_calls_to_programs(&all_nodes, &all_edges);

        let includes_count = copybook_includes
            .into_iter()
            .collect::<std::collections::BTreeSet<_>>()
            .len();

        let facts = json!({
            "file_count": files.len(),
            "includes_count": includes_count,
        });

        Ok((all_nodes.into_values().collect(), all_edges, facts))
    }
}

fn normalize_program_name(name: &str) -> String {
    name.trim().to_ascii_uppercase()
}

fn resolve_literal_calls_to_programs(
    all_nodes: &std::collections::BTreeMap<String, CodeNodeRecord>,
    edges: &[EdgeRecord],
) -> Vec<EdgeRecord> {
    // program_name -> (node_id, file_path)
    let mut program_index: std::collections::BTreeMap<String, (String, String)> = std::collections::BTreeMap::new();
    for n in all_nodes.values() {
        if n.language != CodeLanguage::Cobol || n.kind != "program" {
            continue;
        }
        let Some(sym) = &n.symbol else { continue };
        let key = normalize_program_name(sym);
        let cand = (n.node_id.clone(), n.file_path.clone());
        match program_index.get(&key) {
            None => {
                program_index.insert(key, cand);
            }
            Some((ex_id, ex_path)) => {
                if (cand.1.clone(), cand.0.clone()) < (ex_path.clone(), ex_id.clone()) {
                    program_index.insert(key, cand);
                }
            }
        }
    }
    if program_index.is_empty() {
        return edges.to_vec();
    }

    let mut out: Vec<EdgeRecord> = Vec::new();
    for e in edges {
        if e.rel_type != CodeRelType::Calls {
            out.push(e.clone());
            continue;
        }
        let call_type = e.metadata.get("call_type").and_then(|v| v.as_str());
        if call_type != Some("literal") {
            out.push(e.clone());
            continue;
        }
        let callee = e.metadata.get("callee").and_then(|v| v.as_str()).unwrap_or("");
        if callee.trim().is_empty() {
            out.push(e.clone());
            continue;
        }
        let Some((resolved_node_id, _file_path)) = program_index.get(&normalize_program_name(callee)) else {
            out.push(e.clone());
            continue;
        };

        let mut md = e.metadata.clone();
        md.insert("resolved".to_string(), Value::Bool(true));
        md.insert("strategy".to_string(), Value::String("program-id".to_string()));
        out.push(EdgeRecord {
            project_id: e.project_id.clone(),
            repo_fingerprint: e.repo_fingerprint.clone(),
            rel_type: e.rel_type,
            src_node_id: e.src_node_id.clone(),
            dst_node_id: resolved_node_id.clone(),
            confidence: 0.9,
            metadata: md,
        });
    }

    // Deterministic de-dup after rewrite.
    let mut uniq: std::collections::BTreeMap<(CodeRelType, String, String, String), EdgeRecord> =
        std::collections::BTreeMap::new();
    for e in out {
        let md_key = serde_json::to_string(&e.metadata).unwrap_or_default();
        uniq.insert((e.rel_type, e.src_node_id.clone(), e.dst_node_id.clone(), md_key), e);
    }
    uniq.into_values().collect()
}


