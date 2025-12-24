use crate::core::records::{CodeNodeRecord, EdgeRecord};
use crate::core::stable_ids::{snippet_hash, stable_node_id};
use crate::core::types::{CodeLanguage, CodeRelType};
use crate::plugins::base::{IngestionContext, LanguagePlugin};
use crate::plugins::tsg;
use serde_json::{json, Value};
use std::path::PathBuf;

const JS_EXTS: &[&str] = &[".js", ".jsx", ".ts", ".tsx", ".mjs", ".cjs"];

#[derive(Debug, Default)]
pub struct JavaScriptPlugin;

impl JavaScriptPlugin {
    pub fn new() -> Self {
        Self
    }
}

impl LanguagePlugin for JavaScriptPlugin {
    fn name(&self) -> &str {
        "javascript"
    }

    fn iter_files(&self, ctx: &IngestionContext) -> anyhow::Result<Vec<PathBuf>> {
        let mut files: Vec<PathBuf> = Vec::new();
        for entry in walkdir::WalkDir::new(&ctx.repo_root).follow_links(false) {
            let entry = entry?;
            if !entry.file_type().is_file() {
                continue;
            }
            let p = entry.path();
            let rel = p
                .strip_prefix(&ctx.repo_root)
                .unwrap_or(p)
                .to_string_lossy()
                .replace('\\', "/")
                .to_ascii_lowercase();
            if JS_EXTS.iter().any(|ext| rel.ends_with(ext)) {
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
        let mut nodes: std::collections::BTreeMap<String, CodeNodeRecord> = std::collections::BTreeMap::new();
        let mut edges: Vec<EdgeRecord> = Vec::new();

        let repo_files: std::collections::BTreeSet<String> = files
            .iter()
            .map(|p| p.strip_prefix(&ctx.repo_root).unwrap_or(p).to_string_lossy().replace('\\', "/"))
            .collect();

        for p in files {
            let rel = p.strip_prefix(&ctx.repo_root).unwrap_or(p).to_string_lossy().replace('\\', "/");
            let bytes = std::fs::read(p)?;
            let mut text = String::from_utf8_lossy(&bytes).to_string();
            text = text.replace("\r\n", "\n").replace('\r', "\n");
            let mut lines: Vec<String> = text.split('\n').map(|s| s.to_string()).collect();
            if lines.last().is_some_and(|s| s.is_empty()) {
                lines.pop();
            }

            let end_line = (lines.len() as i64).max(1);
            let module_id = stable_node_id([
                &ctx.project_id,
                &ctx.repo_fingerprint,
                &rel,
                "module",
                "",
                "1",
                &end_line.to_string(),
            ]);
            let module_text = lines.join("\n") + "\n";
            let module_hash = snippet_hash(&rel, 1, end_line, &module_text);
            nodes.insert(
                module_id.clone(),
                CodeNodeRecord {
                    project_id: ctx.project_id.clone(),
                    repo_fingerprint: ctx.repo_fingerprint.clone(),
                    node_id: module_id.clone(),
                    language: CodeLanguage::Javascript,
                    kind: "module".to_string(),
                    symbol: None,
                    file_path: rel.clone(),
                    start_line: 1,
                    end_line,
                    snippet_hash: module_hash,
                    text: module_text,
                    extra_labels: vec![],
                },
            );

            let rel_lc = rel.to_ascii_lowercase();
            let extracted = if rel_lc.ends_with(".tsx") {
                tsg::extract_tsx(&text)?
            } else if rel_lc.ends_with(".ts") {
                tsg::extract_ts(&text)?
            } else {
                tsg::extract_js(&text)?
            };
            for hit in extracted.imports {
                let (conf, kind) = match hit.kind {
                    "es_import" => (0.8, "es_import"),
                    "export_from" => (0.75, "export_from"),
                    "dynamic_import" => (0.6, "dynamic_import"),
                    "require" => (0.6, "require"),
                    _ => (0.5, hit.kind),
                };
                let mut md = serde_json::Map::new();
                md.insert("kind".to_string(), Value::String(kind.to_string()));
                md.insert("specifier".to_string(), Value::String(hit.specifier.clone()));

                let spec = hit.specifier;
                let target_id = stable_node_id([
                    &ctx.project_id,
                    &ctx.repo_fingerprint,
                    &rel,
                    "unresolved_module",
                    &spec,
                ]);
                nodes.insert(
                    target_id.clone(),
                    CodeNodeRecord {
                        project_id: ctx.project_id.clone(),
                        repo_fingerprint: ctx.repo_fingerprint.clone(),
                        node_id: target_id.clone(),
                        language: CodeLanguage::Javascript,
                        kind: "unresolved".to_string(),
                        symbol: Some(spec.clone()),
                        file_path: rel.clone(),
                        start_line: 1,
                        end_line,
                        snippet_hash: snippet_hash(&rel, 1, end_line, ""),
                        text: String::new(),
                        extra_labels: vec![],
                    },
                );
                edges.push(EdgeRecord {
                    project_id: ctx.project_id.clone(),
                    repo_fingerprint: ctx.repo_fingerprint.clone(),
                    rel_type: CodeRelType::Imports,
                    src_node_id: module_id.clone(),
                    dst_node_id: target_id,
                    confidence: conf,
                    metadata: md,
                });
            }
        }

        edges = resolve_js_import_edges(&nodes, &edges, &repo_files);

        let facts = json!({ "js_file_count": files.len() });
        Ok((nodes.into_values().collect(), edges, facts))
    }
}

fn resolve_js_import_edges(
    nodes: &std::collections::BTreeMap<String, CodeNodeRecord>,
    edges: &[EdgeRecord],
    repo_files: &std::collections::BTreeSet<String>,
) -> Vec<EdgeRecord> {
    let mut module_by_file: std::collections::BTreeMap<String, String> = std::collections::BTreeMap::new();
    for n in nodes.values() {
        if n.language == CodeLanguage::Javascript && n.kind == "module" {
            module_by_file.insert(n.file_path.clone(), n.node_id.clone());
        }
    }
    if module_by_file.is_empty() {
        return edges.to_vec();
    }

    let mut out: Vec<EdgeRecord> = Vec::new();
    for e in edges {
        if e.rel_type != CodeRelType::Imports {
            out.push(e.clone());
            continue;
        }
        let spec = nodes.get(&e.dst_node_id).and_then(|n| n.symbol.clone());
        let Some(spec) = spec else {
            out.push(e.clone());
            continue;
        };
        if spec.trim().is_empty() {
            out.push(e.clone());
            continue;
        }
        let importer_file_path = nodes.get(&e.src_node_id).map(|n| n.file_path.clone()).unwrap_or_default();
        let resolved_file = resolve_js_specifier_to_file_path(&importer_file_path, &spec, repo_files);
        let Some(resolved_file) = resolved_file else {
            out.push(e.clone());
            continue;
        };
        let Some(module_id) = module_by_file.get(&resolved_file) else {
            out.push(e.clone());
            continue;
        };

        let mut md = e.metadata.clone();
        md.insert("resolved".to_string(), Value::Bool(true));
        md.insert("strategy".to_string(), Value::String("import-specifier".to_string()));
        md.insert("resolved_file_path".to_string(), Value::String(resolved_file));

        let kind = md.get("kind").and_then(|v| v.as_str()).unwrap_or("");
        let mut new_conf = e.confidence;
        if kind == "es_import" {
            new_conf = new_conf.max(0.85);
        } else {
            new_conf = new_conf.max(0.75);
        }

        out.push(EdgeRecord {
            project_id: e.project_id.clone(),
            repo_fingerprint: e.repo_fingerprint.clone(),
            rel_type: e.rel_type,
            src_node_id: e.src_node_id.clone(),
            dst_node_id: module_id.clone(),
            confidence: new_conf,
            metadata: md,
        });
    }

    // Deterministic de-dup.
    let mut uniq: std::collections::BTreeMap<(CodeRelType, String, String, String), EdgeRecord> =
        std::collections::BTreeMap::new();
    for e in out {
        let md_key = serde_json::to_string(&e.metadata).unwrap_or_default();
        uniq.insert((e.rel_type, e.src_node_id.clone(), e.dst_node_id.clone(), md_key), e);
    }
    uniq.into_values().collect()
}

fn resolve_js_specifier_to_file_path(importer_file_path: &str, specifier: &str, repo_files: &std::collections::BTreeSet<String>) -> Option<String> {
    let spec = specifier.trim();
    if spec.is_empty() {
        return None;
    }

    if spec.starts_with('.') {
        let base_dir = std::path::Path::new(importer_file_path)
            .parent()
            .map(|p| p.to_string_lossy().replace('\\', "/"))
            .unwrap_or_default();
        let base = if base_dir.is_empty() {
            normalize_posix_path(spec)
        } else {
            normalize_posix_path(&format!("{base_dir}/{spec}"))
        };
        return pick_first_existing_js_candidate(&base, repo_files);
    }

    if spec.starts_with('/') {
        let base = normalize_posix_path(spec.trim_start_matches('/'));
        return pick_first_existing_js_candidate(&base, repo_files);
    }

    let candidates = js_path_candidates(spec);
    let mut matches: Vec<String> = Vec::new();
    for f in repo_files {
        for cand in &candidates {
            if f == cand || f.ends_with(&format!("/{cand}")) {
                matches.push(f.clone());
                break;
            }
        }
    }
    matches.sort();
    matches.dedup();
    if matches.len() == 1 {
        return Some(matches[0].clone());
    }
    None
}

fn pick_first_existing_js_candidate(base: &str, repo_files: &std::collections::BTreeSet<String>) -> Option<String> {
    for cand in js_path_candidates(base) {
        if repo_files.contains(&cand) {
            return Some(cand);
        }
    }
    None
}

fn js_path_candidates(base: &str) -> Vec<String> {
    let base = normalize_posix_path(base);
    if JS_EXTS.iter().any(|ext| base.ends_with(ext)) {
        return vec![base];
    }
    let mut out: Vec<String> = Vec::new();
    for ext in JS_EXTS {
        out.push(format!("{base}{ext}"));
    }
    for ext in JS_EXTS {
        out.push(format!("{base}/index{ext}"));
    }
    out
}

fn normalize_posix_path(p: &str) -> String {
    let mut parts: Vec<&str> = Vec::new();
    let normalized = p.replace('\\', "/");
    for part in normalized.split('/') {
        if part.is_empty() || part == "." {
            continue;
        }
        if part == ".." {
            parts.pop();
            continue;
        }
        parts.push(part);
    }
    parts.join("/")
}

