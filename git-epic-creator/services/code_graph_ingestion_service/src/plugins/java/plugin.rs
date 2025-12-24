use crate::core::records::{CodeNodeRecord, EdgeRecord};
use crate::core::stable_ids::{snippet_hash, stable_node_id};
use crate::core::types::{CodeLanguage, CodeRelType};
use crate::plugins::base::{IngestionContext, LanguagePlugin};
use crate::plugins::tsg;
use crate::plugins::xml::parser::extract_class_attributes;
use serde_json::{json, Value};
use std::path::PathBuf;

#[derive(Debug, Default)]
pub struct JavaPlugin;

impl JavaPlugin {
    pub fn new() -> Self {
        Self
    }
}

impl LanguagePlugin for JavaPlugin {
    fn name(&self) -> &str {
        "java"
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
            if rel.ends_with(".java") || rel.ends_with(".xml") {
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

        let java_files: Vec<PathBuf> = files.iter().filter(|p| p.extension().is_some_and(|e| e.to_string_lossy().eq_ignore_ascii_case("java"))).cloned().collect();
        let xml_files: Vec<PathBuf> = files.iter().filter(|p| p.extension().is_some_and(|e| e.to_string_lossy().eq_ignore_ascii_case("xml"))).cloned().collect();

        let mut package_by_file: std::collections::BTreeMap<String, String> = std::collections::BTreeMap::new();
        let mut import_specs: Vec<(String, String, String)> = Vec::new(); // (importer_file, module_id, import_spec)

        for p in &java_files {
            let rel = p.strip_prefix(&ctx.repo_root).unwrap_or(p).to_string_lossy().replace('\\', "/");
            let bytes = std::fs::read(p)?;
            let mut text = String::from_utf8_lossy(&bytes).to_string();
            text = text.replace("\r\n", "\n").replace('\r', "\n");
            let mut lines: Vec<String> = text.split('\n').map(|s| s.to_string()).collect();
            if lines.last().is_some_and(|s| s.is_empty()) {
                lines.pop();
            }
            let end_line = (lines.len() as i64).max(1);

            // Module span (whole file).
            let module_id =
                stable_node_id([&ctx.project_id, &ctx.repo_fingerprint, &rel, "module", "", "1", &end_line.to_string()]);
            let module_text = lines.join("\n") + "\n";
            let module_hash = snippet_hash(&rel, 1, end_line, &module_text);
            nodes.insert(
                module_id.clone(),
                CodeNodeRecord {
                    project_id: ctx.project_id.clone(),
                    repo_fingerprint: ctx.repo_fingerprint.clone(),
                    node_id: module_id.clone(),
                    language: CodeLanguage::Java,
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

            // Tree-sitter-graph extraction.
            let extracted = tsg::extract_java(&text)?;
            if let Some(pkg) = extracted.package {
                package_by_file.insert(rel.clone(), pkg);
            }
            for imp in extracted.imports {
                import_specs.push((rel.clone(), module_id.clone(), imp));
            }

            for cls in extracted.classes {
                let snippet = slice_lines(&lines, cls.start_line, cls.end_line);
                let nid = stable_node_id([
                    &ctx.project_id,
                    &ctx.repo_fingerprint,
                    &rel,
                    "class",
                    &cls.name,
                    &cls.start_line.to_string(),
                    &cls.end_line.to_string(),
                ]);
                nodes.insert(
                    nid.clone(),
                    CodeNodeRecord {
                        project_id: ctx.project_id.clone(),
                        repo_fingerprint: ctx.repo_fingerprint.clone(),
                        node_id: nid,
                        language: CodeLanguage::Java,
                        kind: "class".to_string(),
                        symbol: Some(cls.name),
                        file_path: rel.clone(),
                        start_line: cls.start_line,
                        end_line: cls.end_line,
                        snippet_hash: snippet_hash(&rel, cls.start_line, cls.end_line, &snippet),
                        text: snippet,
                        extra_labels: vec![],
                    },
                );
            }

            let mut method_spans: Vec<(i64, i64, String)> = Vec::new();
            for m in extracted.methods {
                let snippet = slice_lines(&lines, m.start_line, m.end_line);
                let nid = stable_node_id([
                    &ctx.project_id,
                    &ctx.repo_fingerprint,
                    &rel,
                    "method",
                    &m.name,
                    &m.start_line.to_string(),
                    &m.end_line.to_string(),
                ]);
                method_spans.push((m.start_line, m.end_line, nid.clone()));
                nodes.insert(
                    nid.clone(),
                    CodeNodeRecord {
                        project_id: ctx.project_id.clone(),
                        repo_fingerprint: ctx.repo_fingerprint.clone(),
                        node_id: nid,
                        language: CodeLanguage::Java,
                        kind: "method".to_string(),
                        symbol: Some(m.name),
                        file_path: rel.clone(),
                        start_line: m.start_line,
                        end_line: m.end_line,
                        snippet_hash: snippet_hash(&rel, m.start_line, m.end_line, &snippet),
                        text: snippet,
                        extra_labels: vec![],
                    },
                );
            }

            // Optional: method invocation call sites -> unresolved call nodes.
            for call in extracted.call_sites {
                let src_node_id = find_container_node_id(&method_spans, call.start_line).unwrap_or_else(|| module_id.clone());
                let unresolved_id = stable_node_id([
                    &ctx.project_id,
                    &ctx.repo_fingerprint,
                    &rel,
                    "unresolved_java_call",
                    &call.name,
                ]);
                nodes.entry(unresolved_id.clone()).or_insert_with(|| CodeNodeRecord {
                    project_id: ctx.project_id.clone(),
                    repo_fingerprint: ctx.repo_fingerprint.clone(),
                    node_id: unresolved_id.clone(),
                    language: CodeLanguage::Java,
                    kind: "unresolved".to_string(),
                    symbol: Some(call.name.clone()),
                    file_path: rel.clone(),
                    start_line: 1,
                    end_line,
                    snippet_hash: snippet_hash(&rel, 1, 1, ""),
                    text: String::new(),
                    extra_labels: vec!["__UnresolvedCall__".to_string()],
                });
                let mut md = serde_json::Map::new();
                md.insert("callee".to_string(), Value::String(call.name));
                md.insert("kind".to_string(), Value::String("method_invocation".to_string()));
                edges.push(EdgeRecord {
                    project_id: ctx.project_id.clone(),
                    repo_fingerprint: ctx.repo_fingerprint.clone(),
                    rel_type: CodeRelType::Calls,
                    src_node_id: src_node_id,
                    dst_node_id: unresolved_id,
                    confidence: 0.2,
                    metadata: md,
                });
            }
        }

        // Emit IMPORTS edges (module -> unresolved import node).
        for (importer_file, module_id, imp) in &import_specs {
            let target_id = stable_node_id([
                &ctx.project_id,
                &ctx.repo_fingerprint,
                importer_file,
                "unresolved_java_import",
                imp,
            ]);
            nodes.insert(
                target_id.clone(),
                CodeNodeRecord {
                    project_id: ctx.project_id.clone(),
                    repo_fingerprint: ctx.repo_fingerprint.clone(),
                    node_id: target_id.clone(),
                    language: CodeLanguage::Java,
                    kind: "unresolved".to_string(),
                    symbol: Some(imp.clone()),
                    file_path: importer_file.clone(),
                    start_line: 1,
                    end_line: nodes.get(module_id).map(|n| n.end_line).unwrap_or(1).max(1),
                    snippet_hash: snippet_hash(importer_file, 1, 1, ""),
                    text: String::new(),
                    extra_labels: vec!["__UnresolvedImport__".to_string()],
                },
            );

            let mut md = serde_json::Map::new();
            md.insert("import".to_string(), Value::String(imp.clone()));
            edges.push(EdgeRecord {
                project_id: ctx.project_id.clone(),
                repo_fingerprint: ctx.repo_fingerprint.clone(),
                rel_type: CodeRelType::Imports,
                src_node_id: module_id.clone(),
                dst_node_id: target_id,
                confidence: 0.7,
                metadata: md,
            });
        }

        if !import_specs.is_empty() {
            edges = resolve_java_import_edges(&nodes, &edges, &package_by_file);
        }

        // XML wiring nodes + CONFIG_WIRES edges.
        for p in &xml_files {
            let rel = p.strip_prefix(&ctx.repo_root).unwrap_or(p).to_string_lossy().replace('\\', "/");
            let mut xml_text = std::fs::read_to_string(p)?;
            xml_text = xml_text.replace("\r\n", "\n").replace('\r', "\n");
            let mut lines: Vec<String> = xml_text.split('\n').map(|s| s.to_string()).collect();
            if lines.last().is_some_and(|s| s.is_empty()) {
                lines.pop();
            }
            let end_line = (lines.len() as i64).max(1);
            let xml_node_id = stable_node_id([
                &ctx.project_id,
                &ctx.repo_fingerprint,
                &rel,
                "xml_config",
                "",
                "1",
                &end_line.to_string(),
            ]);
            let xml_blob = lines.join("\n") + "\n";
            let xml_hash = snippet_hash(&rel, 1, end_line, &xml_blob);
            nodes.insert(
                xml_node_id.clone(),
                CodeNodeRecord {
                    project_id: ctx.project_id.clone(),
                    repo_fingerprint: ctx.repo_fingerprint.clone(),
                    node_id: xml_node_id.clone(),
                    language: CodeLanguage::Xml,
                    kind: "xml_config".to_string(),
                    symbol: None,
                    file_path: rel.clone(),
                    start_line: 1,
                    end_line,
                    snippet_hash: xml_hash,
                    text: xml_blob,
                    extra_labels: vec![],
                },
            );

            let wiring = extract_class_attributes(&xml_text);
            for cls in wiring.class_names {
                let target_id = stable_node_id([
                    &ctx.project_id,
                    &ctx.repo_fingerprint,
                    &rel,
                    "unresolved_java_class",
                    &cls,
                ]);
                nodes.insert(
                    target_id.clone(),
                    CodeNodeRecord {
                        project_id: ctx.project_id.clone(),
                        repo_fingerprint: ctx.repo_fingerprint.clone(),
                        node_id: target_id.clone(),
                        language: CodeLanguage::Java,
                        kind: "unresolved".to_string(),
                        symbol: Some(cls.clone()),
                        file_path: rel.clone(),
                        start_line: 1,
                        end_line,
                        snippet_hash: snippet_hash(&rel, 1, end_line, ""),
                        text: String::new(),
                        extra_labels: vec!["__UnresolvedCall__".to_string()],
                    },
                );
                let mut md = serde_json::Map::new();
                md.insert("class".to_string(), Value::String(cls));
                edges.push(EdgeRecord {
                    project_id: ctx.project_id.clone(),
                    repo_fingerprint: ctx.repo_fingerprint.clone(),
                    rel_type: CodeRelType::ConfigWires,
                    src_node_id: xml_node_id.clone(),
                    dst_node_id: target_id,
                    confidence: 0.6,
                    metadata: md,
                });
            }
        }

        let facts = json!({
            "java_file_count": java_files.len(),
            "xml_file_count": xml_files.len(),
        });
        Ok((nodes.into_values().collect(), edges, facts))
    }
}

fn slice_lines(lines: &[String], start_line: i64, end_line: i64) -> String {
    let s = start_line.max(1);
    let e = end_line.min(lines.len() as i64);
    if e < s {
        return String::new();
    }
    lines[(s - 1) as usize..=(e - 1) as usize].join("\n") + "\n"
}

fn find_container_node_id(spans: &[(i64, i64, String)], line_no: i64) -> Option<String> {
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

fn resolve_java_import_edges(
    nodes: &std::collections::BTreeMap<String, CodeNodeRecord>,
    edges: &[EdgeRecord],
    package_by_file: &std::collections::BTreeMap<String, String>,
) -> Vec<EdgeRecord> {
    let mut fqn_index: std::collections::BTreeMap<String, String> = std::collections::BTreeMap::new();
    let mut simple_index: std::collections::BTreeMap<String, Vec<String>> = std::collections::BTreeMap::new();

    for n in nodes.values() {
        if n.language != CodeLanguage::Java || n.kind != "class" {
            continue;
        }
        let Some(simple) = n.symbol.as_ref().map(|s| s.trim().to_string()) else { continue };
        if simple.is_empty() {
            continue;
        }
        if let Some(pkg) = package_by_file.get(&n.file_path) {
            let fqn = format!("{pkg}.{simple}");
            match fqn_index.get(&fqn) {
                None => {
                    fqn_index.insert(fqn, n.node_id.clone());
                }
                Some(existing) => {
                    let ex_node = nodes.get(existing);
                    if let Some(ex_node) = ex_node {
                        if (n.file_path.clone(), n.node_id.clone()) < (ex_node.file_path.clone(), ex_node.node_id.clone()) {
                            fqn_index.insert(fqn, n.node_id.clone());
                        }
                    }
                }
            }
        }
        simple_index.entry(simple).or_default().push(n.node_id.clone());
    }
    for v in simple_index.values_mut() {
        v.sort();
        v.dedup();
    }

    let mut out: Vec<EdgeRecord> = Vec::new();
    for e in edges {
        if e.rel_type != CodeRelType::Imports {
            out.push(e.clone());
            continue;
        }
        let spec = nodes
            .get(&e.dst_node_id)
            .and_then(|n| n.symbol.clone())
            .or_else(|| e.metadata.get("import").and_then(|v| v.as_str()).map(|s| s.to_string()));
        let Some(mut spec) = spec else {
            out.push(e.clone());
            continue;
        };
        spec = spec.trim().to_string();
        if spec.is_empty() || spec.ends_with(".*") {
            out.push(e.clone());
            continue;
        }

        let mut resolved_id: Option<String> = None;
        let mut strategy: Option<&'static str> = None;
        if let Some(id) = fqn_index.get(&spec) {
            resolved_id = Some(id.clone());
            strategy = Some("java-import-fqn");
        } else {
            let simple = spec.split('.').last().unwrap_or("").to_string();
            let cands = simple_index.get(&simple).cloned().unwrap_or_default();
            if cands.len() == 1 {
                resolved_id = Some(cands[0].clone());
                strategy = Some("java-import-simple");
            }
        }

        let Some(resolved_id) = resolved_id else {
            out.push(e.clone());
            continue;
        };
        let mut md = e.metadata.clone();
        md.insert("resolved".to_string(), Value::Bool(true));
        md.insert("strategy".to_string(), Value::String(strategy.unwrap().to_string()));
        md.insert("import".to_string(), Value::String(spec));
        out.push(EdgeRecord {
            project_id: e.project_id.clone(),
            repo_fingerprint: e.repo_fingerprint.clone(),
            rel_type: e.rel_type,
            src_node_id: e.src_node_id.clone(),
            dst_node_id: resolved_id,
            confidence: e.confidence.max(0.75),
            metadata: md,
        });
    }

    // Deterministic de-dup
    let mut uniq: std::collections::BTreeMap<(CodeRelType, String, String, String), EdgeRecord> =
        std::collections::BTreeMap::new();
    for e in out {
        let md_key = serde_json::to_string(&e.metadata).unwrap_or_default();
        uniq.insert((e.rel_type, e.src_node_id.clone(), e.dst_node_id.clone(), md_key), e);
    }
    uniq.into_values().collect()
}


