use crate::core::records::CodeNodeRecord;
use crate::core::stable_ids::{snippet_hash, stable_node_id};
use crate::core::types::CodeLanguage;
use serde_json::Value;

use super::regexes;
use super::text;

pub(crate) fn extract_data_defs(
    project_id: &str,
    repo_fingerprint: &str,
    file_path: &str,
    physical_lines: &[String],
    divisions: &[(String, String, i64, i64)],
    sections: &[(String, String, i64, i64)],
) -> (
    Vec<CodeNodeRecord>,
    std::collections::BTreeMap<String, Vec<serde_json::Map<String, Value>>>,
) {
    let data_div = divisions.iter().find(|(_, sym, _, _)| sym == "DATA");
    let Some((_k, _sym, data_start, data_end)) = data_div else {
        return (vec![], std::collections::BTreeMap::new());
    };

    let sec_spans: Vec<(String, i64, i64)> = sections.iter().map(|(_, sym, s, e)| (sym.clone(), *s, *e)).collect();

    let mut nodes: Vec<CodeNodeRecord> = Vec::new();
    let mut defs_by_name: std::collections::BTreeMap<String, Vec<serde_json::Map<String, Value>>> =
        std::collections::BTreeMap::new();
    let mut stack: Vec<(i64, Option<String>)> = Vec::new();

    for i in *data_start..=*data_end {
        let line = &physical_lines[(i - 1) as usize];
        let Some(cap) = regexes::re_data_def().captures(line) else {
            continue;
        };
        let level_s = cap.get(1).unwrap().as_str().trim();
        let name = cap.get(2).unwrap().as_str().trim();
        if level_s.is_empty() || name.is_empty() {
            continue;
        }
        let up_name = name.to_ascii_uppercase();
        if up_name == "FILLER" {
            continue;
        }
        let level: i64 = level_s.parse::<i64>().unwrap_or(99);

        let parent_names: Vec<String>;
        if (1..=49).contains(&level) {
            while stack.last().is_some_and(|(lvl, _)| *lvl >= level) {
                stack.pop();
            }
            parent_names = stack.iter().rev().filter_map(|(_, n)| n.clone()).collect();
            stack.push((level, Some(up_name.clone())));
        } else {
            parent_names = stack.iter().rev().filter_map(|(_, n)| n.clone()).collect();
        }

        let storage = storage_key_for_line(&sec_spans, i).unwrap_or_else(|| "DATA".to_string());
        let display = data_display_symbol(&storage, &up_name, &parent_names);
        let nid = stable_node_id([
            project_id,
            repo_fingerprint,
            file_path,
            "data_def",
            &display,
            &i.to_string(),
            &i.to_string(),
        ]);
        let text = text::slice_text(physical_lines, i, i);

        let mut extra_labels: Vec<String> = Vec::new();
        if level == 88 {
            extra_labels.push("__Data88__".to_string());
        } else if level == 66 {
            extra_labels.push("__Data66__".to_string());
        } else if level == 77 {
            extra_labels.push("__Data77__".to_string());
        }

        nodes.push(CodeNodeRecord {
            project_id: project_id.to_string(),
            repo_fingerprint: repo_fingerprint.to_string(),
            node_id: nid.clone(),
            language: CodeLanguage::Cobol,
            kind: "data_def".to_string(),
            symbol: Some(display.clone()),
            file_path: file_path.to_string(),
            start_line: i,
            end_line: i,
            snippet_hash: snippet_hash(file_path, i, i, &text),
            text,
            extra_labels,
        });

        let mut def = serde_json::Map::new();
        def.insert("name".to_string(), Value::String(up_name.clone()));
        def.insert(
            "qualifiers".to_string(),
            Value::Array(parent_names.iter().map(|q| Value::String(q.clone())).collect()),
        );
        def.insert("storage".to_string(), Value::String(storage.clone()));
        def.insert("node_id".to_string(), Value::String(nid.clone()));
        def.insert("start_line".to_string(), Value::Number(i.into()));
        defs_by_name.entry(up_name).or_default().push(def);
    }

    // Deterministic ranking for defs_by_name.
    for defs in defs_by_name.values_mut() {
        defs.sort_by(|a, b| {
            let ar = storage_rank(a.get("storage").and_then(|v| v.as_str()).unwrap_or(""));
            let br = storage_rank(b.get("storage").and_then(|v| v.as_str()).unwrap_or(""));
            ar.cmp(&br)
                .then_with(|| {
                    let al = a.get("qualifiers").and_then(|v| v.as_array()).map(|x| x.len()).unwrap_or(0);
                    let bl = b.get("qualifiers").and_then(|v| v.as_array()).map(|x| x.len()).unwrap_or(0);
                    bl.cmp(&al)
                })
                .then_with(|| {
                    a.get("start_line")
                        .and_then(|v| v.as_i64())
                        .unwrap_or(0)
                        .cmp(&b.get("start_line").and_then(|v| v.as_i64()).unwrap_or(0))
                })
                .then_with(|| {
                    a.get("node_id")
                        .and_then(|v| v.as_str())
                        .unwrap_or("")
                        .cmp(b.get("node_id").and_then(|v| v.as_str()).unwrap_or(""))
                })
        });
    }

    (nodes, defs_by_name)
}

fn storage_key_for_line(sec_spans: &[(String, i64, i64)], line_no: i64) -> Option<String> {
    for (sym, s, e) in sec_spans {
        if *s <= line_no && line_no <= *e {
            return Some(sym.to_string());
        }
    }
    None
}

fn storage_rank(storage: &str) -> i64 {
    match storage.to_ascii_uppercase().as_str() {
        "WORKING-STORAGE" => 1,
        "LOCAL-STORAGE" => 2,
        "LINKAGE" => 3,
        "FILE" => 4,
        _ => 9,
    }
}

fn data_display_symbol(storage: &str, name: &str, parent_names: &[String]) -> String {
    if parent_names.is_empty() {
        return format!("{storage}:{name}");
    }
    format!("{storage}:{name} OF {}", parent_names.join(" OF "))
}

pub(crate) fn parse_qualified_ref(s: &str) -> (String, Vec<String>) {
    let tokens: Vec<&str> = s.split_whitespace().collect();
    if tokens.is_empty() {
        return (String::new(), vec![]);
    }
    let name = tokens[0].to_ascii_uppercase();
    let mut quals: Vec<String> = Vec::new();
    let mut i = 1usize;
    while i + 1 < tokens.len() {
        let kw = tokens[i].to_ascii_uppercase();
        if kw == "OF" || kw == "IN" {
            quals.push(tokens[i + 1].to_ascii_uppercase());
            i += 2;
        } else {
            i += 1;
        }
    }
    (name, quals)
}

pub(crate) fn resolve_data_ref(
    ref_name: &str,
    qualifiers: &[String],
    defs_by_name: &std::collections::BTreeMap<String, Vec<serde_json::Map<String, Value>>>,
) -> Option<String> {
    let cands = defs_by_name.get(&ref_name.to_ascii_uppercase())?;
    let q: Vec<String> = qualifiers.iter().map(|x| x.to_ascii_uppercase()).collect();
    let mut filtered = Vec::new();
    for d in cands {
        let cand_q: Vec<String> = d
            .get("qualifiers")
            .and_then(|v| v.as_array())
            .map(|arr| arr.iter().filter_map(|x| x.as_str().map(|s| s.to_string())).collect())
            .unwrap_or_else(Vec::new);
        if cand_q.iter().take(q.len()).cloned().collect::<Vec<_>>() == q {
            filtered.push(d);
        }
    }
    if filtered.len() == 1 {
        filtered[0]
            .get("node_id")
            .and_then(|v| v.as_str())
            .map(|s| s.to_string())
    } else {
        None
    }
}

pub(crate) fn candidate_count(
    ref_name: &str,
    defs_by_name: &std::collections::BTreeMap<String, Vec<serde_json::Map<String, Value>>>,
) -> usize {
    defs_by_name
        .get(&ref_name.to_ascii_uppercase())
        .map(|v| v.len())
        .unwrap_or(0)
}


