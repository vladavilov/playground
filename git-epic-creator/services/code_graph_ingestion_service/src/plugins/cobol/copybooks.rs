use regex::Regex;
use std::path::{Path, PathBuf};

#[derive(Debug, Clone)]
pub struct CopybookInsertProvenance {
    pub copybook_path: String, // repo-relative POSIX path
    pub copybook_start_line: i64,
    pub copybook_end_line: i64,
    pub inserted_at_logical_line: i64,
    pub inserted_line_count: i64,
}

#[derive(Debug, Clone)]
pub struct CopybookExpansionResult {
    pub logical_lines: Vec<String>,
    pub logical_spans: Vec<(i64, i64)>,
    pub includes: Vec<String>,
    pub expansion_map: Vec<CopybookInsertProvenance>,
}

fn re_copy() -> &'static Regex {
    static RE: std::sync::OnceLock<Regex> = std::sync::OnceLock::new();
    RE.get_or_init(|| Regex::new(r"^\s*COPY\s+([A-Za-z0-9_.-]+)").unwrap())
}

fn re_replacing() -> &'static Regex {
    static RE: std::sync::OnceLock<Regex> = std::sync::OnceLock::new();
    RE.get_or_init(|| Regex::new(r"\bREPLACING\b").unwrap())
}

fn re_pseudo_text() -> &'static Regex {
    static RE: std::sync::OnceLock<Regex> = std::sync::OnceLock::new();
    RE.get_or_init(|| Regex::new(r"==(.+?)==").unwrap())
}

pub fn expand_copybooks(
    repo_root: &Path,
    logical_lines: &[String],
    logical_spans: &[(i64, i64)],
    search_paths: Option<&[PathBuf]>,
) -> anyhow::Result<CopybookExpansionResult> {
    if logical_lines.len() != logical_spans.len() {
        anyhow::bail!("logical_lines and logical_spans must be aligned");
    }

    let search_paths_owned;
    let search_paths = match search_paths {
        Some(p) => p,
        None => {
            search_paths_owned = vec![repo_root.to_path_buf()];
            &search_paths_owned
        }
    };

    let mut out_lines: Vec<String> = Vec::new();
    let mut out_spans: Vec<(i64, i64)> = Vec::new();
    let mut includes: Vec<String> = Vec::new();
    let mut expansion_map: Vec<CopybookInsertProvenance> = Vec::new();

    for (idx, (line, span)) in logical_lines.iter().zip(logical_spans.iter()).enumerate() {
        let _line_no = (idx as i64) + 1;
        let m = re_copy().captures(line);
        if m.is_none() {
            out_lines.push(line.clone());
            out_spans.push(*span);
            continue;
        }
        let m = m.unwrap();
        let name = m.get(1).unwrap().as_str().trim().trim_end_matches('.').to_string();
        let replacing_pairs = parse_replacing_pairs(line);
        let copy_path = resolve_copybook(repo_root, &name, search_paths);

        let Some(copy_path) = copy_path else {
            out_lines.push(line.clone());
            out_spans.push(*span);
            continue;
        };

        let rel = copy_path.strip_prefix(repo_root).unwrap_or(&copy_path).to_string_lossy().replace('\\', "/");
        includes.push(rel.clone());

        let mut raw = std::fs::read_to_string(&copy_path)?;
        raw = raw.replace("\r\n", "\n").replace('\r', "\n");
        let mut raw_lines: Vec<String> = raw.split('\n').map(|s| s.to_string()).collect();
        if raw_lines.last().is_some_and(|s| s.is_empty()) {
            raw_lines.pop();
        }

        let inserted_lines: Vec<String> = raw_lines
            .iter()
            .map(|ln| apply_replacing(ln, &replacing_pairs))
            .collect();

        let inserted_at = (out_lines.len() as i64) + 1;
        out_lines.extend(inserted_lines.iter().cloned());
        out_spans.extend(std::iter::repeat(*span).take(inserted_lines.len()));

        if !inserted_lines.is_empty() {
            expansion_map.push(CopybookInsertProvenance {
                copybook_path: rel,
                copybook_start_line: 1,
                copybook_end_line: raw_lines.len() as i64,
                inserted_at_logical_line: inserted_at,
                inserted_line_count: inserted_lines.len() as i64,
            });
        }
    }

    // Deterministic de-dup includes while preserving order.
    let mut seen = std::collections::HashSet::new();
    let mut includes_dedup: Vec<String> = Vec::new();
    for p in includes {
        if seen.insert(p.clone()) {
            includes_dedup.push(p);
        }
    }

    Ok(CopybookExpansionResult {
        logical_lines: out_lines,
        logical_spans: out_spans,
        includes: includes_dedup,
        expansion_map,
    })
}

fn resolve_copybook(repo_root: &Path, name: &str, search_paths: &[PathBuf]) -> Option<PathBuf> {
    let candidates = vec![
        name.to_string(),
        format!("{name}.cpy"),
        format!("{name}.cbl"),
        format!("{name}.cob"),
        name.to_lowercase(),
        format!("{}.cpy", name.to_lowercase()),
        name.to_uppercase(),
        format!("{}.cpy", name.to_uppercase()),
    ];

    for base in search_paths {
        for cand in &candidates {
            let p = base.join(cand);
            let p = std::path::absolute(&p).ok()?;
            let rr = std::path::absolute(repo_root).ok()?;
            if p.strip_prefix(&rr).is_err() {
                continue;
            }
            if p.is_file() {
                return Some(p);
            }
        }
    }
    None
}

fn parse_replacing_pairs(line: &str) -> Vec<(String, String)> {
    if !re_replacing().is_match(line) {
        return vec![];
    }
    let chunks: Vec<String> = re_pseudo_text()
        .captures_iter(line)
        .filter_map(|c| c.get(1).map(|m| m.as_str().to_string()))
        .collect();
    if chunks.is_empty() {
        return vec![];
    }
    let mut pairs = Vec::new();
    let mut it = chunks.into_iter();
    while let Some(a) = it.next() {
        if let Some(b) = it.next() {
            pairs.push((a, b));
        } else {
            break;
        }
    }
    pairs
}

fn apply_replacing(s: &str, pairs: &[(String, String)]) -> String {
    let mut out = s.to_string();
    for (a, b) in pairs {
        out = out.replace(a, b);
    }
    out
}


