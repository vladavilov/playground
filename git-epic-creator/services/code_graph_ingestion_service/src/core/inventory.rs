use crate::core::ignore_rules::{build_ignore_rules, IgnoreRules};
use crate::core::types::CodeLanguage;
use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};
use std::path::{Component, Path, PathBuf};

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct InventoryEntry {
    pub path: String, // repo-relative POSIX path
    pub sha256: String,
    pub language: CodeLanguage,
    pub line_count: i64,
}

fn to_posix_rel_path(repo_root: &Path, p: &Path) -> anyhow::Result<String> {
    let rel = p.strip_prefix(repo_root)?;
    let mut out = String::new();
    for (i, c) in rel.components().enumerate() {
        let s = match c {
            Component::Normal(os) => os.to_string_lossy(),
            _ => continue,
        };
        if i > 0 {
            out.push('/');
        }
        out.push_str(&s);
    }
    Ok(out)
}

fn count_physical_lines(data: &[u8]) -> i64 {
    if data.is_empty() {
        return 0;
    }
    let n = data.iter().filter(|b| **b == b'\n').count() as i64;
    if data.last() == Some(&b'\n') {
        n
    } else {
        n + 1
    }
}

fn infer_language(rel_posix_path: &str) -> CodeLanguage {
    let p = rel_posix_path.to_ascii_lowercase();
    if p.ends_with(".cbl") || p.ends_with(".cob") || p.ends_with(".cpy") {
        return CodeLanguage::Cobol;
    }
    if p.ends_with(".java") {
        return CodeLanguage::Java;
    }
    if p.ends_with(".js")
        || p.ends_with(".jsx")
        || p.ends_with(".mjs")
        || p.ends_with(".cjs")
        || p.ends_with(".ts")
        || p.ends_with(".tsx")
    {
        return CodeLanguage::Javascript;
    }
    if p.ends_with(".xml") || p.ends_with(".xsd") || p.ends_with(".wsdl") {
        return CodeLanguage::Xml;
    }
    CodeLanguage::Other
}

pub fn iter_repo_files(repo_root: &Path, ignore: &IgnoreRules) -> anyhow::Result<Vec<PathBuf>> {
    let mut files: Vec<PathBuf> = Vec::new();
    for entry in walkdir::WalkDir::new(repo_root).follow_links(false) {
        let entry = entry?;
        if !entry.file_type().is_file() {
            continue;
        }
        let p = entry.path().to_path_buf();
        let rel = to_posix_rel_path(repo_root, &p)?;
        if ignore.is_ignored(&rel) {
            continue;
        }
        files.push(p);
    }
    files.sort_by_key(|p| to_posix_rel_path(repo_root, p).unwrap_or_default());
    Ok(files)
}

pub fn build_inventory(repo_root: &Path, ignore: Option<&IgnoreRules>) -> anyhow::Result<Vec<InventoryEntry>> {
    let owned_ignore;
    let ignore = match ignore {
        Some(i) => i,
        None => {
            owned_ignore = build_ignore_rules(repo_root, None)?;
            &owned_ignore
        }
    };

    let mut out: Vec<InventoryEntry> = Vec::new();
    for p in iter_repo_files(repo_root, ignore)? {
        let rel = to_posix_rel_path(repo_root, &p)?;
        let data = std::fs::read(&p)?;
        let sha = hex::encode(Sha256::digest(&data));
        out.push(InventoryEntry {
            path: rel.clone(),
            sha256: sha,
            language: infer_language(&rel),
            line_count: count_physical_lines(&data),
        });
    }
    Ok(out)
}


