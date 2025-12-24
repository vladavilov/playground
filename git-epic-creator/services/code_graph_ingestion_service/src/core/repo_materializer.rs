use git2::{build::CheckoutBuilder, Repository};
use std::io::{Cursor, Read};
use std::path::{Path, PathBuf};
use std::thread::sleep;
use std::time::Duration;

#[derive(Debug, thiserror::Error)]
pub enum MaterializeError {
    #[error("zip slip: {0}")]
    ZipSlip(String),
    #[error("unsafe zip entry (symlink): {0}")]
    UnsafeZipEntry(String),
    #[error(transparent)]
    Io(#[from] std::io::Error),
    #[error(transparent)]
    Zip(#[from] zip::result::ZipError),
    #[error(transparent)]
    Git(#[from] git2::Error),
}

fn rmtree_retry(path: &Path, attempts: usize, delay: Duration) -> std::io::Result<()> {
    let mut last_err: Option<std::io::Error> = None;
    for _ in 0..attempts {
        match std::fs::remove_dir_all(path) {
            Ok(()) => return Ok(()),
            Err(e) => {
                last_err = Some(e);
                sleep(delay);
            }
        }
    }
    Err(last_err.unwrap_or_else(|| std::io::Error::other("failed to remove dir")))
}

fn normalized_zip_relpath(name: &str) -> Result<String, MaterializeError> {
    // ZIP spec uses forward slashes, but Windows tools may emit backslashes.
    let mut p = name.replace('\\', "/");

    // Normalize path segments manually (POSIX semantics).
    while p.starts_with("./") {
        p = p[2..].to_string();
    }

    // Reject absolute paths and drive-letter-ish paths.
    if p.starts_with('/') || p.starts_with('\\') {
        return Err(MaterializeError::ZipSlip(format!(
            "unsafe absolute path in zip entry: {name:?}"
        )));
    }
    if let Some(first) = p.split('/').next() {
        if first.contains(':') {
            return Err(MaterializeError::ZipSlip(format!(
                "unsafe absolute path in zip entry: {name:?}"
            )));
        }
    }

    let mut out_parts: Vec<&str> = Vec::new();
    for part in p.split('/') {
        if part.is_empty() || part == "." {
            continue;
        }
        if part == ".." {
            return Err(MaterializeError::ZipSlip(format!(
                "unsafe parent traversal in zip entry: {name:?}"
            )));
        }
        out_parts.push(part);
    }

    let rel = out_parts.join("/");
    if rel.is_empty() || rel == "." {
        Ok(String::new())
    } else {
        Ok(rel)
    }
}

pub fn materialize_zip_bytes(zip_bytes: &[u8], dest_dir: &Path) -> Result<PathBuf, MaterializeError> {
    // Deterministic: ensure the output directory is empty.
    if dest_dir.exists() {
        rmtree_retry(dest_dir, 30, Duration::from_millis(100))?;
    }
    std::fs::create_dir_all(dest_dir)?;

    let mut archive = zip::ZipArchive::new(Cursor::new(zip_bytes))?;

    // Deterministic extraction order: sort by normalized repo-relative path.
    let mut ordered: Vec<(String, usize)> = Vec::new();
    for i in 0..archive.len() {
        let name = archive.by_index(i)?.name().to_string();
        let rel = normalized_zip_relpath(&name)?;
        if rel.is_empty() {
            continue;
        }

        // Reject symlinks (best-effort via unix mode).
        let f = archive.by_index(i)?;
        if let Some(mode) = f.unix_mode() {
            let file_type = mode & 0o170000;
            if file_type == 0o120000 {
                return Err(MaterializeError::UnsafeZipEntry(name));
            }
        }

        ordered.push((rel, i));
    }
    ordered.sort_by(|a, b| a.0.cmp(&b.0));

    for (rel, idx) in ordered {
        let mut f = archive.by_index(idx)?;
        let out_path = dest_dir.join(&rel);

        if f.name().ends_with('/') {
            std::fs::create_dir_all(&out_path)?;
            continue;
        }
        if let Some(parent) = out_path.parent() {
            std::fs::create_dir_all(parent)?;
        }

        let mut out = std::fs::File::create(&out_path)?;
        let mut buf = Vec::new();
        f.read_to_end(&mut buf)?;
        std::io::Write::write_all(&mut out, &buf)?;
    }

    Ok(dest_dir.to_path_buf())
}

#[derive(Debug, Clone)]
pub struct GitMaterializationResult {
    pub repo_root: PathBuf,
    pub head_commit: Option<String>,
}

pub fn materialize_git(git_url: &str, ref_name: Option<&str>, dest_dir: &Path) -> Result<GitMaterializationResult, MaterializeError> {
    // Deterministic: ensure the output directory is empty.
    if dest_dir.exists() {
        rmtree_retry(dest_dir, 30, Duration::from_millis(100))?;
    }
    if let Some(parent) = dest_dir.parent() {
        std::fs::create_dir_all(parent)?;
    }

    let repo = Repository::clone(git_url, dest_dir)?;

    if let Some(r) = ref_name {
        let obj = repo.revparse_single(r)?;
        repo.checkout_tree(&obj, Some(CheckoutBuilder::new().force()))?;
        repo.set_head_detached(obj.id())?;
    }

    let head_commit = repo
        .head()
        .ok()
        .and_then(|h| h.target())
        .map(|oid| oid.to_string());

    Ok(GitMaterializationResult {
        repo_root: dest_dir.to_path_buf(),
        head_commit,
    })
}


