use sha2::{Digest, Sha256};
use std::io::Read;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RepoFingerprint {
    pub value: String,
    pub source_type: String, // "zip" | "git"
    pub anchor: String,      // content sha (zip) or commit sha (git)
}

pub fn fingerprint_for_zip_bytes(zip_bytes: &[u8]) -> anyhow::Result<RepoFingerprint> {
    let mut archive = zip::ZipArchive::new(std::io::Cursor::new(zip_bytes))?;

    // Canonical: hash entry paths + bytes in normalized path order (independent of ZIP container ordering).
    let mut ordered: Vec<(String, usize)> = Vec::new();
    for i in 0..archive.len() {
        let f = archive.by_index(i)?;
        let name = f.name().to_string();

        // Directories don't contribute to repo content.
        if name.ends_with('/') {
            continue;
        }

        // Reject zip-slip and normalize to repo-relative POSIX paths.
        let rel = crate::core::zip_utils::normalized_zip_relpath(&name)?;
        if rel.is_empty() {
            continue;
        }

        // Reject symlinks (best-effort via unix mode).
        if let Some(mode) = f.unix_mode() {
            let file_type = mode & 0o170000;
            if file_type == 0o120000 {
                anyhow::bail!("unsafe zip entry (symlink): {name}");
            }
        }

        ordered.push((rel, i));
    }
    ordered.sort_by(|a, b| a.0.cmp(&b.0).then_with(|| a.1.cmp(&b.1)));

    let mut hasher = Sha256::new();
    let mut buf = vec![0u8; 8 * 1024];
    for (rel, idx) in ordered {
        hasher.update(rel.as_bytes());
        hasher.update([0u8]);

        let mut f = archive.by_index(idx)?;
        loop {
            let n = f.read(&mut buf)?;
            if n == 0 {
                break;
            }
            hasher.update(&buf[..n]);
        }
        hasher.update([0u8]);
    }

    let sha = hex::encode(hasher.finalize());
    Ok(RepoFingerprint {
        value: format!("zip:{sha}"),
        source_type: "zip".to_string(),
        anchor: sha,
    })
}

pub fn fingerprint_for_git_head_commit(head_commit: &str) -> RepoFingerprint {
    RepoFingerprint {
        value: format!("git:{head_commit}"),
        source_type: "git".to_string(),
        anchor: head_commit.to_string(),
    }
}
