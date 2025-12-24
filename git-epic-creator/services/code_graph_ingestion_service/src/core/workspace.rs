use sha2::{Digest, Sha256};
use std::path::{Path, PathBuf};

fn is_safe_segment(s: &str) -> bool {
    let bytes = s.as_bytes();
    if bytes.is_empty() || bytes.len() > 128 {
        return false;
    }
    let first = bytes[0];
    if !first.is_ascii_alphanumeric() {
        return false;
    }
    bytes.iter().all(|b| {
        b.is_ascii_alphanumeric() || matches!(*b, b'_' | b'.' | b'-')
    })
}

fn stable_key(value: &str) -> String {
    if is_safe_segment(value) {
        return value.to_string();
    }
    let mut h = Sha256::new();
    h.update(value.as_bytes());
    let digest = hex::encode(h.finalize());
    format!("sha256-{}", &digest[..16])
}

#[derive(Debug, Clone)]
pub struct Workspace {
    pub root: PathBuf,
}

impl Workspace {
    pub fn new(root: impl AsRef<Path>) -> Self {
        Self {
            root: root.as_ref().to_path_buf(),
        }
    }

    pub fn project_dir(&self, project_id: &str) -> PathBuf {
        self.root.join(stable_key(project_id))
    }

    pub fn zip_repo_dir(&self, project_id: &str) -> PathBuf {
        self.project_dir(project_id).join("zip").join("repo")
    }

    pub fn git_repo_dir(&self, project_id: &str, git_url: &str, ref_name: Option<&str>) -> PathBuf {
        let key_src = format!("{}\n{}", git_url, ref_name.unwrap_or(""));
        let key = stable_key(&key_src);
        self.project_dir(project_id).join("git").join(key).join("repo")
    }
}


