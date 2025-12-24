use sha2::{Digest, Sha256};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RepoFingerprint {
    pub value: String,
    pub source_type: String, // "zip" | "git"
    pub anchor: String,      // raw sha (zip) or commit sha (git)
}

fn sha256_hex(bytes: &[u8]) -> String {
    let mut h = Sha256::new();
    h.update(bytes);
    hex::encode(h.finalize())
}

pub fn fingerprint_for_zip_bytes(zip_bytes: &[u8]) -> RepoFingerprint {
    let sha = sha256_hex(zip_bytes);
    RepoFingerprint {
        value: format!("zip:{sha}"),
        source_type: "zip".to_string(),
        anchor: sha,
    }
}

pub fn fingerprint_for_git_head_commit(head_commit: &str) -> RepoFingerprint {
    RepoFingerprint {
        value: format!("git:{head_commit}"),
        source_type: "git".to_string(),
        anchor: head_commit.to_string(),
    }
}


