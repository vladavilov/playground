#[derive(Debug, thiserror::Error)]
pub enum ZipPathError {
    #[error("zip slip: {0}")]
    ZipSlip(String),
}

pub fn normalized_zip_relpath(name: &str) -> Result<String, ZipPathError> {
    // ZIP spec uses forward slashes, but Windows tools may emit backslashes.
    let mut p = name.replace('\\', "/");

    // Normalize path segments manually (POSIX semantics).
    while p.starts_with("./") {
        p = p[2..].to_string();
    }

    // Reject absolute paths and drive-letter-ish paths.
    if p.starts_with('/') || p.starts_with('\\') {
        return Err(ZipPathError::ZipSlip(format!(
            "unsafe absolute path in zip entry: {name:?}"
        )));
    }
    if let Some(first) = p.split('/').next() {
        if first.contains(':') {
            return Err(ZipPathError::ZipSlip(format!(
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
            return Err(ZipPathError::ZipSlip(format!(
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
