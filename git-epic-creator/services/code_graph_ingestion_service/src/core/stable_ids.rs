use sha2::{Digest, Sha256};

pub fn stable_node_id<I, S>(parts: I) -> String
where
    I: IntoIterator<Item = S>,
    S: AsRef<str>,
{
    stable_node_id_with_size(parts, 24)
}

pub fn stable_node_id_with_size<I, S>(parts: I, size: usize) -> String
where
    I: IntoIterator<Item = S>,
    S: AsRef<str>,
{
    let raw = parts
        .into_iter()
        .map(|s| s.as_ref().to_string())
        .collect::<Vec<_>>()
        .join("|");

    let mut h = Sha256::new();
    h.update(raw.as_bytes());
    let digest = h.finalize();
    hex::encode(digest)[..size].to_string()
}

fn normalize_text_for_snippet_hash(text: &str) -> String {
    let t = text.replace("\r\n", "\n").replace('\r', "\n");
    let lines = t.split('\n').map(|ln| ln.trim_end_matches([' ', '\t']));
    lines.collect::<Vec<_>>().join("\n")
}

pub fn snippet_hash(file_path: &str, start_line: i64, end_line: i64, text: &str) -> String {
    let norm = normalize_text_for_snippet_hash(text);
    let raw = format!("{file_path}|{start_line}|{end_line}|\n{norm}");

    let mut h = Sha256::new();
    h.update(raw.as_bytes());
    hex::encode(h.finalize())
}


