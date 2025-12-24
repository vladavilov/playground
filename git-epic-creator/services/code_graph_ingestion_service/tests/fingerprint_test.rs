use code_graph_ingestion_service::core::fingerprint::{
    fingerprint_for_git_head_commit, fingerprint_for_zip_bytes,
};
use sha2::{Digest, Sha256};

fn make_zip(entries: Vec<(&str, &[u8])>) -> Vec<u8> {
    let mut buf = std::io::Cursor::new(Vec::new());
    {
        let mut z = zip::ZipWriter::new(&mut buf);
        let opts = zip::write::SimpleFileOptions::default();
        for (name, data) in entries {
            z.start_file(name, opts).unwrap();
            std::io::Write::write_all(&mut z, data).unwrap();
        }
        z.finish().unwrap();
    }
    buf.into_inner()
}

#[test]
fn zip_entry_order_changes_fingerprint() {
    let z1 = make_zip(vec![("b.txt", b"2"), ("a.txt", b"1"), ("dir/x.txt", b"3")]);
    let z2 = make_zip(vec![("dir/x.txt", b"3"), ("a.txt", b"1"), ("b.txt", b"2")]);

    let f1 = fingerprint_for_zip_bytes(&z1);
    let f2 = fingerprint_for_zip_bytes(&z2);

    assert_ne!(f1.value, f2.value);
    let sha1 = hex::encode(Sha256::digest(&z1));
    let sha2 = hex::encode(Sha256::digest(&z2));
    assert_eq!(f1.anchor, sha1);
    assert_eq!(f2.anchor, sha2);
    assert_eq!(f1.value, format!("zip:{}", f1.anchor));
    assert_eq!(f2.value, format!("zip:{}", f2.anchor));
}

#[test]
fn zip_manifest_change_changes_fingerprint() {
    let z1 = make_zip(vec![
        ("src/app.py", b"print('hi')\n"),
        ("README.md", b"x\n"),
        ("pyproject.toml", b"[project]\nname='a'\n"),
    ]);
    let z2 = make_zip(vec![
        ("src/app.py", b"print('hi')\n"),
        ("README.md", b"x\n"),
        ("pyproject.toml", b"[project]\nname='b'\n"),
    ]);

    let f1 = fingerprint_for_zip_bytes(&z1);
    let f2 = fingerprint_for_zip_bytes(&z2);

    assert_ne!(f1.value, f2.value);
    assert!(f1.value.starts_with("zip:"));
    assert!(f2.value.starts_with("zip:"));
}

#[test]
fn git_manifest_change_changes_fingerprint_by_commit() {
    // Git fingerprints are the checked-out commit SHA; we don't include manifests in the hash
    // beyond what Git commit SHA already reflects.
    let fp1 = fingerprint_for_git_head_commit("abc");
    let fp2 = fingerprint_for_git_head_commit("def");
    assert_eq!(fp1.value, format!("git:{}", fp1.anchor));
    assert_eq!(fp2.value, format!("git:{}", fp2.anchor));
    assert_ne!(fp1.value, fp2.value);
}


