use code_graph_ingestion_service::core::repo_materializer::{materialize_git, materialize_zip_bytes, MaterializeError};
use git2::{Oid, Repository, Signature};
use sha2::{Digest, Sha256};
use std::io::Write;
use std::path::{Path, PathBuf};

fn tree_digest(root: &Path) -> String {
    let mut h = Sha256::new();
    let mut files: Vec<PathBuf> = walkdir::WalkDir::new(root)
        .follow_links(false)
        .into_iter()
        .filter_map(|e| e.ok())
        .filter(|e| e.file_type().is_file())
        .map(|e| e.path().to_path_buf())
        .filter(|p| !p.components().any(|c| c.as_os_str() == ".git"))
        .collect();
    files.sort_by_key(|p| p.to_string_lossy().to_string());

    for p in files {
        let rel = p.strip_prefix(root).unwrap().to_string_lossy().replace('\\', "/");
        h.update(rel.as_bytes());
        h.update([0u8]);
        h.update(std::fs::read(&p).unwrap());
        h.update(b"\n");
    }
    hex::encode(h.finalize())
}

fn make_zip(entries: Vec<(&str, &[u8])>) -> Vec<u8> {
    let mut buf = std::io::Cursor::new(Vec::new());
    {
        let mut z = zip::ZipWriter::new(&mut buf);
        let opts = zip::write::SimpleFileOptions::default();
        for (name, data) in entries {
            z.start_file(name, opts).unwrap();
            z.write_all(data).unwrap();
        }
        z.finish().unwrap();
    }
    buf.into_inner()
}

fn commit_file(repo: &Repository, rel: &Path, contents: &str, message: &str) -> Oid {
    let workdir = repo.workdir().expect("non-bare repo workdir");
    let abs = workdir.join(rel);
    if let Some(parent) = abs.parent() {
        std::fs::create_dir_all(parent).unwrap();
    }
    std::fs::write(&abs, contents).unwrap();

    let mut index = repo.index().unwrap();
    index.add_path(rel).unwrap();
    index.write().unwrap();
    let tree_id = index.write_tree().unwrap();
    let tree = repo.find_tree(tree_id).unwrap();

    let sig = Signature::now("Test", "test@example.com").unwrap();

    let parents: Vec<git2::Commit<'_>> = repo
        .head()
        .ok()
        .and_then(|h| h.target())
        .and_then(|oid| repo.find_commit(oid).ok())
        .into_iter()
        .collect();

    let parent_refs: Vec<&git2::Commit<'_>> = parents.iter().collect();
    repo.commit(
        Some("HEAD"),
        &sig,
        &sig,
        message,
        &tree,
        parent_refs.as_slice(),
    )
    .unwrap()
}

#[test]
fn zip_slip_rejected() {
    let zip_bytes = make_zip(vec![("../evil.txt", b"nope")]);
    let tmp = tempfile::tempdir().unwrap();
    let out = tmp.path().join("out");

    let err = materialize_zip_bytes(&zip_bytes, &out).unwrap_err();
    assert!(matches!(err, MaterializeError::ZipSlip(_)));
}

#[test]
fn zip_extract_is_deterministic_for_identical_input() {
    let zip_bytes = make_zip(vec![("b.txt", b"world"), ("a.txt", b"hello"), ("dir/sub.txt", b"sub")]);
    let tmp = tempfile::tempdir().unwrap();

    let out1 = tmp.path().join("out1");
    let out2 = tmp.path().join("out2");
    materialize_zip_bytes(&zip_bytes, &out1).unwrap();
    materialize_zip_bytes(&zip_bytes, &out2).unwrap();

    assert_eq!(tree_digest(&out1), tree_digest(&out2));
}

#[test]
fn git_clone_and_checkout_ref_is_deterministic() {
    let tmp = tempfile::tempdir().unwrap();
    let src_repo = tmp.path().join("src_repo");
    std::fs::create_dir_all(&src_repo).unwrap();

    let repo = Repository::init(&src_repo).unwrap();
    let sha1 = commit_file(&repo, Path::new("file.txt"), "v1", "c1");
    let _sha2 = commit_file(&repo, Path::new("file.txt"), "v2", "c2");

    let ref1 = sha1.to_string();
    let out1 = tmp.path().join("clone1");
    let out2 = tmp.path().join("clone2");

    let r1 = materialize_git(src_repo.to_string_lossy().as_ref(), Some(&ref1), &out1).unwrap();
    let r2 = materialize_git(src_repo.to_string_lossy().as_ref(), Some(&ref1), &out2).unwrap();

    assert_eq!(std::fs::read_to_string(r1.repo_root.join("file.txt")).unwrap(), "v1");
    assert_eq!(std::fs::read_to_string(r2.repo_root.join("file.txt")).unwrap(), "v1");
    assert_eq!(tree_digest(&r1.repo_root), tree_digest(&r2.repo_root));
}


