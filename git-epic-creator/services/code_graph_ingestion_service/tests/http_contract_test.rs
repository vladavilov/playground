use code_graph_ingestion_service::core::orchestrator::OrchestratorConfig;
use code_graph_ingestion_service::web::server::{app, AppState};
use git2::{Oid, Repository, Signature};
use axum::http::{Request, StatusCode};
use http_body_util::BodyExt;
use serde_json::Value;
use std::io::Write;
use std::path::Path;
use tower::ServiceExt;
use uuid::Uuid;

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

fn multipart_body(boundary: &str, fields: Vec<(&str, &str)>, file_field: (&str, &str, &[u8], &str)) -> Vec<u8> {
    let mut out = Vec::new();
    for (name, value) in fields {
        out.extend_from_slice(format!("--{boundary}\r\n").as_bytes());
        out.extend_from_slice(format!("Content-Disposition: form-data; name=\"{name}\"\r\n\r\n").as_bytes());
        out.extend_from_slice(value.as_bytes());
        out.extend_from_slice(b"\r\n");
    }
    let (field_name, filename, bytes, content_type) = file_field;
    out.extend_from_slice(format!("--{boundary}\r\n").as_bytes());
    out.extend_from_slice(
        format!(
            "Content-Disposition: form-data; name=\"{field_name}\"; filename=\"{filename}\"\r\nContent-Type: {content_type}\r\n\r\n"
        )
        .as_bytes(),
    );
    out.extend_from_slice(bytes);
    out.extend_from_slice(b"\r\n");
    out.extend_from_slice(format!("--{boundary}--\r\n").as_bytes());
    out
}

async fn json_body(resp: axum::response::Response) -> Value {
    let bytes = resp.into_body().collect().await.unwrap().to_bytes();
    serde_json::from_slice(&bytes).unwrap()
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

    repo.commit(Some("HEAD"), &sig, &sig, message, &tree, parent_refs.as_slice())
        .unwrap()
}

#[tokio::test]
async fn health_returns_required_keys() {
    let tmp = tempfile::tempdir().unwrap();
    let state = AppState {
        orchestrator: OrchestratorConfig {
            workspace_root: tmp.path().join("ws"),
        },
    };
    let app = app(state);

    let resp = app
        .oneshot(Request::builder().method("GET").uri("/health").body(axum::body::Body::empty()).unwrap())
        .await
        .unwrap();
    assert_eq!(resp.status(), StatusCode::OK);
    assert_eq!(json_body(resp).await, serde_json::json!({"status":"ok"}));
}

#[tokio::test]
async fn ingest_git_requires_project_id_and_source_language() {
    let tmp = tempfile::tempdir().unwrap();
    let state = AppState {
        orchestrator: OrchestratorConfig {
            workspace_root: tmp.path().join("ws"),
        },
    };
    let app = app(state);

    let missing_project = app
        .clone()
        .oneshot(
            Request::builder()
                .method("POST")
                .uri("/ingest/git")
                .header("content-type", "application/json")
                .body(axum::body::Body::from(
                    serde_json::json!({"source_language":"cobol","git_url":"https://example.com/repo.git"}).to_string(),
                ))
                .unwrap(),
        )
        .await
        .unwrap();
    assert_eq!(missing_project.status(), StatusCode::UNPROCESSABLE_ENTITY);

    let missing_lang = app
        .oneshot(
            Request::builder()
                .method("POST")
                .uri("/ingest/git")
                .header("content-type", "application/json")
                .body(axum::body::Body::from(
                    serde_json::json!({"project_id":Uuid::new_v4(),"git_url":"https://example.com/repo.git"}).to_string(),
                ))
                .unwrap(),
        )
        .await
        .unwrap();
    assert_eq!(missing_lang.status(), StatusCode::UNPROCESSABLE_ENTITY);
}

#[tokio::test]
async fn ingest_git_returns_contract_shape() {
    let tmp = tempfile::tempdir().unwrap();
    let state = AppState {
        orchestrator: OrchestratorConfig {
            workspace_root: tmp.path().join("ws"),
        },
    };
    let app = app(state);

    // local git repo (hermetic)
    let repo_dir = tmp.path().join("src_repo");
    std::fs::create_dir_all(&repo_dir).unwrap();
    let repo = Repository::init(&repo_dir).unwrap();
    let sha = commit_file(&repo, Path::new("A.java"), "class A {}", "c1");
    let ref1 = sha.to_string();

    let pid = Uuid::new_v4();
    let resp = app
        .oneshot(
            Request::builder()
                .method("POST")
                .uri("/ingest/git")
                .header("content-type", "application/json")
                .body(axum::body::Body::from(
                    serde_json::json!({
                        "project_id": pid,
                        "source_language":"java",
                        "git_url": repo_dir.to_string_lossy(),
                        "ref": ref1,
                    })
                    .to_string(),
                ))
                .unwrap(),
        )
        .await
        .unwrap();
    assert_eq!(resp.status(), StatusCode::OK);
    let body = json_body(resp).await;
    assert_eq!(body["project_id"], Value::String(pid.to_string()));
    assert!(body["repo_fingerprint"].as_str().is_some_and(|s| !s.is_empty()));
}

#[tokio::test]
async fn ingest_zip_requires_project_id_and_source_language() {
    let tmp = tempfile::tempdir().unwrap();
    let state = AppState {
        orchestrator: OrchestratorConfig {
            workspace_root: tmp.path().join("ws"),
        },
    };
    let app = app(state);

    // Missing project_id
    let boundary = "XBOUNDARY";
    let body = multipart_body(
        boundary,
        vec![("source_language", "cobol")],
        ("file", "repo.zip", b"not-a-real-zip", "application/zip"),
    );
    let resp = app
        .clone()
        .oneshot(
            Request::builder()
                .method("POST")
                .uri("/ingest/zip")
                .header("content-type", format!("multipart/form-data; boundary={boundary}"))
                .body(axum::body::Body::from(body))
                .unwrap(),
        )
        .await
        .unwrap();
    assert_eq!(resp.status(), StatusCode::UNPROCESSABLE_ENTITY);

    // Missing source_language
    let boundary = "XBOUNDARY2";
    let pid = Uuid::new_v4();
    let body = multipart_body(
        boundary,
        vec![("project_id", &pid.to_string())],
        ("file", "repo.zip", b"not-a-real-zip", "application/zip"),
    );
    let resp = app
        .oneshot(
            Request::builder()
                .method("POST")
                .uri("/ingest/zip")
                .header("content-type", format!("multipart/form-data; boundary={boundary}"))
                .body(axum::body::Body::from(body))
                .unwrap(),
        )
        .await
        .unwrap();
    assert_eq!(resp.status(), StatusCode::UNPROCESSABLE_ENTITY);
}

#[tokio::test]
async fn ingest_zip_returns_contract_shape() {
    let tmp = tempfile::tempdir().unwrap();
    let state = AppState {
        orchestrator: OrchestratorConfig {
            workspace_root: tmp.path().join("ws"),
        },
    };
    let app = app(state);

    let pid = Uuid::new_v4();
    let zip_bytes = make_zip(vec![("hello.js", b"console.log('hi');\n")]);
    let boundary = "XBOUNDARY3";
    let body = multipart_body(
        boundary,
        vec![("project_id", &pid.to_string()), ("source_language", "javascript")],
        ("file", "repo.zip", &zip_bytes, "application/zip"),
    );

    let resp = app
        .oneshot(
            Request::builder()
                .method("POST")
                .uri("/ingest/zip")
                .header("content-type", format!("multipart/form-data; boundary={boundary}"))
                .body(axum::body::Body::from(body))
                .unwrap(),
        )
        .await
        .unwrap();
    assert_eq!(resp.status(), StatusCode::OK);
    let body = json_body(resp).await;
    assert_eq!(body["project_id"], Value::String(pid.to_string()));
    assert!(body["repo_fingerprint"].as_str().is_some_and(|s| !s.is_empty()));
}

#[tokio::test]
async fn zip_ingest_changes_across_entry_order() {
    let tmp = tempfile::tempdir().unwrap();
    let state = AppState {
        orchestrator: OrchestratorConfig {
            workspace_root: tmp.path().join("ws"),
        },
    };
    let app = app(state);

    let pid = Uuid::new_v4();
    let z1 = make_zip(vec![("a.js", b"1"), ("b.js", b"2")]);
    let z2 = make_zip(vec![("b.js", b"2"), ("a.js", b"1")]);

    let b1 = "B1";
    let r1 = app
        .clone()
        .oneshot(
            Request::builder()
                .method("POST")
                .uri("/ingest/zip")
                .header("content-type", format!("multipart/form-data; boundary={b1}"))
                .body(axum::body::Body::from(multipart_body(
                    b1,
                    vec![("project_id", &pid.to_string()), ("source_language", "javascript")],
                    ("file", "repo.zip", &z1, "application/zip"),
                )))
                .unwrap(),
        )
        .await
        .unwrap();
    let fp1 = json_body(r1).await["repo_fingerprint"].as_str().unwrap().to_string();

    let b2 = "B2";
    let r2 = app
        .oneshot(
            Request::builder()
                .method("POST")
                .uri("/ingest/zip")
                .header("content-type", format!("multipart/form-data; boundary={b2}"))
                .body(axum::body::Body::from(multipart_body(
                    b2,
                    vec![("project_id", &pid.to_string()), ("source_language", "javascript")],
                    ("file", "repo.zip", &z2, "application/zip"),
                )))
                .unwrap(),
        )
        .await
        .unwrap();
    let fp2 = json_body(r2).await["repo_fingerprint"].as_str().unwrap().to_string();

    assert_ne!(fp1, fp2);
}

#[tokio::test]
async fn git_ingest_is_deterministic_for_same_ref() {
    let tmp = tempfile::tempdir().unwrap();
    let state = AppState {
        orchestrator: OrchestratorConfig {
            workspace_root: tmp.path().join("ws"),
        },
    };
    let app = app(state);

    let repo_dir = tmp.path().join("repo2");
    std::fs::create_dir_all(&repo_dir).unwrap();
    let repo = Repository::init(&repo_dir).unwrap();
    let sha = commit_file(&repo, Path::new("A.java"), "class A{}", "c1");
    let ref1 = sha.to_string();

    let pid1 = Uuid::new_v4();
    let pid2 = Uuid::new_v4();

    let r1 = app
        .clone()
        .oneshot(
            Request::builder()
                .method("POST")
                .uri("/ingest/git")
                .header("content-type", "application/json")
                .body(axum::body::Body::from(
                    serde_json::json!({
                        "project_id": pid1,
                        "source_language":"java",
                        "git_url": repo_dir.to_string_lossy(),
                        "ref": ref1,
                    })
                    .to_string(),
                ))
                .unwrap(),
        )
        .await
        .unwrap();
    let fp1 = json_body(r1).await["repo_fingerprint"].as_str().unwrap().to_string();

    let r2 = app
        .oneshot(
            Request::builder()
                .method("POST")
                .uri("/ingest/git")
                .header("content-type", "application/json")
                .body(axum::body::Body::from(
                    serde_json::json!({
                        "project_id": pid2,
                        "source_language":"java",
                        "git_url": repo_dir.to_string_lossy(),
                        "ref": ref1,
                    })
                    .to_string(),
                ))
                .unwrap(),
        )
        .await
        .unwrap();
    let fp2 = json_body(r2).await["repo_fingerprint"].as_str().unwrap().to_string();

    assert_eq!(fp1, fp2);
}


