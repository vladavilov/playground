mod common;

use axum::{
    body::{Body, to_bytes},
    http::{Request, StatusCode},
};
use serde_json::json;
use tower::ServiceExt;

use neo4j_repository_service::queries::QueryRegistry;

#[tokio::test]
async fn merge_code_graph_rejects_unknown_rel_type() {
    let queries = QueryRegistry::load_from_dir("queries").unwrap();
    let (state, _mock) = common::state_with_queries(queries, vec![]);
    let app = common::app(state);

    let resp = app
        .clone()
        .oneshot(
            Request::builder()
                .method("POST")
                .uri("/v1/code-graph/merge-code-graph")
                .header("content-type", "application/json")
                .body(Body::from(
                    json!({
                        "project_id": "p1",
                        "repo_fingerprint": "r1",
                        "files": [],
                        "nodes": [],
                        "edges": {
                            "unknown": []
                        }
                    })
                    .to_string(),
                ))
                .unwrap(),
        )
        .await
        .unwrap();

    // empty rows are ignored; add a non-empty unknown type to trigger validation
    assert_eq!(resp.status(), StatusCode::OK);

    let resp = app
        .oneshot(
            Request::builder()
                .method("POST")
                .uri("/v1/code-graph/merge-code-graph")
                .header("content-type", "application/json")
                .body(Body::from(
                    json!({
                        "project_id": "p1",
                        "repo_fingerprint": "r1",
                        "files": [],
                        "nodes": [],
                        "edges": {
                            "unknown": [ {"a": 1} ]
                        }
                    })
                    .to_string(),
                ))
                .unwrap(),
        )
        .await
        .unwrap();
    assert_eq!(resp.status(), StatusCode::BAD_REQUEST);
}

#[tokio::test]
async fn merge_code_graph_returns_processed_counts() {
    let queries = QueryRegistry::load_from_dir("queries").unwrap();
    let (state, _mock) = common::state_with_queries(
        queries,
        vec![
            (
                "code_graph/merge_code_graph_full_apoc",
                vec![
                    common::Row::from([
                        ("files_processed".to_string(), json!(1)),
                        ("nodes_processed".to_string(), json!(2)),
                        ("edges_processed_total".to_string(), json!(3)),
                        ("edges_by_type".to_string(), json!({"CALLS": 3})),
                    ]),
                ],
            ),
        ],
    );
    let app = common::app(state);

    let resp = app
        .oneshot(
            Request::builder()
                .method("POST")
                .uri("/v1/code-graph/merge-code-graph")
                .header("content-type", "application/json")
                .body(Body::from(
                    json!({
                        "project_id": "p1",
                        "repo_fingerprint": "r1",
                        "files": [ {"file_path": "a.cbl", "sha256": "x", "language": "cobol", "line_count": 1} ],
                        "nodes": [ {"a": 1} ],
                        "edges": {
                            "calls": [ {"a": 1} ]
                        }
                    })
                    .to_string(),
                ))
                .unwrap(),
        )
        .await
        .unwrap();

    assert_eq!(resp.status(), StatusCode::OK);
    let body = to_bytes(resp.into_body(), usize::MAX).await.unwrap();
    let v: serde_json::Value = serde_json::from_slice(&body).unwrap();
    assert_eq!(v["files_processed"], json!(1));
    assert_eq!(v["nodes_processed"], json!(2));
    assert_eq!(v["edges_processed_total"], json!(3));
    assert_eq!(v["edges_by_type"]["CALLS"], json!(3));
}

#[tokio::test]
async fn backfill_entity_relationship_ids_uses_write_and_returns_success_true() {
    let queries = QueryRegistry::load_from_dir("queries").unwrap();
    let (state, mock) = common::state_with_queries(queries, vec![]);
    let app = common::app(state);

    let resp = app
        .oneshot(
            Request::builder()
                .method("POST")
                .uri("/v1/requirements-graph/backfill/entity-relationship-ids")
                .header("content-type", "application/json")
                .body(Body::from(json!({ "project_id": "p1" }).to_string()))
                .unwrap(),
        )
        .await
        .unwrap();

    assert_eq!(resp.status(), StatusCode::OK);
    let body = to_bytes(resp.into_body(), usize::MAX).await.unwrap();
    let v: serde_json::Value = serde_json::from_slice(&body).unwrap();
    assert_eq!(v["success"], json!(true));

    let calls = mock.calls.lock().unwrap();
    assert!(calls.iter().any(|c| matches!(c, common::Call::Write { .. })));
}



