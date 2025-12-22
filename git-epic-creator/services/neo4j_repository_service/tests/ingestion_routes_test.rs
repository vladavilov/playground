mod common;

use axum::{
    body::{Body, to_bytes},
    http::{Request, StatusCode},
};
use serde_json::json;
use tower::ServiceExt;

use neo4j_repository_service::queries::QueryRegistry;

#[tokio::test]
async fn merge_edges_rejects_unknown_rel_type() {
    let queries = QueryRegistry::load_from_dir("queries").unwrap();
    let (state, _mock) = common::state_with_queries(queries, vec![]);
    let app = common::app(state);

    let resp = app
        .oneshot(
            Request::builder()
                .method("POST")
                .uri("/v1/code-graph/merge-edges/unknown")
                .header("content-type", "application/json")
                .body(Body::from(json!({ "rows": [] }).to_string()))
                .unwrap(),
        )
        .await
        .unwrap();

    assert_eq!(resp.status(), StatusCode::BAD_REQUEST);
}

#[tokio::test]
async fn merge_edges_calls_returns_processed_count() {
    let queries = QueryRegistry::load_from_dir("queries").unwrap();
    let (state, _mock) = common::state_with_queries(
        queries,
        vec![(
            "code_graph/merge_edges_calls",
            vec![common::Row::from([("n".to_string(), json!(3))])],
        )],
    );
    let app = common::app(state);

    let resp = app
        .oneshot(
            Request::builder()
                .method("POST")
                .uri("/v1/code-graph/merge-edges/calls")
                .header("content-type", "application/json")
                .body(Body::from(json!({ "rows": [ {"a":1} ] }).to_string()))
                .unwrap(),
        )
        .await
        .unwrap();

    assert_eq!(resp.status(), StatusCode::OK);
    let body = to_bytes(resp.into_body(), usize::MAX).await.unwrap();
    let v: serde_json::Value = serde_json::from_slice(&body).unwrap();
    assert_eq!(v["processed"], json!(3));
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

