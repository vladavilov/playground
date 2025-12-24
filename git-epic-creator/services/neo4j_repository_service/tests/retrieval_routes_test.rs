mod common;

use std::collections::HashMap;

use axum::{
    body::{Body, to_bytes},
    http::{Request, StatusCode},
};
use serde_json::json;
use tower::ServiceExt;

use neo4j_repository_service::queries::QueryRegistry;

#[tokio::test]
async fn retrieval_primer_context_returns_communities_and_brief() {
    let queries = QueryRegistry::load_from_dir("queries").unwrap();
    let (state, _mock) = common::state_with_queries(
        queries.clone(),
        vec![
            (
                "requirements_graph/retrieval/get_max_community_level",
                vec![HashMap::from([("max_level".to_string(), json!(2))])],
            ),
            (
                "requirements_graph/retrieval/vector_query_communities_by_level",
                vec![
                    HashMap::from([
                        ("community".to_string(), json!(10)),
                        ("score".to_string(), json!(0.9)),
                    ]),
                    HashMap::from([
                        ("community".to_string(), json!(11)),
                        ("score".to_string(), json!(0.8)),
                    ]),
                ],
            ),
            (
                "requirements_graph/retrieval/fetch_community_summaries",
                vec![
                    HashMap::from([
                        ("id".to_string(), json!(10)),
                        ("summary".to_string(), json!("s10")),
                    ]),
                    HashMap::from([
                        ("id".to_string(), json!(11)),
                        ("summary".to_string(), json!("s11")),
                    ]),
                ],
            ),
        ],
    );

    let app = common::app(state);
    let req = Request::builder()
        .method("POST")
        .uri("/v1/retrieval/primer-context")
        .header("content-type", "application/json")
        .body(Body::from(
            json!({
                "project_id": "p1",
                "community_index_name": "idx",
                "k": 2,
                "qvec": [0.1, 0.2]
            })
            .to_string(),
        ))
        .unwrap();

    let resp = app.oneshot(req).await.unwrap();
    assert_eq!(resp.status(), StatusCode::OK);
    let body = to_bytes(resp.into_body(), usize::MAX).await.unwrap();
    let v: serde_json::Value = serde_json::from_slice(&body).unwrap();
    assert_eq!(v["communities"], json!([10, 11]));
    assert_eq!(v["community_brief"][0]["summary"], json!("s10"));
}

#[tokio::test]
async fn retrieval_primer_context_rejects_empty_index_name() {
    let queries = QueryRegistry::load_from_dir("queries").unwrap();
    let (state, _mock) = common::state_with_queries(queries, vec![]);
    let app = common::app(state);

    let req = Request::builder()
        .method("POST")
        .uri("/v1/retrieval/primer-context")
        .header("content-type", "application/json")
        .body(Body::from(
            json!({
                "project_id": "p1",
                "community_index_name": " ",
                "k": 2,
                "qvec": [0.1, 0.2]
            })
            .to_string(),
        ))
        .unwrap();

    let resp = app.oneshot(req).await.unwrap();
    assert_eq!(resp.status(), StatusCode::BAD_REQUEST);
}

#[tokio::test]
async fn retrieval_primer_context_falls_back_to_vector_query_nodes_when_no_levels() {
    let queries = QueryRegistry::load_from_dir("queries").unwrap();
    let (state, _mock) = common::state_with_queries(
        queries,
        vec![
            (
                "requirements_graph/retrieval/get_max_community_level",
                vec![HashMap::from([("max_level".to_string(), json!(0))])],
            ),
            (
                "requirements_graph/retrieval/vector_query_nodes",
                vec![HashMap::from([
                    ("community".to_string(), json!(7)),
                    ("score".to_string(), json!(0.99)),
                ])],
            ),
            (
                "requirements_graph/retrieval/fetch_community_summaries",
                vec![HashMap::from([
                    ("id".to_string(), json!(7)),
                    ("summary".to_string(), json!("s7")),
                ])],
            ),
        ],
    );
    let app = common::app(state);

    let req = Request::builder()
        .method("POST")
        .uri("/v1/retrieval/primer-context")
        .header("content-type", "application/json")
        .body(Body::from(
            json!({
                "project_id": "p1",
                "community_index_name": "idx",
                "k": 1,
                "qvec": [0.1, 0.2]
            })
            .to_string(),
        ))
        .unwrap();

    let resp = app.oneshot(req).await.unwrap();
    assert_eq!(resp.status(), StatusCode::OK);
    let body = to_bytes(resp.into_body(), usize::MAX).await.unwrap();
    let v: serde_json::Value = serde_json::from_slice(&body).unwrap();
    assert_eq!(v["communities"], json!([7]));
    assert_eq!(v["community_brief"][0]["summary"], json!("s7"));
}

#[tokio::test]
async fn retrieval_followup_context_returns_empty_when_no_community_ids() {
    let queries = QueryRegistry::load_from_dir("queries").unwrap();
    let (state, _mock) = common::state_with_queries(queries, vec![]);
    let app = common::app(state);

    let req = Request::builder()
        .method("POST")
        .uri("/v1/retrieval/followup-context")
        .header("content-type", "application/json")
        .body(Body::from(
            json!({
                "project_id": "p1",
                "chunk_index_name": "chunk_idx",
                "community_ids": [],
                "k": 3,
                "qvec": [0.1, 0.2]
            })
            .to_string(),
        ))
        .unwrap();

    let resp = app.oneshot(req).await.unwrap();
    assert_eq!(resp.status(), StatusCode::OK);
    let body = to_bytes(resp.into_body(), usize::MAX).await.unwrap();
    let v: serde_json::Value = serde_json::from_slice(&body).unwrap();
    assert_eq!(v["chunk_ids"], json!([]));
    assert_eq!(v["neighborhoods"], json!([]));
}

#[tokio::test]
async fn retrieval_followup_context_orders_neighborhoods_to_match_chunk_ids() {
    let queries = QueryRegistry::load_from_dir("queries").unwrap();
    let (state, _mock) = common::state_with_queries(
        queries,
        vec![
            (
                "requirements_graph/retrieval/optimized_scoped_chunks",
                vec![
                    HashMap::from([
                        ("chunk_id".to_string(), json!("c2")),
                        ("score".to_string(), json!(0.9)),
                    ]),
                    HashMap::from([
                        ("chunk_id".to_string(), json!("c1")),
                        ("score".to_string(), json!(0.8)),
                    ]),
                ],
            ),
            (
                "requirements_graph/retrieval/expand_neighborhood_minimal",
                vec![
                    HashMap::from([("chunk_id".to_string(), json!("c1"))]),
                    HashMap::from([("chunk_id".to_string(), json!("c2"))]),
                ],
            ),
        ],
    );
    let app = common::app(state);

    let req = Request::builder()
        .method("POST")
        .uri("/v1/retrieval/followup-context")
        .header("content-type", "application/json")
        .body(Body::from(
            json!({
                "project_id": "p1",
                "chunk_index_name": "chunk_idx",
                "community_ids": [10],
                "k": 2,
                "qvec": [0.1, 0.2]
            })
            .to_string(),
        ))
        .unwrap();

    let resp = app.oneshot(req).await.unwrap();
    assert_eq!(resp.status(), StatusCode::OK);
    let body = to_bytes(resp.into_body(), usize::MAX).await.unwrap();
    let v: serde_json::Value = serde_json::from_slice(&body).unwrap();
    assert_eq!(v["chunk_ids"], json!(["c2", "c1"]));
    assert_eq!(v["neighborhoods"][0]["chunk_id"], json!("c2"));
    assert_eq!(v["neighborhoods"][1]["chunk_id"], json!("c1"));
}


