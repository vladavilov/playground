mod common;

use axum::{
    body::{Body, to_bytes},
    http::{Request, StatusCode},
};
use serde_json::json;
use tower::ServiceExt;

use neo4j_repository_service::queries::QueryRegistry;

#[tokio::test]
async fn health_is_ok() {
    let queries = QueryRegistry::load_from_dir("queries").unwrap();
    let (state, _mock) = common::state_with_queries(queries, vec![]);
    let app = common::app(state);

    let resp = app
        .oneshot(Request::builder().uri("/health").body(Body::empty()).unwrap())
        .await
        .unwrap();

    assert_eq!(resp.status(), StatusCode::OK);
    let body = to_bytes(resp.into_body(), usize::MAX).await.unwrap();
    let v: serde_json::Value = serde_json::from_slice(&body).unwrap();
    assert_eq!(v, json!({"status":"ok"}));
}

#[tokio::test]
async fn unsafe_endpoints_are_disabled_by_default() {
    let queries = QueryRegistry::load_from_dir("queries").unwrap();
    let (state, _mock) = common::state_with_queries(queries, vec![]);
    let app = common::app(state);

    let resp = app
        .oneshot(
            Request::builder()
                .uri("/v1/unsafe/queries")
                .body(Body::empty())
                .unwrap(),
        )
        .await
        .unwrap();

    assert_eq!(resp.status(), StatusCode::NOT_FOUND);
}

#[tokio::test]
async fn init_schema_executes_schema_queries() {
    let queries = QueryRegistry::load_from_dir("queries").unwrap();
    let (state, mock) = common::state_with_queries(queries, vec![]);
    let app = common::app(state);

    let resp = app
        .oneshot(
            Request::builder()
                .method("POST")
                .uri("/v1/maintenance/init-schema")
                .body(Body::empty())
                .unwrap(),
        )
        .await
        .unwrap();

    assert_eq!(resp.status(), StatusCode::OK);

    // Ensure at least one schema query was attempted (repo includes many schema/*.cypher files).
    let calls = mock.calls.lock().unwrap();
    assert!(calls.iter().any(|c| matches!(c, common::Call::Write { .. })));
}

#[tokio::test]
async fn health_neo4j_includes_index_health_summary() {
    // The handler uses a hard-coded cypher "SHOW INDEXES ...", so we stub by cypher text.
    let queries = QueryRegistry::load_from_dir("queries").unwrap();
    let show_indexes_cypher = r#"
        SHOW INDEXES
        YIELD name, state, populationPercent, type
        RETURN name, state, populationPercent, type
    "#;

    let (state, mock) = common::state_with_queries(queries, vec![]);
    {
        let mut map = mock.rows_by_cypher.lock().unwrap();
        map.insert(
            show_indexes_cypher.trim().to_string(),
            vec![
                common::Row::from([
                    ("name".to_string(), json!("vec_idx")),
                    ("state".to_string(), json!("POPULATING")),
                    ("populationPercent".to_string(), json!(42.0)),
                    ("type".to_string(), json!("VECTOR")),
                ]),
                common::Row::from([
                    ("name".to_string(), json!("fts_idx")),
                    ("state".to_string(), json!("ONLINE")),
                    ("populationPercent".to_string(), json!(100.0)),
                    ("type".to_string(), json!("FULLTEXT")),
                ]),
            ],
        );
    }

    let app = common::app(state);
    let resp = app
        .oneshot(
            Request::builder()
                .uri("/health/neo4j")
                .body(Body::empty())
                .unwrap(),
        )
        .await
        .unwrap();

    assert_eq!(resp.status(), StatusCode::OK);
    let body = to_bytes(resp.into_body(), usize::MAX).await.unwrap();
    let v: serde_json::Value = serde_json::from_slice(&body).unwrap();
    assert_eq!(v["status"], json!("ok"));
    assert_eq!(v["indexes_ok"], json!(false));
    assert_eq!(v["indexes"]["status"], json!("degraded"));
    assert_eq!(v["indexes"]["unhealthy"], json!(1));
}



