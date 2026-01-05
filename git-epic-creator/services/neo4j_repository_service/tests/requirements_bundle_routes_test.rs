mod common;

use axum::{
    body::{Body, to_bytes},
    http::{Request, StatusCode},
};
use serde_json::json;
use tower::ServiceExt;

use neo4j_repository_service::queries::QueryRegistry;

#[tokio::test]
async fn requirements_bundle_returns_counts_from_single_query() {
    common::set_local_jwt_secret();
    let queries = QueryRegistry::load_from_dir("queries").unwrap();
    let (state, _mock) = common::state_with_queries(
        queries,
        vec![(
            "requirements_graph/merge_bundle_full",
            vec![common::Row::from([
                ("documents_created".to_string(), json!(1)),
                ("chunks_created".to_string(), json!(2)),
                ("entities_created".to_string(), json!(3)),
                ("relationships_processed".to_string(), json!(4)),
                ("community_reports_created".to_string(), json!(5)),
                ("communities_created".to_string(), json!(6)),
            ])],
        )],
    );
    let app = common::app(state);

    let resp = app
        .oneshot(
            Request::builder()
                .method("POST")
                .uri("/v1/requirements-graph/merge/bundle")
                .header("authorization", common::s2s_auth_header_value())
                .header("content-type", "application/json")
                .body(Body::from(
                    json!({
                        "project_id": "p1",
                        "documents": [ {"id":"d1"} ],
                        "chunks": [ {"id":"c1","document_ids":["d1"]} ],
                        "entities": [ {"id":"e1","text_unit_ids":["c1"]} ],
                        "relationships": [ {"source":"A","target":"B"} ],
                        "community_reports": [ {"community": 1, "level": 0, "title":"t"} ],
                        "communities": [ {"community": 1, "entity_ids":["e1"]} ]
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
    assert_eq!(v["documents_created"], json!(1));
    assert_eq!(v["chunks_created"], json!(2));
    assert_eq!(v["entities_created"], json!(3));
    assert_eq!(v["relationships_processed"], json!(4));
    assert_eq!(v["community_reports_created"], json!(5));
    assert_eq!(v["communities_created"], json!(6));
}





