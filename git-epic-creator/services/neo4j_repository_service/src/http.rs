use std::collections::HashMap;

use axum::{
    Json, Router,
    extract::{Path, State},
    http::StatusCode,
    response::{IntoResponse, Response},
    routing::{get, post},
};
use serde_json::{Value, json};
use tracing::error;

use crate::{AppState, queries::QueryError};

pub fn router() -> Router<AppState> {
    let mut r = Router::new()
        .route("/health", get(health))
        .route("/health/neo4j", get(health_neo4j))
        .route("/v1/maintenance/init-schema", post(init_schema))
        // Feature APIs (stable, typed)
        .route(
            "/v1/retrieval/primer-context",
            post(crate::api::retrieval::primer_context),
        )
        .route(
            "/v1/retrieval/followup-context",
            post(crate::api::retrieval::followup_context),
        )
        .route(
            "/v1/retrieval/vector/communities",
            post(crate::api::retrieval::vector_communities),
        )
        .route(
            "/v1/retrieval/vector/communities-by-level",
            post(crate::api::retrieval::vector_communities_by_level),
        )
        .route(
            "/v1/retrieval/communities/max-level",
            post(crate::api::retrieval::max_community_level),
        )
        .route(
            "/v1/retrieval/communities/summaries",
            post(crate::api::retrieval::community_summaries),
        )
        .route(
            "/v1/retrieval/chunks/scoped",
            post(crate::api::retrieval::scoped_chunks),
        )
        .route(
            "/v1/retrieval/chunks/neighborhood-minimal",
            post(crate::api::retrieval::neighborhood_minimal),
        )
        // Ingestion APIs (stable, typed)
        .route(
            "/v1/code-graph/merge-project",
            post(crate::api::ingestion::merge_project),
        )
        .route(
            "/v1/code-graph/merge-repo",
            post(crate::api::ingestion::merge_repo),
        )
        .route(
            "/v1/code-graph/merge-files",
            post(crate::api::ingestion::merge_files),
        )
        .route(
            "/v1/code-graph/merge-code-graph",
            post(crate::api::ingestion::merge_code_graph),
        )
        .route(
            "/v1/requirements-graph/merge/documents",
            post(crate::api::ingestion::rg_merge_documents),
        )
        .route(
            "/v1/requirements-graph/merge/chunks",
            post(crate::api::ingestion::rg_merge_chunks),
        )
        .route(
            "/v1/requirements-graph/merge/entities",
            post(crate::api::ingestion::rg_merge_entities),
        )
        .route(
            "/v1/requirements-graph/merge/relationships",
            post(crate::api::ingestion::rg_merge_relationships),
        )
        .route(
            "/v1/requirements-graph/merge/community-reports",
            post(crate::api::ingestion::rg_merge_community_reports),
        )
        .route(
            "/v1/requirements-graph/merge/communities",
            post(crate::api::ingestion::rg_merge_communities),
        )
        .route(
            "/v1/requirements-graph/embeddings/chunk-text",
            post(crate::api::ingestion::rg_embeddings_chunk_text),
        )
        .route(
            "/v1/requirements-graph/embeddings/entity-description",
            post(crate::api::ingestion::rg_embeddings_entity_description),
        )
        .route(
            "/v1/requirements-graph/embeddings/community-full-content",
            post(crate::api::ingestion::rg_embeddings_community_full_content),
        )
        .route(
            "/v1/requirements-graph/backfill/entity-relationship-ids",
            post(crate::api::ingestion::rg_backfill_entity_rel_ids),
        )
        .route(
            "/v1/requirements-graph/backfill/community-membership",
            post(crate::api::ingestion::rg_backfill_community_membership),
        )
        .route(
            "/v1/requirements-graph/backfill/community-ids",
            post(crate::api::ingestion::rg_backfill_community_ids),
        )
        .route(
            "/v1/requirements-graph/backfill/community-hierarchy",
            post(crate::api::ingestion::rg_backfill_community_hierarchy),
        )
        .route(
            "/v1/requirements-graph/validate/embeddings",
            post(crate::api::ingestion::rg_validate_embeddings),
        )
        .route(
            "/v1/requirements-graph/validate/relationships",
            post(crate::api::ingestion::rg_validate_relationships),
        )
        .route(
            "/v1/requirements-graph/cleanup/duplicate-relationships",
            post(crate::api::ingestion::rg_cleanup_duplicate_relationships),
        )
        .route(
            "/v1/requirements-graph/detect/orphaned-nodes",
            post(crate::api::ingestion::rg_detect_orphaned_nodes),
        )
        .route(
            "/v1/requirements-graph/cleanup/orphaned-nodes",
            post(crate::api::ingestion::rg_cleanup_orphaned_nodes),
        )
        .route(
            "/v1/requirements-graph/sync/entity-relationship-ids",
            post(crate::api::ingestion::rg_sync_entity_relationship_ids),
        );

    // Escape hatch (disabled by default): generic cypher execution.
    // This is intentionally hidden behind a prefix and env-guard.
    let unsafe_enabled = std::env::var("NEO4J_REPOSITORY_ENABLE_UNSAFE_ENDPOINTS")
        .ok()
        .map(|v| v == "1" || v.eq_ignore_ascii_case("true"))
        .unwrap_or(false);
    if unsafe_enabled {
        r = r
            .route("/v1/unsafe/queries", get(list_queries))
            .route("/v1/unsafe/execute/{key}", post(execute_query))
            .route("/v1/unsafe/query/{key}", post(query_rows));
    }

    r
}

async fn health() -> impl IntoResponse {
    Json(json!({ "status": "ok" }))
}

async fn health_neo4j(
    axum::extract::State(state): axum::extract::State<AppState>,
) -> Result<Json<Value>, AppError> {
    state
        .executor
        .healthcheck()
        .await
        .map_err(AppError::neo4j)?;

    // Include index health summary in the neo4j health payload so we can remove the Python
    // maintenance service entirely.
    let (index_health_ok, index_health) = index_health_summary(&state).await;

    Ok(Json(json!({
        "status": "ok",
        "neo4j": "connected",
        "indexes": index_health,
        "indexes_ok": index_health_ok
    })))
}

async fn index_health_summary(state: &AppState) -> (bool, Value) {
    // NOTE: SHOW INDEXES is supported in Neo4j 5.x (our target).
    let cypher = r#"
        SHOW INDEXES
        YIELD name, state, populationPercent, type
        RETURN name, state, populationPercent, type
    "#;

    let rows = match state
        .executor
        .execute_read_rows(
            cypher,
            HashMap::new(),
            vec![
                "name".to_string(),
                "state".to_string(),
                "populationPercent".to_string(),
                "type".to_string(),
            ],
            5000,
        )
        .await
    {
        Ok(r) => r,
        Err(e) => {
            return (
                false,
                json!({
                    "status": "unknown",
                    "error": e.to_string()
                }),
            );
        }
    };

    let mut unhealthy: Vec<Value> = Vec::new();
    let mut total = 0usize;

    for r in rows {
        total += 1;
        let name = r
            .get("name")
            .and_then(|v| v.as_str())
            .unwrap_or("")
            .to_string();
        let state_s = r
            .get("state")
            .and_then(|v| v.as_str())
            .unwrap_or("")
            .to_string();
        let idx_type = r
            .get("type")
            .and_then(|v| v.as_str())
            .unwrap_or("")
            .to_string();
        let pop = r
            .get("populationPercent")
            .and_then(|v| v.as_f64())
            .unwrap_or(100.0);

        let mut issues: Vec<String> = Vec::new();
        if state_s != "ONLINE" {
            issues.push(format!("state={state_s}"));
        }
        if idx_type == "VECTOR" && pop < 100.0 {
            issues.push(format!("populationPercent={pop}"));
        }

        if !issues.is_empty() {
            unhealthy.push(json!({
                "name": name,
                "type": idx_type,
                "state": state_s,
                "populationPercent": pop,
                "issues": issues
            }));
        }
    }

    let ok = unhealthy.is_empty();
    let status = if ok { "ok" } else { "degraded" };
    (
        ok,
        json!({
            "status": status,
            "total": total,
            "unhealthy": unhealthy.len(),
            "unhealthy_indexes": unhealthy
        }),
    )
}

async fn init_schema(State(state): State<AppState>) -> Result<Json<Value>, AppError> {
    // Execute all cypher files under queries/schema/** in sorted order.
    let keys = state
        .queries
        .keys()
        .into_iter()
        .filter(|k| k.starts_with("schema/"))
        .collect::<Vec<_>>();

    let mut executed: Vec<String> = Vec::new();
    let mut failed: Vec<Value> = Vec::new();

    for key in keys {
        let cypher = state.queries.get(&key).map_err(AppError::query)?;
        match state.executor.execute_write(cypher, HashMap::new()).await {
            Ok(_summary) => executed.push(key),
            Err(e) => {
                failed.push(json!({
                    "key": key,
                    "error": e.to_string()
                }));
            }
        }
    }

    let ok = failed.is_empty();
    Ok(Json(json!({
        "status": if ok { "ok" } else { "partial" },
        "success": ok,
        "executed": executed,
        "failed": failed,
        "total": executed.len() + failed.len()
    })))
}

async fn list_queries(State(state): State<AppState>) -> impl IntoResponse {
    Json(json!({ "queries": state.queries.keys() }))
}

async fn execute_query(
    State(state): State<AppState>,
    Path(key): Path<String>,
    body: Option<Json<Value>>,
) -> Result<Json<crate::neo4j::ExecuteSummary>, AppError> {
    let cypher = state.queries.get(&key).map_err(AppError::query)?;
    let params = match body.map(|Json(v)| v) {
        None | Some(Value::Null) => HashMap::new(),
        Some(Value::Object(map)) => map.into_iter().collect::<HashMap<_, _>>(),
        Some(other) => {
            return Err(AppError::bad_request(format!(
                "request body must be a JSON object, got: {}",
                other
            )));
        }
    };

    let result = state
        .executor
        .execute_write(cypher, params)
        .await
        .map_err(AppError::neo4j)?;
    Ok(Json(result))
}

#[derive(serde::Deserialize)]
struct QueryRowsRequest {
    /// Parameters for the cypher query (top-level JSON object).
    #[serde(default)]
    params: HashMap<String, Value>,
    /// Which columns to extract from each row (because neo4rs Row doesn't expose keys).
    fields: Vec<String>,
    /// Hard limit for safety.
    #[serde(default = "default_max_rows")]
    max_rows: usize,
}

fn default_max_rows() -> usize {
    1000
}

async fn query_rows(
    State(state): State<AppState>,
    Path(key): Path<String>,
    Json(req): Json<QueryRowsRequest>,
) -> Result<Json<Value>, AppError> {
    if req.fields.is_empty() {
        return Err(AppError::bad_request(
            "fields must be a non-empty array".to_string(),
        ));
    }
    let cypher = state.queries.get(&key).map_err(AppError::query)?;
    let rows = state
        .executor
        .execute_read_rows(cypher, req.params, req.fields, req.max_rows)
        .await
        .map_err(AppError::neo4j)?;
    Ok(Json(json!({ "rows": rows })))
}

#[derive(Debug)]
pub struct AppError {
    status: StatusCode,
    message: String,
}

impl AppError {
    pub fn bad_request(message: String) -> Self {
        Self {
            status: StatusCode::BAD_REQUEST,
            message,
        }
    }

    pub fn query(err: QueryError) -> Self {
        match err {
            QueryError::NotFound(_) => Self {
                status: StatusCode::NOT_FOUND,
                message: err.to_string(),
            },
            _ => Self {
                status: StatusCode::INTERNAL_SERVER_ERROR,
                message: err.to_string(),
            },
        }
    }

    pub fn neo4j(err: crate::neo4j::Neo4jError) -> Self {
        Self {
            status: StatusCode::BAD_GATEWAY,
            message: err.to_string(),
        }
    }
}

impl IntoResponse for AppError {
    fn into_response(self) -> Response {
        if self.status.is_server_error() {
            error!("request_failed: {}", self.message);
        }
        let body = Json(json!({ "error": self.message }));
        (self.status, body).into_response()
    }
}
