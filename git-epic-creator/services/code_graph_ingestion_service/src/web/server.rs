use axum::extract::{Multipart, State};
use axum::extract::rejection::JsonRejection;
use axum::http::StatusCode;
use axum::response::{IntoResponse, Response};
use axum::routing::{get, post};
use axum::{Json, Router};
use serde::{Deserialize, Serialize};
use serde_json::json;
use std::sync::Arc;
use uuid::Uuid;

use crate::core::orchestrator::{ingest_git, ingest_zip, OrchestratorConfig};
use crate::core::types::CodeLanguage;

#[derive(Clone)]
pub struct AppState {
    pub orchestrator: OrchestratorConfig,
}

#[derive(Debug, Deserialize)]
pub struct IngestGitRequest {
    pub project_id: Uuid,
    pub source_language: CodeLanguage,
    pub git_url: String,
    #[serde(rename = "ref")]
    pub ref_name: Option<String>,
}

#[derive(Debug, Serialize)]
pub struct IngestResponse {
    pub project_id: Uuid,
    pub repo_fingerprint: String,
}

pub fn app(state: AppState) -> Router {
    Router::new()
        .route("/health", get(health))
        .route("/health/postgres", get(health_postgres))
        .route("/health/neo4j", get(health_neo4j))
        .route("/ingest/git", post(ingest_git_endpoint))
        .route("/ingest/zip", post(ingest_zip_endpoint))
        .with_state(Arc::new(state))
}

async fn health() -> impl IntoResponse {
    Json(json!({ "status": "ok" }))
}

async fn health_postgres() -> Result<Json<serde_json::Value>, ApiError> {
    let Some(database_url) = std::env::var("DATABASE_URL").ok() else {
        return Ok(Json(json!({ "status": "skipped" })));
    };

    let ok = tokio::task::spawn_blocking(move || -> bool {
        let mut client = match postgres::Client::connect(&database_url, postgres::NoTls) {
            Ok(c) => c,
            Err(_) => return false,
        };
        client.simple_query("SELECT 1").is_ok()
    })
    .await
    .map_err(|e| ApiError::internal(format!("join error: {e}")))?;

    if ok {
        Ok(Json(json!({ "status": "ok" })))
    } else {
        Err(ApiError::service_unavailable("postgres unavailable"))
    }
}

async fn health_neo4j() -> Result<Json<serde_json::Value>, ApiError> {
    let Some(base_url) = std::env::var("NEO4J_REPOSITORY_SERVICE_URL").ok() else {
        return Ok(Json(json!({ "status": "skipped" })));
    };

    let url = format!("{}/health", base_url.trim_end_matches('/'));
    match reqwest::Client::new().get(url).send().await {
        Ok(resp) if resp.status().is_success() => Ok(Json(json!({ "status": "ok" }))),
        _ => Err(ApiError::service_unavailable(
            "neo4j-repository-service unavailable",
        )),
    }
}

async fn ingest_git_endpoint(
    State(state): State<Arc<AppState>>,
    payload: Result<Json<IngestGitRequest>, JsonRejection>,
) -> Result<Json<IngestResponse>, ApiError> {
    let Json(payload) = payload.map_err(|_e| ApiError::unprocessable("invalid json body"))?;
    if payload.git_url.trim().is_empty() {
        return Err(ApiError::unprocessable("git_url must be non-empty"));
    }
    let out = tokio::task::spawn_blocking({
        let cfg = state.orchestrator.clone();
        let payload = payload;
        move || ingest_git(&cfg, payload.project_id, payload.source_language, &payload.git_url, payload.ref_name.as_deref())
    })
    .await
    .map_err(|e| ApiError::internal(format!("join error: {e}")))?
    .map_err(|e| ApiError::internal(e.to_string()))?;

    Ok(Json(IngestResponse {
        project_id: out.project_id,
        repo_fingerprint: out.repo_fingerprint,
    }))
}

async fn ingest_zip_endpoint(
    State(state): State<Arc<AppState>>,
    mut multipart: Multipart,
) -> Result<Json<IngestResponse>, ApiError> {
    let mut project_id: Option<Uuid> = None;
    let mut source_language: Option<CodeLanguage> = None;
    let mut zip_bytes: Option<Vec<u8>> = None;

    while let Some(field) = multipart.next_field().await.map_err(|e| ApiError::unprocessable(e.to_string()))? {
        let name = field.name().unwrap_or("").to_string();
        match name.as_str() {
            "project_id" => {
                let s = field.text().await.map_err(|e| ApiError::unprocessable(e.to_string()))?;
                project_id = Some(s.parse::<Uuid>().map_err(|_| ApiError::unprocessable("project_id must be uuid"))?);
            }
            "source_language" => {
                let s = field.text().await.map_err(|e| ApiError::unprocessable(e.to_string()))?;
                source_language = Some(
                    s.parse::<CodeLanguage>()
                        .map_err(|_| ApiError::unprocessable("unsupported source_language"))?,
                );
            }
            "file" => {
                let data = field.bytes().await.map_err(|e| ApiError::unprocessable(e.to_string()))?;
                zip_bytes = Some(data.to_vec());
            }
            _ => {}
        }
    }

    let project_id = project_id.ok_or_else(|| ApiError::unprocessable("project_id is required"))?;
    let source_language = source_language.ok_or_else(|| ApiError::unprocessable("source_language is required"))?;
    let zip_bytes = zip_bytes.ok_or_else(|| ApiError::unprocessable("file is required"))?;

    let out = tokio::task::spawn_blocking({
        let cfg = state.orchestrator.clone();
        move || ingest_zip(&cfg, project_id, source_language, zip_bytes)
    })
    .await
    .map_err(|e| ApiError::internal(format!("join error: {e}")))?
    .map_err(|e| ApiError::internal(e.to_string()))?;

    Ok(Json(IngestResponse {
        project_id: out.project_id,
        repo_fingerprint: out.repo_fingerprint,
    }))
}

#[derive(Debug)]
pub struct ApiError {
    status: StatusCode,
    message: String,
}

impl ApiError {
    fn unprocessable(msg: impl Into<String>) -> Self {
        Self {
            status: StatusCode::UNPROCESSABLE_ENTITY,
            message: msg.into(),
        }
    }
    fn internal(msg: impl Into<String>) -> Self {
        Self {
            status: StatusCode::INTERNAL_SERVER_ERROR,
            message: msg.into(),
        }
    }

    fn service_unavailable(msg: impl Into<String>) -> Self {
        Self {
            status: StatusCode::SERVICE_UNAVAILABLE,
            message: msg.into(),
        }
    }
}

impl IntoResponse for ApiError {
    fn into_response(self) -> Response {
        (self.status, Json(json!({ "detail": self.message }))).into_response()
    }
}


