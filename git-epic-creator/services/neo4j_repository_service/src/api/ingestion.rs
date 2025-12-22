use std::collections::HashMap;

use axum::{
    Json,
    extract::{Path, State},
    http::StatusCode,
    response::IntoResponse,
};
use serde::Deserialize;
use serde_json::{Value, json};

use crate::{AppState, api::cypher, http::AppError};

#[derive(Debug, Deserialize)]
pub struct MergeProjectRequest {
    pub project_id: String,
}

#[derive(Debug, Deserialize)]
pub struct MergeRepoRequest {
    pub project_id: String,
    pub repo_fingerprint: String,
}

#[derive(Debug, Deserialize)]
pub struct RowsRequest {
    pub rows: Vec<Value>,
}

#[derive(Debug, Deserialize)]
pub struct EmbeddingsRequest {
    pub project_id: String,
    pub rows: Vec<Value>,
}

pub async fn merge_project(
    State(state): State<AppState>,
    Json(req): Json<MergeProjectRequest>,
) -> Result<impl IntoResponse, AppError> {
    let row = cypher::first_row(
        &state,
        "code_graph/merge_project",
        HashMap::from([("project_id".to_string(), json!(req.project_id))]),
        &["id"],
    )
    .await?;
    Ok((StatusCode::OK, Json(row)))
}

pub async fn merge_repo(
    State(state): State<AppState>,
    Json(req): Json<MergeRepoRequest>,
) -> Result<impl IntoResponse, AppError> {
    let row = cypher::first_row(
        &state,
        "code_graph/merge_repo",
        HashMap::from([
            ("project_id".to_string(), json!(req.project_id)),
            ("repo_fingerprint".to_string(), json!(req.repo_fingerprint)),
        ]),
        &["repo_fingerprint"],
    )
    .await?;
    Ok((StatusCode::OK, Json(row)))
}

pub async fn merge_files(
    State(state): State<AppState>,
    Json(req): Json<RowsRequest>,
) -> Result<impl IntoResponse, AppError> {
    let processed = cypher::count(
        &state,
        "code_graph/merge_files",
        HashMap::from([("rows".to_string(), Value::Array(req.rows))]),
        "n",
    )
    .await?;
    Ok((StatusCode::OK, Json(json!({ "processed": processed }))))
}

pub async fn merge_code_nodes(
    State(state): State<AppState>,
    Json(req): Json<RowsRequest>,
) -> Result<impl IntoResponse, AppError> {
    let processed = cypher::count(
        &state,
        "code_graph/merge_code_nodes_apoc",
        HashMap::from([("rows".to_string(), Value::Array(req.rows))]),
        "n",
    )
    .await?;
    Ok((StatusCode::OK, Json(json!({ "processed": processed }))))
}

pub async fn merge_edges(
    State(state): State<AppState>,
    Path(rel_type): Path<String>,
    Json(req): Json<RowsRequest>,
) -> Result<impl IntoResponse, AppError> {
    let key = code_graph_edges_key(&rel_type)?;
    let processed = cypher::count(
        &state,
        key,
        HashMap::from([("rows".to_string(), Value::Array(req.rows))]),
        "n",
    )
    .await?;
    Ok((StatusCode::OK, Json(json!({ "processed": processed }))))
}

pub async fn rg_merge_documents(
    State(state): State<AppState>,
    Json(req): Json<RowsRequest>,
) -> Result<impl IntoResponse, AppError> {
    let processed = cypher::count(
        &state,
        "requirements_graph/merge_document",
        HashMap::from([("rows".to_string(), Value::Array(req.rows))]),
        "documents_created",
    )
    .await?;
    Ok((StatusCode::OK, Json(json!({ "processed": processed }))))
}

pub async fn rg_merge_chunks(
    State(state): State<AppState>,
    Json(req): Json<RowsRequest>,
) -> Result<impl IntoResponse, AppError> {
    let processed = cypher::count(
        &state,
        "requirements_graph/merge_chunk",
        HashMap::from([("rows".to_string(), Value::Array(req.rows))]),
        "chunks_created",
    )
    .await?;
    Ok((StatusCode::OK, Json(json!({ "processed": processed }))))
}

pub async fn rg_merge_entities(
    State(state): State<AppState>,
    Json(req): Json<RowsRequest>,
) -> Result<impl IntoResponse, AppError> {
    let processed = cypher::count(
        &state,
        "requirements_graph/merge_entity",
        HashMap::from([("rows".to_string(), Value::Array(req.rows))]),
        "entities_created",
    )
    .await?;
    Ok((StatusCode::OK, Json(json!({ "processed": processed }))))
}

pub async fn rg_merge_relationships(
    State(state): State<AppState>,
    Json(req): Json<RowsRequest>,
) -> Result<impl IntoResponse, AppError> {
    let processed = cypher::count(
        &state,
        "requirements_graph/merge_relationship",
        HashMap::from([("rows".to_string(), Value::Array(req.rows))]),
        "relationships_processed",
    )
    .await?;
    Ok((StatusCode::OK, Json(json!({ "processed": processed }))))
}

pub async fn rg_merge_community_reports(
    State(state): State<AppState>,
    Json(req): Json<RowsRequest>,
) -> Result<impl IntoResponse, AppError> {
    let processed = cypher::count(
        &state,
        "requirements_graph/merge_community_report",
        HashMap::from([("rows".to_string(), Value::Array(req.rows))]),
        "community_reports_created",
    )
    .await?;
    Ok((StatusCode::OK, Json(json!({ "processed": processed }))))
}

pub async fn rg_merge_communities(
    State(state): State<AppState>,
    Json(req): Json<RowsRequest>,
) -> Result<impl IntoResponse, AppError> {
    let processed = cypher::count(
        &state,
        "requirements_graph/merge_community",
        HashMap::from([("rows".to_string(), Value::Array(req.rows))]),
        "communities_created",
    )
    .await?;
    Ok((StatusCode::OK, Json(json!({ "processed": processed }))))
}

pub async fn rg_embeddings_chunk_text(
    State(state): State<AppState>,
    Json(req): Json<EmbeddingsRequest>,
) -> Result<impl IntoResponse, AppError> {
    let processed = cypher::count(
        &state,
        "requirements_graph/update_embeddings_chunk_text",
        HashMap::from([
            ("project_id".to_string(), json!(req.project_id)),
            ("rows".to_string(), Value::Array(req.rows)),
        ]),
        "updated",
    )
    .await?;
    Ok((StatusCode::OK, Json(json!({ "updated": processed }))))
}

pub async fn rg_embeddings_entity_description(
    State(state): State<AppState>,
    Json(req): Json<EmbeddingsRequest>,
) -> Result<impl IntoResponse, AppError> {
    let processed = cypher::count(
        &state,
        "requirements_graph/update_embeddings_entity_description",
        HashMap::from([
            ("project_id".to_string(), json!(req.project_id)),
            ("rows".to_string(), Value::Array(req.rows)),
        ]),
        "updated",
    )
    .await?;
    Ok((StatusCode::OK, Json(json!({ "updated": processed }))))
}

pub async fn rg_embeddings_community_full_content(
    State(state): State<AppState>,
    Json(req): Json<EmbeddingsRequest>,
) -> Result<impl IntoResponse, AppError> {
    let processed = cypher::count(
        &state,
        "requirements_graph/update_embeddings_community_full_content",
        HashMap::from([
            ("project_id".to_string(), json!(req.project_id)),
            ("rows".to_string(), Value::Array(req.rows)),
        ]),
        "updated",
    )
    .await?;
    Ok((StatusCode::OK, Json(json!({ "updated": processed }))))
}

#[derive(Debug, Deserialize)]
pub struct ProjectOnlyRequest {
    pub project_id: String,
}

pub async fn rg_backfill_entity_rel_ids(
    State(state): State<AppState>,
    Json(req): Json<ProjectOnlyRequest>,
) -> Result<impl IntoResponse, AppError> {
    cypher::exec_write_no_result(
        &state,
        "requirements_graph/backfill_entity_rel_ids",
        HashMap::from([("project_id".to_string(), json!(req.project_id))]),
    )
    .await?;
    Ok((StatusCode::OK, Json(json!({ "success": true }))))
}

pub async fn rg_backfill_community_membership(
    State(state): State<AppState>,
    Json(req): Json<ProjectOnlyRequest>,
) -> Result<impl IntoResponse, AppError> {
    let row = cypher::first_row(
        &state,
        "requirements_graph/backfill_community_membership",
        HashMap::from([("project_id".to_string(), json!(req.project_id))]),
        &[
            "communities_processed",
            "communities_with_entities_processed",
            "communities_without_entities",
        ],
    )
    .await?;
    Ok((StatusCode::OK, Json(row)))
}

pub async fn rg_backfill_community_hierarchy(
    State(state): State<AppState>,
    Json(req): Json<ProjectOnlyRequest>,
) -> Result<impl IntoResponse, AppError> {
    let row = cypher::first_row(
        &state,
        "requirements_graph/backfill_community_hierarchy",
        HashMap::from([("project_id".to_string(), json!(req.project_id))]),
        &["hierarchy_links_created"],
    )
    .await?;
    Ok((StatusCode::OK, Json(row)))
}

pub async fn rg_backfill_community_ids(
    State(state): State<AppState>,
    Json(_req): Json<Value>,
) -> Result<impl IntoResponse, AppError> {
    let row = cypher::first_row(
        &state,
        "requirements_graph/backfill_community_ids",
        HashMap::new(),
        &["communities_updated"],
    )
    .await?;
    Ok((StatusCode::OK, Json(row)))
}

pub async fn rg_validate_embeddings(
    State(state): State<AppState>,
    Json(req): Json<ProjectOnlyRequest>,
) -> Result<impl IntoResponse, AppError> {
    let embedding_dim = std::env::var("VECTOR_INDEX_DIMENSIONS")
        .ok()
        .and_then(|v| v.parse::<i64>().ok())
        .unwrap_or(3072);
    let row = cypher::first_row(
        &state,
        "requirements_graph/validate_embeddings",
        HashMap::from([
            ("project_id".to_string(), json!(req.project_id)),
            ("embedding_dim".to_string(), json!(embedding_dim)),
        ]),
        &[
            "project_id",
            "total_chunks",
            "chunks_with_embedding",
            "chunks_missing_embedding",
            "total_entities",
            "entities_with_embedding",
            "entities_missing_embedding",
            "total_communities",
            "communities_with_embedding",
            "communities_missing_embedding",
            "community_embedding_coverage",
        ],
    )
    .await?;
    Ok((StatusCode::OK, Json(row)))
}

pub async fn rg_validate_relationships(
    State(state): State<AppState>,
    Json(req): Json<ProjectOnlyRequest>,
) -> Result<impl IntoResponse, AppError> {
    let row = cypher::first_row(
        &state,
        "requirements_graph/validate_relationships",
        HashMap::from([("project_id".to_string(), json!(req.project_id))]),
        &[
            "total_entities",
            "total_relationships",
            "duplicate_count",
            "duplication_ratio",
        ],
    )
    .await?;
    Ok((StatusCode::OK, Json(row)))
}

pub async fn rg_cleanup_duplicate_relationships(
    State(state): State<AppState>,
    Json(req): Json<ProjectOnlyRequest>,
) -> Result<impl IntoResponse, AppError> {
    let row = cypher::first_row(
        &state,
        "requirements_graph/cleanup_duplicate_relationships",
        HashMap::from([("project_id".to_string(), json!(req.project_id))]),
        &["total_duplicates_removed"],
    )
    .await?;
    Ok((StatusCode::OK, Json(row)))
}

pub async fn rg_detect_orphaned_nodes(
    State(state): State<AppState>,
    Json(req): Json<ProjectOnlyRequest>,
) -> Result<impl IntoResponse, AppError> {
    let row = cypher::first_row(
        &state,
        "requirements_graph/detect_orphaned_nodes",
        HashMap::from([("project_id".to_string(), json!(req.project_id))]),
        &[
            "total_documents",
            "total_chunks",
            "total_entities",
            "total_communities",
            "orphaned_chunks",
            "orphaned_entities",
            "orphaned_communities",
            "unlinked_nodes",
            "total_orphaned",
        ],
    )
    .await?;
    Ok((StatusCode::OK, Json(row)))
}

pub async fn rg_cleanup_orphaned_nodes(
    State(state): State<AppState>,
    Json(req): Json<ProjectOnlyRequest>,
) -> Result<impl IntoResponse, AppError> {
    let row = cypher::first_row(
        &state,
        "requirements_graph/cleanup_orphaned_nodes",
        HashMap::from([("project_id".to_string(), json!(req.project_id))]),
        &[
            "orphaned_chunks_removed",
            "orphaned_entities_removed",
            "orphaned_communities_removed",
            "unlinked_nodes_removed",
            "total_removed",
        ],
    )
    .await?;
    Ok((StatusCode::OK, Json(row)))
}

pub async fn rg_sync_entity_relationship_ids(
    State(state): State<AppState>,
    Json(req): Json<ProjectOnlyRequest>,
) -> Result<impl IntoResponse, AppError> {
    let row = cypher::first_row(
        &state,
        "requirements_graph/sync_entity_relationship_ids",
        HashMap::from([("project_id".to_string(), json!(req.project_id))]),
        &["entities_updated"],
    )
    .await?;
    Ok((StatusCode::OK, Json(row)))
}

fn code_graph_edges_key(rel_type: &str) -> Result<&'static str, AppError> {
    let t = rel_type.trim().to_ascii_uppercase();
    Ok(match t.as_str() {
        "CALLS" => "code_graph/merge_edges_calls",
        "PERFORMS" => "code_graph/merge_edges_performs",
        "IMPORTS" => "code_graph/merge_edges_imports",
        "INCLUDES" => "code_graph/merge_edges_includes",
        "READS" => "code_graph/merge_edges_reads",
        "WRITES" => "code_graph/merge_edges_writes",
        "CONFIG_WIRES" => "code_graph/merge_edges_config_wires",
        _ => {
            return Err(AppError::bad_request(format!(
                "unsupported rel_type: {rel_type}"
            )));
        }
    })
}
