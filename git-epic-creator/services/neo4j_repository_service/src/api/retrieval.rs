use std::collections::HashMap;

use axum::{Json, extract::State, http::StatusCode, response::IntoResponse};
use serde::{Deserialize, Serialize};
use serde_json::{Value, json};

use crate::{AppState, api::cypher, http::AppError};

#[derive(Debug, Deserialize)]
pub struct VectorCommunitiesRequest {
    pub project_id: String,
    pub index_name: String,
    pub k: i64,
    pub qvec: Vec<f64>,
}

#[derive(Debug, Deserialize)]
pub struct VectorCommunitiesByLevelRequest {
    pub project_id: String,
    pub index_name: String,
    pub k: i64,
    pub qvec: Vec<f64>,
    pub level: i64,
}

#[derive(Debug, Deserialize)]
pub struct MaxCommunityLevelRequest {
    pub project_id: String,
}

#[derive(Debug, Serialize)]
pub struct MaxCommunityLevelResponse {
    pub max_level: i64,
}

#[derive(Debug, Deserialize)]
pub struct CommunitySummariesRequest {
    pub project_id: String,
    pub ids: Vec<i64>,
}

#[derive(Debug, Serialize)]
pub struct CommunitySummary {
    pub id: i64,
    pub summary: String,
}

#[derive(Debug, Deserialize)]
pub struct ScopedChunksRequest {
    pub project_id: String,
    pub community_ids: Vec<i64>,
    pub chunk_index: String,
    pub qvec: Vec<f64>,
    pub limit: i64,
}

#[derive(Debug, Deserialize)]
pub struct NeighborhoodRequest {
    pub project_id: String,
    pub chunk_ids: Vec<String>,
    #[serde(default = "default_max_chunk_len")]
    pub max_chunk_len: i64,
}

fn default_max_chunk_len() -> i64 {
    1500
}

#[derive(Debug, Deserialize)]
pub struct PrimerContextRequest {
    pub project_id: String,
    pub community_index_name: String,
    pub k: i64,
    pub qvec: Vec<f64>,
}

#[derive(Debug, Serialize)]
pub struct PrimerContextResponse {
    pub communities: Vec<i64>,
    pub community_brief: Vec<HashMap<String, Value>>,
}

#[derive(Debug, Deserialize)]
pub struct FollowupContextRequest {
    pub project_id: String,
    pub chunk_index_name: String,
    pub community_ids: Vec<i64>,
    pub k: i64,
    pub qvec: Vec<f64>,
    #[serde(default = "default_max_chunk_len")]
    pub max_chunk_len: i64,
}

#[derive(Debug, Serialize)]
pub struct FollowupContextResponse {
    /// Chunk ids in the same order as the vector query returns (score-desc).
    pub chunk_ids: Vec<String>,
    /// Expanded neighborhoods for the chunk_ids list.
    pub neighborhoods: Vec<HashMap<String, Value>>,
}

pub async fn vector_communities(
    State(state): State<AppState>,
    Json(req): Json<VectorCommunitiesRequest>,
) -> Result<impl IntoResponse, AppError> {
    if req.k <= 0 {
        return Err(AppError::bad_request("k must be > 0".to_string()));
    }
    let rows = cypher::query_rows(
        &state,
        "requirements_graph/retrieval/vector_query_nodes",
        HashMap::from([
            ("projectId".to_string(), json!(req.project_id)),
            ("name".to_string(), json!(req.index_name)),
            ("k".to_string(), json!(req.k)),
            ("qvec".to_string(), json!(req.qvec)),
        ]),
        &["community", "score"],
        req.k as usize,
    )
    .await?;

    Ok((StatusCode::OK, Json(json!({ "communities": rows }))))
}

pub async fn vector_communities_by_level(
    State(state): State<AppState>,
    Json(req): Json<VectorCommunitiesByLevelRequest>,
) -> Result<impl IntoResponse, AppError> {
    if req.k <= 0 {
        return Err(AppError::bad_request("k must be > 0".to_string()));
    }
    let rows = cypher::query_rows(
        &state,
        "requirements_graph/retrieval/vector_query_communities_by_level",
        HashMap::from([
            ("projectId".to_string(), json!(req.project_id)),
            ("name".to_string(), json!(req.index_name)),
            ("k".to_string(), json!(req.k)),
            ("qvec".to_string(), json!(req.qvec)),
            ("level".to_string(), json!(req.level)),
        ]),
        &["community", "score"],
        req.k as usize,
    )
    .await?;

    Ok((StatusCode::OK, Json(json!({ "communities": rows }))))
}

pub async fn max_community_level(
    State(state): State<AppState>,
    Json(req): Json<MaxCommunityLevelRequest>,
) -> Result<impl IntoResponse, AppError> {
    let rows = cypher::query_rows(
        &state,
        "requirements_graph/retrieval/get_max_community_level",
        HashMap::from([("projectId".to_string(), json!(req.project_id))]),
        &["max_level"],
        1,
    )
    .await?;

    let max_level = rows
        .first()
        .and_then(|m| m.get("max_level"))
        .and_then(|v| v.as_i64())
        .unwrap_or(0);

    Ok((
        StatusCode::OK,
        Json(MaxCommunityLevelResponse { max_level }),
    ))
}

pub async fn community_summaries(
    State(state): State<AppState>,
    Json(req): Json<CommunitySummariesRequest>,
) -> Result<impl IntoResponse, AppError> {
    let rows = cypher::query_rows(
        &state,
        "requirements_graph/retrieval/fetch_community_summaries",
        HashMap::from([
            ("projectId".to_string(), json!(req.project_id)),
            ("ids".to_string(), json!(req.ids)),
        ]),
        &["id", "summary"],
        10_000,
    )
    .await?;

    Ok((StatusCode::OK, Json(json!({ "summaries": rows }))))
}

pub async fn scoped_chunks(
    State(state): State<AppState>,
    Json(req): Json<ScopedChunksRequest>,
) -> Result<impl IntoResponse, AppError> {
    if req.limit <= 0 {
        return Err(AppError::bad_request("limit must be > 0".to_string()));
    }

    let rows = cypher::query_rows(
        &state,
        "requirements_graph/retrieval/optimized_scoped_chunks",
        HashMap::from([
            ("projectId".to_string(), json!(req.project_id)),
            ("cids".to_string(), json!(req.community_ids)),
            ("chunkIndex".to_string(), json!(req.chunk_index)),
            ("qvec".to_string(), json!(req.qvec)),
            ("limit".to_string(), json!(req.limit)),
        ]),
        &["chunk_id", "score"],
        req.limit as usize,
    )
    .await?;

    Ok((StatusCode::OK, Json(json!({ "chunks": rows }))))
}

pub async fn neighborhood_minimal(
    State(state): State<AppState>,
    Json(req): Json<NeighborhoodRequest>,
) -> Result<impl IntoResponse, AppError> {
    let max_len = if req.max_chunk_len <= 0 {
        default_max_chunk_len()
    } else {
        req.max_chunk_len
    };

    // Safety cap to avoid unbounded fanout payloads.
    let chunk_ids: Vec<String> = req.chunk_ids.into_iter().take(50).collect();

    let rows = cypher::query_rows(
        &state,
        "requirements_graph/retrieval/expand_neighborhood_minimal",
        HashMap::from([
            ("projectId".to_string(), json!(req.project_id)),
            ("chunkIds".to_string(), json!(chunk_ids)),
            ("maxChunkLen".to_string(), json!(max_len)),
        ]),
        &[
            "chunk_id",
            "text",
            "document_name",
            "neighbours",
            "related_entities",
            "relationships",
            "neighbor_chunk_ids",
        ],
        50,
    )
    .await?;

    Ok((StatusCode::OK, Json(json!({ "neighborhoods": rows }))))
}

pub async fn primer_context(
    State(state): State<AppState>,
    Json(req): Json<PrimerContextRequest>,
) -> Result<impl IntoResponse, AppError> {
    if req.k <= 0 {
        return Err(AppError::bad_request("k must be > 0".to_string()));
    }
    if req.community_index_name.trim().is_empty() {
        return Err(AppError::bad_request(
            "community_index_name must be a non-empty string".to_string(),
        ));
    }

    // 1) max level
    let rows = cypher::query_rows(
        &state,
        "requirements_graph/retrieval/get_max_community_level",
        HashMap::from([("projectId".to_string(), json!(req.project_id))]),
        &["max_level"],
        1,
    )
    .await?;
    let max_level = rows
        .first()
        .and_then(|m| m.get("max_level"))
        .and_then(|v| v.as_i64())
        .unwrap_or(0);

    // 2) vector query communities (with fallback)
    let mut communities: Vec<i64> = Vec::new();
    if max_level > 0 {
        let mut top = cypher::query_rows(
            &state,
            "requirements_graph/retrieval/vector_query_communities_by_level",
            HashMap::from([
                ("projectId".to_string(), json!(req.project_id)),
                ("name".to_string(), json!(req.community_index_name)),
                ("k".to_string(), json!(req.k)),
                ("qvec".to_string(), json!(req.qvec)),
                ("level".to_string(), json!(max_level)),
            ]),
            &["community", "score"],
            req.k as usize,
        )
        .await?;

        if (top.len() as i64) < (req.k / 2) && max_level > 0 {
            let remaining = (req.k - top.len() as i64).max(0) as usize;
            if remaining > 0 {
                let mut extra = cypher::query_rows(
                    &state,
                    "requirements_graph/retrieval/vector_query_communities_by_level",
                    HashMap::from([
                        ("projectId".to_string(), json!(req.project_id)),
                        ("name".to_string(), json!(req.community_index_name)),
                        ("k".to_string(), json!(remaining as i64)),
                        ("qvec".to_string(), json!(req.qvec)),
                        ("level".to_string(), json!(max_level - 1)),
                    ]),
                    &["community", "score"],
                    remaining,
                )
                .await?;
                top.append(&mut extra);
            }
        }

        for r in top {
            if let Some(cid) = r.get("community").and_then(|v| v.as_i64()) {
                communities.push(cid);
            }
        }
    } else {
        let rows = cypher::query_rows(
            &state,
            "requirements_graph/retrieval/vector_query_nodes",
            HashMap::from([
                ("projectId".to_string(), json!(req.project_id)),
                ("name".to_string(), json!(req.community_index_name)),
                ("k".to_string(), json!(req.k)),
                ("qvec".to_string(), json!(req.qvec)),
            ]),
            &["community", "score"],
            req.k as usize,
        )
        .await?;
        for r in rows {
            if let Some(cid) = r.get("community").and_then(|v| v.as_i64()) {
                communities.push(cid);
            }
        }
    }

    // Deduplicate while preserving order.
    let mut seen = std::collections::HashSet::<i64>::new();
    communities.retain(|c| seen.insert(*c));

    // 3) brief summaries (same as prior "brief" usage)
    let summaries_rows = if communities.is_empty() {
        Vec::new()
    } else {
        cypher::query_rows(
            &state,
            "requirements_graph/retrieval/fetch_community_summaries",
            HashMap::from([
                ("projectId".to_string(), json!(req.project_id)),
                ("ids".to_string(), json!(communities)),
            ]),
            &["id", "summary"],
            10_000,
        )
        .await?
    };

    // Keep output stable for Python: list of {id, summary}
    Ok((
        StatusCode::OK,
        Json(PrimerContextResponse {
            communities,
            community_brief: summaries_rows,
        }),
    ))
}

pub async fn followup_context(
    State(state): State<AppState>,
    Json(req): Json<FollowupContextRequest>,
) -> Result<impl IntoResponse, AppError> {
    if req.k <= 0 {
        return Err(AppError::bad_request("k must be > 0".to_string()));
    }
    if req.chunk_index_name.trim().is_empty() {
        return Err(AppError::bad_request(
            "chunk_index_name must be a non-empty string".to_string(),
        ));
    }
    if req.community_ids.is_empty() {
        return Ok((
            StatusCode::OK,
            Json(FollowupContextResponse {
                chunk_ids: Vec::new(),
                neighborhoods: Vec::new(),
            }),
        ));
    }

    // 1) vector query for chunk ids
    let chunk_rows = cypher::query_rows(
        &state,
        "requirements_graph/retrieval/optimized_scoped_chunks",
        HashMap::from([
            ("projectId".to_string(), json!(req.project_id)),
            ("cids".to_string(), json!(req.community_ids)),
            ("chunkIndex".to_string(), json!(req.chunk_index_name)),
            ("qvec".to_string(), json!(req.qvec)),
            ("limit".to_string(), json!(req.k)),
        ]),
        &["chunk_id", "score"],
        req.k as usize,
    )
    .await?;

    let mut chunk_ids: Vec<String> = chunk_rows
        .iter()
        .filter_map(|r| {
            r.get("chunk_id")
                .and_then(|v| v.as_str())
                .map(|s| s.to_string())
        })
        .collect();

    if chunk_ids.is_empty() {
        return Ok((
            StatusCode::OK,
            Json(FollowupContextResponse {
                chunk_ids: Vec::new(),
                neighborhoods: Vec::new(),
            }),
        ));
    }

    // 2) expand neighborhoods for those chunk ids
    let max_len = if req.max_chunk_len <= 0 {
        default_max_chunk_len()
    } else {
        req.max_chunk_len
    };
    // Safety cap
    chunk_ids.truncate(50);

    let mut neighborhoods = cypher::query_rows(
        &state,
        "requirements_graph/retrieval/expand_neighborhood_minimal",
        HashMap::from([
            ("projectId".to_string(), json!(req.project_id)),
            ("chunkIds".to_string(), json!(chunk_ids)),
            ("maxChunkLen".to_string(), json!(max_len)),
        ]),
        &[
            "chunk_id",
            "text",
            "document_name",
            "neighbours",
            "related_entities",
            "relationships",
            "neighbor_chunk_ids",
        ],
        50,
    )
    .await?;

    // Order neighborhoods to match chunk_ids.
    let order: HashMap<String, usize> = chunk_ids
        .iter()
        .enumerate()
        .map(|(i, s)| (s.clone(), i))
        .collect();
    neighborhoods.sort_by_key(|m| {
        m.get("chunk_id")
            .and_then(|v| v.as_str())
            .and_then(|id| order.get(id).copied())
            .unwrap_or(usize::MAX)
    });

    Ok((
        StatusCode::OK,
        Json(FollowupContextResponse {
            chunk_ids,
            neighborhoods,
        }),
    ))
}
