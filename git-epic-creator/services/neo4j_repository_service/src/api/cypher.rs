use std::collections::HashMap;

use serde_json::Value;

use crate::{AppState, http::AppError};

pub async fn query_rows(
    state: &AppState,
    key: &str,
    params: HashMap<String, Value>,
    fields: &[&str],
    max_rows: usize,
) -> Result<Vec<HashMap<String, Value>>, AppError> {
    let cypher = state.queries.get(key).map_err(AppError::query)?;
    let fields = fields.iter().map(|s| (*s).to_string()).collect::<Vec<_>>();

    state
        .executor
        .execute_read_rows(cypher, params, fields, max_rows)
        .await
        .map_err(AppError::neo4j)
}

pub async fn first_row(
    state: &AppState,
    key: &str,
    params: HashMap<String, Value>,
    fields: &[&str],
) -> Result<HashMap<String, Value>, AppError> {
    let mut rows = query_rows(state, key, params, fields, 1).await?;
    Ok(rows.pop().unwrap_or_default())
}

pub async fn count(
    state: &AppState,
    key: &str,
    params: HashMap<String, Value>,
    field: &str,
) -> Result<i64, AppError> {
    let rows = query_rows(state, key, params, &[field], 1).await?;
    let v = rows
        .first()
        .and_then(|m| m.get(field))
        .and_then(|v| v.as_i64())
        .unwrap_or(0);
    Ok(v)
}

pub async fn exec_write_no_result(
    state: &AppState,
    key: &str,
    params: HashMap<String, Value>,
) -> Result<(), AppError> {
    let cypher = state.queries.get(key).map_err(AppError::query)?;
    state
        .executor
        .execute_write(cypher, params)
        .await
        .map_err(AppError::neo4j)?;
    Ok(())
}
