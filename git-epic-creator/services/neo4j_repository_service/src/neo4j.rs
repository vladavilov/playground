use std::collections::HashMap;

use neo4rs::{
    BoltBoolean, BoltFloat, BoltInteger, BoltList, BoltMap, BoltNull, BoltString, BoltType,
    ConfigBuilder, Graph, Id, Keys, Labels, Node, query,
};
use serde_json::Value;
use thiserror::Error;

use crate::config::Config;

#[derive(Debug, Error)]
pub enum Neo4jError {
    #[error("neo4j error: {0}")]
    Neo4rs(#[from] neo4rs::Error),
    #[error("neo4j connect timeout after {0:?}")]
    ConnectTimeout(std::time::Duration),
    #[error("invalid params: {0}")]
    InvalidParams(String),
}

#[derive(Clone, Debug, serde::Serialize)]
pub struct Counters {
    pub nodes_created: i64,
    pub nodes_deleted: i64,
    pub relationships_created: i64,
    pub relationships_deleted: i64,
    pub properties_set: i64,
}

#[derive(Clone, Debug, serde::Serialize)]
pub struct ExecuteSummary {
    pub counters: Counters,
}

pub async fn connect(cfg: &Config) -> Result<Graph, Neo4jError> {
    let config = ConfigBuilder::default()
        .uri(cfg.neo4j_uri.clone())
        .user(cfg.neo4j_username.clone())
        .password(cfg.neo4j_password.clone())
        .db(cfg.neo4j_database.clone())
        .max_connections(cfg.neo4j_max_connections)
        .build()?;

    let graph = match tokio::time::timeout(cfg.neo4j_connect_timeout, Graph::connect(config)).await
    {
        Ok(r) => r?,
        Err(_) => return Err(Neo4jError::ConnectTimeout(cfg.neo4j_connect_timeout)),
    };
    Ok(graph)
}

pub async fn healthcheck(graph: &Graph) -> Result<(), Neo4jError> {
    let mut result = graph.execute(query("RETURN 1 as n")).await?;
    // Consume quickly; we only care that query succeeds.
    while let Some(_row) = result.next().await? {}
    Ok(())
}

pub async fn execute_write(
    graph: &Graph,
    cypher: &str,
    params: HashMap<String, Value>,
) -> Result<ExecuteSummary, Neo4jError> {
    let mut q = query(cypher);
    for (k, v) in params {
        q = q.param(k.as_str(), json_to_bolt(&v));
    }

    let mut result = graph.execute(q).await?;
    while let Some(_row) = result.next().await? {}
    // neo4rs 0.8 does not expose query summary counters from `Graph::execute`.
    // We return a stable shape for callers while keeping the operation best-effort.
    Ok(ExecuteSummary {
        counters: Counters {
            nodes_created: 0,
            nodes_deleted: 0,
            relationships_created: 0,
            relationships_deleted: 0,
            properties_set: 0,
        },
    })
}

pub async fn execute_read_rows(
    graph: &Graph,
    cypher: &str,
    params: HashMap<String, Value>,
    fields: &[String],
    max_rows: usize,
) -> Result<Vec<HashMap<String, Value>>, Neo4jError> {
    let mut q = query(cypher);
    for (k, v) in params {
        q = q.param(k.as_str(), json_to_bolt(&v));
    }

    let mut result = graph.execute(q).await?;
    let mut rows_out: Vec<HashMap<String, Value>> = Vec::new();

    while let Some(row) = result.next().await? {
        let mut out: HashMap<String, Value> = HashMap::with_capacity(fields.len());
        for field in fields {
            // Try decode as JSON-friendly scalar/list/map first.
            if let Ok(v) = row.get::<Value>(field) {
                out.insert(field.clone(), v);
                continue;
            }
            // Fallback: Neo4j node
            if let Ok(n) = row.get::<Node>(field) {
                out.insert(field.clone(), node_to_json(&n)?);
                continue;
            }
            // If the field doesn't exist or is an unsupported type, return null for stability.
            out.insert(field.clone(), Value::Null);
        }
        rows_out.push(out);
        if rows_out.len() >= max_rows {
            break;
        }
    }

    // Connection is released back to pool when the stream is dropped.
    Ok(rows_out)
}

fn json_to_bolt(value: &Value) -> BoltType {
    match value {
        Value::Null => BoltType::Null(BoltNull),
        Value::Bool(b) => BoltType::Boolean(BoltBoolean::new(*b)),
        Value::Number(n) => {
            if let Some(i) = n.as_i64() {
                BoltType::Integer(BoltInteger::new(i))
            } else if let Some(f) = n.as_f64() {
                BoltType::Float(BoltFloat::new(f))
            } else {
                // JSON number can always be represented as f64; fallback.
                BoltType::Float(BoltFloat::new(n.as_f64().unwrap_or(0.0)))
            }
        }
        Value::String(s) => BoltType::String(BoltString::from(s.clone())),
        Value::Array(arr) => {
            let list = arr.iter().map(json_to_bolt).collect::<Vec<_>>();
            BoltType::List(BoltList::from(list))
        }
        Value::Object(obj) => {
            let mut map_items: Vec<(BoltString, BoltType)> = Vec::with_capacity(obj.len());
            for (k, v) in obj {
                map_items.push((BoltString::from(k.clone()), json_to_bolt(v)));
            }
            let map: BoltMap = map_items.into_iter().collect();
            BoltType::Map(map)
        }
    }
}

#[derive(serde::Deserialize)]
struct NodeEnvelope {
    id: Id,
    labels: Labels,
    keys: Keys<Vec<String>>,
}

fn node_to_json(node: &Node) -> Result<Value, Neo4jError> {
    let env: NodeEnvelope = node
        .to()
        .map_err(|e| Neo4jError::InvalidParams(e.to_string()))?;
    let mut props = serde_json::Map::new();
    for k in env.keys.0 {
        let v = node.get::<Value>(&k).unwrap_or(Value::Null);
        props.insert(k, v);
    }
    Ok(serde_json::json!({
        "id": env.id.0,
        "labels": env.labels.0,
        "properties": props
    }))
}
