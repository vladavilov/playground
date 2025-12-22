use std::{collections::HashMap, sync::Arc};

use axum::Router;
use serde_json::Value;

use neo4j_repository_service::{
    executor::{DynExecutor, Neo4jExecutor},
    http,
    neo4j,
    queries::QueryRegistry,
    AppState,
};

pub type Row = HashMap<String, Value>;
pub type Rows = Vec<Row>;
pub type RowsByCypher = HashMap<String, Rows>;

#[allow(dead_code)]
#[derive(Clone, Debug)]
pub enum Call {
    Read {
        cypher: String,
        params: HashMap<String, Value>,
        fields: Vec<String>,
        max_rows: usize,
    },
    Write {
        cypher: String,
        params: HashMap<String, Value>,
    },
    Healthcheck,
}

#[derive(Clone, Default)]
pub struct MockExecutor {
    pub rows_by_cypher: Arc<std::sync::Mutex<RowsByCypher>>,
    pub calls: Arc<std::sync::Mutex<Vec<Call>>>,
}

impl MockExecutor {
    pub fn with_rows(rows_by_cypher: RowsByCypher) -> Self {
        Self {
            rows_by_cypher: Arc::new(std::sync::Mutex::new(rows_by_cypher)),
            calls: Arc::new(std::sync::Mutex::new(Vec::new())),
        }
    }
}

#[async_trait::async_trait]
impl Neo4jExecutor for MockExecutor {
    async fn healthcheck(&self) -> Result<(), neo4j::Neo4jError> {
        self.calls.lock().unwrap().push(Call::Healthcheck);
        Ok(())
    }

    async fn execute_write(
        &self,
        cypher: &str,
        params: HashMap<String, Value>,
    ) -> Result<neo4j::ExecuteSummary, neo4j::Neo4jError> {
        self.calls.lock().unwrap().push(Call::Write {
            cypher: cypher.to_string(),
            params,
        });
        Ok(neo4j::ExecuteSummary {
            counters: neo4j::Counters {
                nodes_created: 0,
                nodes_deleted: 0,
                relationships_created: 0,
                relationships_deleted: 0,
                properties_set: 0,
            },
        })
    }

    async fn execute_read_rows(
        &self,
        cypher: &str,
        params: HashMap<String, Value>,
        fields: Vec<String>,
        max_rows: usize,
    ) -> Result<Vec<HashMap<String, Value>>, neo4j::Neo4jError> {
        self.calls.lock().unwrap().push(Call::Read {
            cypher: cypher.to_string(),
            params,
            fields,
            max_rows,
        });

        let map = self.rows_by_cypher.lock().unwrap();
        Ok(map
            .get(cypher)
            .or_else(|| map.get(cypher.trim()))
            .cloned()
            .unwrap_or_default())
    }
}

pub fn state_with_queries(
    queries: QueryRegistry,
    rows_by_key: Vec<(&str, Rows)>,
) -> (AppState, MockExecutor) {
    let mut by_cypher: RowsByCypher = HashMap::new();
    for (key, rows) in rows_by_key {
        let cypher = queries.get(key).unwrap().to_string();
        by_cypher.insert(cypher, rows);
    }

    let mock = MockExecutor::with_rows(by_cypher);
    let state = AppState {
        cfg: neo4j_repository_service::config::Config {
            bind_addr: "127.0.0.1:0".parse().unwrap(),
            queries_dir: "unused".to_string(),
            neo4j_uri: "".to_string(),
            neo4j_username: "".to_string(),
            neo4j_password: "".to_string(),
            neo4j_database: "".to_string(),
            neo4j_max_connections: 1,
            neo4j_connect_timeout: std::time::Duration::from_secs(1),
        },
        executor: Arc::new(mock.clone()) as DynExecutor,
        queries,
    };

    (state, mock)
}

pub fn app(state: AppState) -> Router {
    http::router().with_state(state)
}

